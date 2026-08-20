# Course: Data Science in R
# Project: Large Language Model
# Author: Mariana Novello

# Overview: Develop a LLM-based classification and standardize heterogeneous 
# food-item descriptions into predefined categories


# Empty your environment --------------------------------------------------

rm(list = ls())


# Install Packages (if needed) --------------------------------------------

# install.packages("ellmer")
# install.packages("usethis")


# Loading Packages --------------------------------------------------------

library(ellmer)            # For interacting with AI
library(usethis)           # For managing environment variables


# Import Database ---------------------------------------------------------

int_data <- read.csv("03.Output/interactions_final.csv")  # Interactions Data Cleaned
manual_classific <- read.csv("02.Raw_Data/categories_manual.csv") # Data with manual classifications to compare with the model's output


# Setting Gemini API ------------------------------------------------------

# API key setup (only needs to be run once)
usethis::edit_r_environ()

# Note: In the .Renviron you write and run "GEMINI_API_KEY=paste_your_api_key_here".
# Then, you need to restart R for the changes to take effect.I did this way just to 
# not share my API key in the code.

# Check if the API key is set in your environment 
Sys.getenv("GEMINI_API_KEY")


# Initializing Gemini chat and specifying the model version ---------------
# chat_google_gemini(): To chat with Google Gemini in R
chat <- chat_google_gemini(
  model = "gemini-3.6-flash",
  system_prompt = " You are an expert in freshwater fish ecology, 
  aquatic food webs, taxonomy, and biological classification.

  Your task is to classify prey items reported in scientific literature
  into one of six predefined ecological categories: Fish, Invertebrate, 
  Plant, Algae, Detritus, or Other.Using the following definitions:

  - Fish: fish or fish-derived material.
  - Invertebrate: aquatic or terrestrial invertebrate animals.
  - Plant: non-algal plant material.
  - Algae: algae or algal material.
  - Detritus: organic detrital material.
  - Other: items that do not fit any of the categories above.

  Use your biological and taxonomic knowledge to determine the most
  appropriate category for each prey item. Prey items may be reported
  using common names, taxonomic names, higher taxonomic groups, or
  general descriptions. Classify each prey item into exactly one category.

  Base the classification on established biological and taxonomic
  knowledge. Do not invent information about the prey item.

  If the information provided is insufficient to confidently assign
  the item to one of the categories, classify it as Other.

  Return only the category name, with no explanation.")

# Note: In this step it defines the model's role, classification rules, and response format.
# This is like a "System Prompt", and the instructions remain active for all subsequent chat requests.


# Test Gemini connection --------------------------------------------------

chat$chat("classify Cichla ocellaris")
chat$chat("classify ephemeroptera")

# Note: I decided to run these tests just to see if the model was working,
# but if you're using the free version of the API I don't recommend
# spending your free entries on it.


# Applying it to our dataset ----------------------------------------------

# Extract unique prey items in the dataset
all_prey <- unique(int_data$Prey)


# Load previous classifications (if available) ------------------------------

# Note: This  allows the script to reuse classifications obtained in
# previous runs, avoiding unnecessary API calls and reducing processing time 
# (its called cache because I lost the data before).

cache_file <- "03.Output/prey_classification_cache.csv"

if (file.exists(cache_file)) {
  
  prev_classification <- read.csv(
    cache_file,
    stringsAsFactors = FALSE
  )
  
} else {
  
  prev_classification <- data.frame(
    Prey = character(),
    Category = character(),
    stringsAsFactors = FALSE
  )
  
}

# Select only prey items that have not been classified before

prey_unique <- setdiff(all_prey, prev_classification$Prey)

cat(
  "\nPreviously classified items:",
  nrow(prev_classification),
  "\nItems remaining to classify:",
  length(prey_unique),
  "\n"
)


# Model -------------------------------------------------------------------

# Split prey into batches

# Note: I did that because the model has a limit on the number of tokens
# it can process in a single request. So this was the way I found to avoid
# that limit and classify all the prey items in the dataset.

batch_size <- 50

batches <- split(
  prey_unique,
  ceiling(seq_along(prey_unique) / batch_size)
)


# Function to classify a batch 

classify_batch <- function(prey_batch) {
  
  prompt <- paste0(
    "Classify each prey item below into one of the categories:
Fish, Invertebrate, Plant, Algae, Detritus, or Other.

Return ONLY the category names.
Return ONE category per line.
Keep the SAME ORDER as the prey items provided.

Prey items:

",
    paste(prey_batch, collapse = "\n")
  )
  
  response <- chat$chat(prompt)
  
  return(response)
  
}

# Note: I am not pretty sure if it needs to receive a prompt like this again,
# but I maintained it just to make sure it was going to follow the rules.


# Store classifications 
prey_classification <- character()


# Classify batches (if there are any prey items to classify)

if (length(prey_unique) > 0) {
  
  for (i in seq_along(batches)) {
    
    cat(
      "\nProcessing batch",
      i,
      "of",
      length(batches),
      "\n"
    )
    
    success <- FALSE
    
    while (!success) {
      
      tryCatch({
        
        response <- classify_batch(
          batches[[i]]
        )
        
        categories <- unlist(
          strsplit(response, "\n")
        )
        
        categories <- trimws(categories)
        
        # Check that the model returned one category per prey item
        
        if (length(categories) != length(batches[[i]])) {
          stop(
            "Number of categories returned does not match number of prey items."
          )
        }
        
        # Check that all returned categories are valid
        
        valid_categories <- c(
          "Fish",
          "Invertebrate",
          "Plant",
          "Algae",
          "Detritus",
          "Other"
        )
        
        if (!all(categories %in% valid_categories)) {
          stop("Invalid category returned by the model.")
        }
        
        prey_classification <- c(
          prey_classification,
          categories
        )
        
        # Save partial results after every batch 
        
        partial_results <- data.frame(
          Prey = unlist(
            batches[1:i]
          )[1:length(prey_classification)],
          Category = prey_classification,
          stringsAsFactors = FALSE
        )
        
        updated_cache <- unique(
          rbind(
            prev_classification,
            partial_results
          )
        )
        
        write.csv(
          updated_cache,
          cache_file,
          row.names = FALSE
        )
        
        # Update the classifications stored in the current session
        
        prev_classification <- updated_cache
        
        success <- TRUE
        
        Sys.sleep(20)
        
      }, error = function(e) {
        
        cat(
          "\nError:",
          conditionMessage(e),
          "\nRetrying in 60 seconds...\n"
        )
        
        Sys.sleep(60)
        
      })
      
    }
    
  }
  
}


# Add the new categories to the original data 
int_data$LLM_Category <- prev_classification$Category[
  match(int_data$Prey,prev_classification$Prey)]


# Check results 
table(int_data$LLM_Category)


# Compare LLM and manual classifications ----------------------------------

# Keep only one classification record per prey item in the manual dataset
manual_unique <- unique(manual_classific[, c("Prey", "Categories_general")])

# Keep only one classification record per prey item in the LLM output
int_unique <- unique(int_data[, c("Prey","LLM_Category")])

# Match manual and LLM classifications using the prey item description
comparison <- merge(manual_unique,int_unique,by = "Prey")

# Calculate accuracy
accuracy <- mean(comparison$Categories_general == comparison$LLM_Category)

cat("\nOverall Accuracy:", round(accuracy * 100, 2), "%\n")

# Cases where the model disagrees with the manual classification
errors <- comparison[comparison$Categories_general !=comparison$LLM_Category,]
cat("\nNumber of disagreements:",nrow(errors),"\n")

View(errors)

# Note: After inspecting the disagreements between the manual and LLM
# classifications, most errors did not seem unreasonable. Many cases were
# inherently ambiguous and could plausibly fit more than one ecological
# category. In several instances the model appeared to generalize prey items
# more broadly than I would have done manually, which led to some
# disagreements. Overall, however, the classifications were more accurate
# and biologically consistent than I initially expected.


################################# END #######################################
