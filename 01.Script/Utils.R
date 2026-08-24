# Course: Data Science in R
# Project: Utility Functions
# Author: Mariana Novello

# Overview:
# This script contains reusable functions that are applied in multiple stages
# of the data cleaning workflow. It reduces code duplication and improves script
# organization and reproducibility.

# Text Standardization ----------------------------------------------------
# across(): Apply the same transformation to multiple columns
# where(): Select columns from a data frame based on a data type

# Create a function to clean text data in a dataframe
# df: input dataframe
clean_text <- function(df, cols_to_lower = NULL) {
  
# Convert selected columns to lowercase
# Note: cols_to_lower = NULL means that no columns will be converted to lowercase
# unless they are explicitly provided when calling the function. I included this
# option because I chose to standardize only specific variables.
  if (!is.null(cols_to_lower)) {
    
    df <- df %>%
      mutate(
        across(all_of(cols_to_lower), str_to_lower)
      )
  }
  
# Remove accents
# stri_trans_general(): General Text Transforms, Including Transliteration
# Latin-ASCII: Converts characters from the Latin alphabet into ASCII
# equivalents (unaccented letters, numbers, and symbols)
df <- df %>%
    mutate(
      across(
        where(is.character),
        ~ stri_trans_general(., "Latin-ASCII")
      )
    )
  
# Remove extra white space
# str_squish(): Removes white space at the start and end and replaces
# internal whitespace with a single space
df <- df %>%
    mutate(
      across(where(is.character), str_squish)
    )
  
  return(df)
}


# Typo Suggestions --------------------------------------------------------
# Create a function to find possible corrections using string similarity
# stringsim(): Calculates string similarity
# which.max(): Identifies the category with the highest similarity
# Used Jaro-Winkler ("jw") similarity because it performs well for detecting typos in short categorical values

# Create a table containing suggested typo corrections
find_typo_suggestions <- function(invalid_values, categories) {
  
  typo_suggestions <- data.frame()
  
  for (col in names(categories)) {
    
    invalid <- invalid_values[[col]]
    invalid <- unique(invalid[!is.na(invalid)])
    
    for (value in invalid) {
      
      if (!(value %in% categories[[col]])) {
        
        similarity <- stringsim(
          value,
          categories[[col]],
          method = "jw"
        )
        
        best_index <- which.max(similarity)
        
        typo_suggestions <- rbind(
          typo_suggestions,
          data.frame(
            Variable = col,
            Invalid_value = value,
            Possible_match = categories[[col]][best_index],
            Similarity = round(similarity[best_index], 3)
          )
        )
      }
    }
  }
  
  return(typo_suggestions)
}


# Automatic Typo Correction ----------------------------------------------

# Create a function to automatically correct high-similarity matches
apply_typo_corrections <- function(
    df,
    typo_suggestions,
    threshold = 0.80
) {
  
  corrected_df <- df
  
  for (i in seq_len(nrow(typo_suggestions))) {
    
    if (typo_suggestions$Similarity[i] >= threshold) {
      
      col <- typo_suggestions$Variable[i]
      
      old_value <- typo_suggestions$Invalid_value[i]
      
      new_value <- typo_suggestions$Possible_match[i]
      
      corrected_df[[col]][
        corrected_df[[col]] == old_value
      ] <- new_value
    }
  }
  
  return(corrected_df)
}


# Manual Review Table -----------------------------------------------------
# Create a table containing values that still require manual review

create_manual_review <- function(df, categories) {
  
  manual_review <- data.frame()
  
  for (col in names(categories)) {
    
    invalid <- unique(df[[col]])
    
    invalid <- invalid[!is.na(invalid)]
    
    invalid <- invalid[
      !invalid %in% categories[[col]]
    ]
    
    if (length(invalid) > 0) {
      
      manual_review <- rbind(
        manual_review,
        data.frame(
          Variable = col,
          Invalid_value = invalid
        )
      )
    }
  }
  
  return(manual_review)
}