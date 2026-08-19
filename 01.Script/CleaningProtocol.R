# Course: Data Science in R
# Project: Data Cleaning Pipeline
# Author: Mariana Novello 


# Overview:
# This script implements a reproducible data cleaning protocol to improve data
# quality, consistency, and standardization in a global trophic web database.
# The workflow includes data validation, taxonomic standardization, and
# LLM-assisted prey-item classification.
# The pipeline also includes detailed comments explaining key functions and
# commands to facilitate understanding of the workflow and promote data
# reusability.


# Empty your environment --------------------------------------------------

rm(list = ls())


# Install Packages (if needed) --------------------------------------------

#install.packages("tidyverse")
#install.packages("stringdist")
#install.packages("rfishbase")
#install.packages("sf")
#install.packages("rnaturalearth")
#install.packages("CoordinateCleaner")
#install.packages("here")
#install.packages("stringi")

# Loading Packages --------------------------------------------------------

library(tidyverse)         # Data manipulation and visualization
library(stringdist)        # Match similar names
library(rfishbase)         # FishBase data access - taxonomy tools
library(sf)                # Spatial data handling
library(CoordinateCleaner) # Coordinate cleaning
library(rnaturalearth)     # World Map Data from Natural Earth
library(here)              # Reproducible file paths
library(stringi)           # Text standardization


# Import Database ----------------------------------------------------------
# here(): find your project’s files

popul <- read.csv(here("Populations_merged.csv"))         #Population Data
trophic_int <- read.csv(here("Interactions_merged.csv"))  #Interactions Data


# Data Structure and Summary ----------------------------------------------

# Population
str(popul)              # Check data structure, variable types, and observations
summary(popul)          # Generate summary statistics for each variable
head(popul)             # Preview the first rows of the data
colSums(is.na(popul))   # Check missing values   

# Interactions
str(trophic_int)
summary(trophic_int)
head(trophic_int)
colSums(is.na(trophic_int))

# Note: When checking the data, we can see that some columns are composed only
# of NA values, while other variables contain NA values that need to be replaced 
# with an explicit value. We can also see that some variables need to be changed 
# to the appropriate data type.

# Data Type Standardization  ----------------------------------------------
# Ensure appropriate variable classes

# Interactions
trophic_int$Strength <- as.numeric(trophic_int$Strength)
popul$YearStart <- as.numeric(popul$YearStart)
popul$YearEnd <- as.numeric(popul$YearEnd)

# Variable Selection  -----------------------------------------------------
# Select(): Keep or drop columns using variables names and types

# Check available variables
colnames(popul)
colnames(trophic_int)

# Remove columns from Population Data
pop_reduced <- select(popul, 
                      -Includes, 
                      -Sampling_statement, 
                      -Benchwork, 
                      -Data_analysis, 
                      -Habitat_size, 
                      -Population_description,
                      -MonthStart,
                      -MonthEnd,
                      -Obs)  

# Remove columns from Interaction Data
int_reduced <- select(trophic_int, 
                      -Input_by,
                      -Individual,
                      -Obs)


# Replacing missing values ------------------------------------------------
# mutate(): Create, modify, and delete columns
# replace_na (): replaces missing values (NA) with a specified value

# Interactions Data
# Replace NA values in Source with "unknown"
int_reduced <- mutate(int_reduced, 
                   Source = replace_na(Source, "unknown"))


# Text Standardization ----------------------------------------------------
# across(): Apply the same transformation to multiple columns
# str_to_lower(): converts to lower case

# Converting selected text variables to lowercase 
# Population Data
pop_textclean_low <- mutate(pop_reduced,
                            across(c(Type, Habitat), str_to_lower))


# Interaction Data
int_textclean_low <- mutate(int_reduced,
                            across(c(Quantified_at_the,
                                     Sampled_at_the,
                                     Method, Source,
                                     Origin, Category), str_to_lower))

# Note: Species names and locality names were not standardized at this stage to 
# avoid interfering with subsequent taxonomic and coordinate validation

# Removing accents from all text variables
# where(): select columns from a data frame based on a data type
# stri_trans_general(): General Text Transforms, Including Transliteration
# Latin-ASCII: Converts characters from the Latin alphabet into ASCII equivalents
# ASCII is a character encoding system that includes unaccented letters, numbers, and symbols

# Population Data
pop_textclean_acc <- mutate(pop_textclean_low,
                            across(where(is.character),
                                   ~ stri_trans_general(., "Latin-ASCII")))


# Interaction Data
int_textclean_acc <- mutate(int_textclean_low,
                            across(where(is.character), 
                                   ~ stri_trans_general(., "Latin-ASCII")))


# Removing extra white space
# str_squish(): removes white space at the start and end, and replaces all 
# internal whitespace with a single space

# Population Data
pop_textclean_whi <- mutate(pop_textclean_acc,
  across(where(is.character), str_squish))

# Interactions Data
int_textclean_whi <- mutate(int_textclean_acc,
  across(where(is.character), str_squish))


# Checking categories for typos -------------------------------------------
# lapply(): applies a function across all columns
# unique(): lists all the distinct categories

# Population
lapply(pop_textclean_whi, unique)

# Interactions
lapply(int_textclean_whi, unique)

# Note: The data contains typos, values assigned to the wrong columns, and
# incorrect dates that are in the future.

# Fixing swapped classifications ------------------------------------------
# filter(): select a subset of rows from a data frame
# mutate(): in this case creates a temporary column to store original Type values
# ifelse(): in this case swaps incorrectly assigned values between Type and Habitat

# Population Data
# Check which rows the categories were swapped
freshwater_rows <- filter(pop_textclean_whi, Habitat == "freshwater") #Habitat and Type were swapped

# Correcting swapped classifications between Type and Habitat
pop_typo <- pop_textclean_whi %>%
  mutate(temp = Type,
         Type = ifelse(Habitat == "freshwater" 
                       & Type %in% c("river", "reservoir", "lagoon"),
                       Habitat, Type),
         Habitat = ifelse(Habitat == "freshwater" 
                          & temp %in% c("river", "reservoir", "lagoon"), 
                          temp, Habitat)) %>% 
  select(-temp) # Remove the temporary column after the correction


# Typo Correction (automatic version) -------------------------------------
# Applied to variables with predefined categories

# Creating the categories for each variable 
# list(): creates a list
# Population Data
pop_categories <- list(
  Type = c("freshwater", "marine", "estuarine"),
  Habitat = c("river", "lake", "reservoir", "coastal waters", "ocean", "lagoon", "estuary"))

# Interactions Data
int_categories <- list(
  Source = c("aquatic", "terrestrial", "unknown", "mixed"),
  Origin = c("vegetal", "animal", "unknown", "protist", "bacteria", "mixed"),
  Sampled_at_the = c("individual-level", "population-level", "community-level"),
  Quantified_at_the = c("individual-level", "population-level", "community-level"),
  Method = c("gut contents", "experiment", "stable isotopes", "observation", "interview", "genetics"))

# Filter invalid values
# filter(): in this case identifies rows containing invalid values based on the predefined categories
# distinct(): returns only unique combinations of the filtered values
# Population Data 
pop_invalid_values <- pop_typo %>%
  filter(
    !Type %in% pop_categories$Type |
      !Habitat %in% pop_categories$Habitat
  ) %>%
  select(Type, Habitat) %>%
  distinct()

# Interactions Data
int_invalid_values <- int_textclean_whi %>%
  filter(
    !Source %in% int_categories$Source |
      !Origin %in% int_categories$Origin |
      !Sampled_at_the %in% int_categories$Sampled_at_the |
      !Quantified_at_the %in% int_categories$Quantified_at_the |
      !Method %in% int_categories$Method
  ) %>%
  select(Source, Origin, Sampled_at_the, Quantified_at_the, Method) %>%
  distinct()


# Find possible corrections using string similarity
# stringsim(): calculates the similarity between the invalid value and valid categories
# which.max(): identifies the category with the highest similarity
# Used Jaro-Winkler ("jw") similarity because it performs well for detecting typos in short categorical values

# Population Data
pop_typo_suggestions <- data.frame()

for (col in names(pop_categories)) {
  
  invalid <- pop_invalid_values[[col]]
  invalid <- unique(invalid[!is.na(invalid)])
  
  for (value in invalid) {
    
    if (!(value %in% pop_categories[[col]])) {
      
      similarity <- stringsim(
        value,
        pop_categories[[col]],
        method = "jw"
      )
      
      best_index <- which.max(similarity)
      best_match <- pop_categories[[col]][best_index]
      best_similarity <- similarity[best_index]
      
      pop_typo_suggestions <- rbind(
        pop_typo_suggestions,
        data.frame(
          Variable = col,
          Invalid_value = value,
          Possible_match = best_match,
          Similarity = round(best_similarity, 3)))}}}

pop_typo_suggestions

# Interactions Data
typo_suggestions_int <- data.frame()

for (col in names(int_categories)) {
  
  invalid <- int_invalid_values[[col]]
  invalid <- unique(invalid[!is.na(invalid)])
  
  for (value in invalid) {
    
    if (!(value %in% int_categories[[col]])) {
      
      similarity <- stringsim(
        value,
        int_categories[[col]],
        method = "jw"
      )
      
      best_index <- which.max(similarity)
      best_match <- int_categories[[col]][best_index]
      best_similarity <- similarity[best_index]
      
      typo_suggestions_int <- rbind(
        typo_suggestions_int,
        data.frame(
          Variable = col,
          Invalid_value = value,
          Possible_match = best_match,
          Similarity = round(best_similarity, 3)
        )
      )
    }
  }
}

typo_suggestions_int


# Automatically correct high-similarity matches
# Note: Check the categories before choosing the similarity threshold.
# I chose the 0.80 threshold after reviewing the suggested matches.
# In my case, values below 0.80 often resulted in incorrect corrections,
# so I decided to keep them for manual review.

# Population Data
for (i in 1:nrow(pop_typo_suggestions)) {
  
  if (pop_typo_suggestions$Similarity[i] >= 0.80) {
    
    col <- pop_typo_suggestions$Variable[i]
    old_value <- pop_typo_suggestions$Invalid_value[i]
    new_value <- pop_typo_suggestions$Possible_match[i]
    
    pop_typo[[col]][pop_typo[[col]] == old_value] <- new_value
  }
}

# Interactions Data
int_typo <- int_textclean_whi #Created a new object just to see the progress

for (i in 1:nrow(typo_suggestions_int)) {
  
  if (typo_suggestions_int$Similarity[i] >= 0.80) {
    
    col <- typo_suggestions_int$Variable[i]
    old_value <- typo_suggestions_int$Invalid_value[i]
    new_value <- typo_suggestions_int$Possible_match[i]
    
    int_typo[[col]][int_typo[[col]] == old_value] <- new_value
  }
}

# Filter remaining invalid values for manual review
# Values that didn't match the predefined categories were kept for manual review

# Population Data
pop_manual_review <- data.frame()

for (col in names(pop_categories)) {
  
  invalid <- unique(pop_typo[[col]])
  invalid <- invalid[!is.na(invalid)]
  
  invalid <- invalid[!invalid %in% pop_categories[[col]]]
  
  if (length(invalid) > 0) {
    
    pop_manual_review <- rbind(
      pop_manual_review,
      data.frame(
        Variable = col,
        Invalid_value = invalid
      )
    )
  }
}

pop_manual_review # check remaining typos

#Interactions Data
int_manual_review <- data.frame()

for (col in names(int_categories)) {
  
  invalid <- unique(int_typo[[col]])
  invalid <- invalid[!is.na(invalid)]
  
  invalid <- invalid[!invalid %in% int_categories[[col]]]
  
  if (length(invalid) > 0) {
    
    int_manual_review <- rbind(
      int_manual_review,
      data.frame(
        Variable = col,
        Invalid_value = invalid
      )
    )
  }
}

int_manual_review # check remaining typos

# Note: Values that were not correctly matched by stringdist can be 
# manually corrected. I also tried to implement the yes/no approach you 
# suggested for accepting or rejecting each match.However, I could not make it 
# work as effectively as I wanted. So, I opted to validate the suggestions based 
# on the  similarity. If you think the yes/no approach would be more practical, 
# I can try implementing it again.


# Typo Correction - Species (automatic version) ---------------------------

# Note: The same stringdist approach was used for species names, but this
# step was separated to make the workflow clearer.

# FishBase species names were used as a reference "dictionary" to identify
# possible typos mistakes before taxonomic validation.

# Get FishBase species names
taxa <- species_names()
fish_species_names <- unique(taxa$Species)

# Population Data
# Find species names that are not exact matches to FishBase
pop_invalid_species <- pop_typo %>%
  filter(!Species %in% fish_species_names) %>%
  select(Species) %>%
  distinct()

# Create an empty table to store suggestions
pop_species_typo_suggestions <- data.frame()

# Find the closest FishBase match for each invalid name
for (species_name in pop_invalid_species$Species) {
  
  similarity <- stringsim(
    species_name,
    fish_species_names,
    method = "jw"
  )
  
  best_index <- which.max(similarity)
  
  best_match <- fish_species_names[best_index]
  
  best_similarity <- similarity[best_index]
  
  pop_species_typo_suggestions <- rbind(
    pop_species_typo_suggestions,
    data.frame(
      Original_name = species_name,
      Suggested_name = best_match,
      Similarity = round(best_similarity, 3)
    )
  )
}

# Check suggested corrections
pop_species_typo_suggestions


# Automatically correct high-confidence matches
pop_species_typo <- pop_typo

for (i in seq_len(nrow(pop_species_typo_suggestions))) {
  
  if (pop_species_typo_suggestions$Similarity[i] >= 0.95) {
    
    old_name <- pop_species_typo_suggestions$Original_name[i]
    
    new_name <- pop_species_typo_suggestions$Suggested_name[i]
    
    pop_species_typo$Species[
      pop_species_typo$Species == old_name
    ] <- new_name
  }
}

# Species that still require manual review
pop_species_manual_review <- pop_species_typo_suggestions %>%
  filter(Similarity < 0.95)

pop_species_manual_review # check remaining typos

# Interactions Data 
# Find species names that are not exact matches to FishBase
int_invalid_species <- int_typo %>%
  filter(!Species %in% fish_species_names) %>%
  select(Species) %>%
  distinct()

# Create an empty table to store suggestions
int_species_typo_suggestions <- data.frame()

# Find the closest FishBase match for each invalid name
for (species_name in int_invalid_species$Species) {
  
  similarity <- stringsim(
    species_name,
    fish_species_names,
    method = "jw"
  )
  
  best_index <- which.max(similarity)
  
  best_match <- fish_species_names[best_index]
  
  best_similarity <- similarity[best_index]
  
  int_species_typo_suggestions <- rbind(
    int_species_typo_suggestions,
    data.frame(
      Original_name = species_name,
      Suggested_name = best_match,
      Similarity = round(best_similarity, 3)
    )
  )
}

# Check suggested corrections
int_species_typo_suggestions

# Automatically correct high-confidence matches
int_species_typo <- int_typo

for (i in seq_len(nrow(int_species_typo_suggestions))) {
  
  if (int_species_typo_suggestions$Similarity[i] >= 0.95) {
    
    old_name <- int_species_typo_suggestions$Original_name[i]
    
    new_name <- int_species_typo_suggestions$Suggested_name[i]
    
    int_species_typo$Species[
      int_species_typo$Species == old_name
    ] <- new_name
  }
}

# Species that still require manual review
int_species_manual_review <- int_species_typo_suggestions %>%
  filter(Similarity < 0.95)

int_species_manual_review  # check remaining typos

# Note: Its important to check the suggested matches before choosing the similarity
# threshold. Species names are compared against thousands of FishBase
# records, so a more conservative threshold is recommended.
# I chose 0.95 after checking the suggested matches (for both datasets). 
# There were several species names with similarities between 0.90 and 0.94 that 
# were matched incorrectly. Some correct matches also had similarities below 0.95, but
# I preferred to review these manually rather than risk incorrect corrections.


# Typo Correction (manual version) ----------------------------------------
# Values not corrected during the automatic correction were reviewed manually

#Population Data
# Check categories and number of registers
unique(pop_species_typo$Species)  
unique(pop_species_typo$YearStart)
unique(pop_species_typo$YearEnd)

# Change names based on whats wrong
#recode(): Recode values
pop_typo_final <- pop_species_typo %>%
  mutate(Species = recode(Species,
                          "Astyanax aff. Bimaculatus" = "Astyanax aff. bimaculatus",
                          "Hemmigrammus sp." = "Hemigrammus sp.",
                          "Phenacogaster fransciscoensis" = "Phenacogaster franciscoensis",
                          "Hypostomus cf.macrops" = "Hypostomus cf. macrops"),
         YearStart = recode(YearStart,
                            `94` = 1994,
                            `2026` = 2021,
                            `2027` = 2021,
                             `2028` = 2021),
         YearEnd = recode(YearEnd,
                          `96` = 1996))

#Trophic Interactions Data
# Check categories and number of registers
unique(int_species_typo$Species)           
unique(int_species_typo$Source)               
unique(int_species_typo$Origin)                  
unique(int_species_typo$Index)           

# Change names based on whats wrong
int_typo_final <- int_species_typo %>%
  mutate(Species = recode(Species,
                     "Astyanax aff. Bimaculatus" = "Astyanax aff. bimaculatus",
                     "Hemmigrammus sp." = "Hemigrammus sp.",
                     "Phenacogaster fransciscoensis" = "Phenacogaster franciscoensis",
                     "Hypostomus cf.macrops" = "Hypostomus cf. macrops"), 
    Source = recode(Source,
                    "allochthonous" = "terrestrial",
                    "autochthonous" = "aquatic",
                    "river" = "aquatic"),
    Origin = recode(Origin,
                    "undetermined" = "unknown"),
    Index = recode(Index,
                   'FO (%)' = '%FO',
                   'FO%' = '%FO', 
                   'FO%' = '%FO',
                   'Fo%' = '%FO',
                   'AI%' = '%IAi', 
                   'Ali%' = '%IAi',
                   'IAI' = '%IAi',  
                   'IAI%' = '%IAi', 
                   'IAi' = '%IAi', 
                   'IAi%' = '%IAi',
                   'Iai' = '%IAi',
                   '%VO' = '%V',
                   'V%' = '%V', 
                   'VO%' = '%V',
                   'Vol%' = '%V', 
                   'Vol. (%)' = '%V',
                   'IIR%' = '%IRI',
                   'IRI%' = '%IRI',
                   'O%' = '%O',
                   'P%' = '%W',
                   'W%' = '%W',
                   '%P' = '%W',
                   'Peso seco (%)' = '%W', 
                   'IIR' = 'IRI', 
                   'N%' = '%N',
                   'weight g' = 'W',
                   'FN (%)' = '%FN',
                   'FN%' = '%FN',
                   'FV%' = '%FV',
                   '%F' = 'F%'))

# Note: the Index variable was also corrected manually because string similarity
# was not very effective for this variable. It contains many different (and short)
# responses and is less standardized than the other variables.
# The incorrect dates in YearStart and YearEnd were also corrected manually.
# As you suggested, I initially considered using the publication year of the
# corresponding paper to identify the correct dates, but, after reviewing the data, 
# I identified that the incorrect years resulted from a Google Sheets issue, 
# where the year can be changed when date columns are dragged.


# Coordinates Conversion --------------------------------------------------
# Convert coordinates from Degrees, Minutes and Seconds (DMS) to
# decimal degrees and create new coordinate columns.

# Population Data
pop_coord <- pop_typo_final %>%
  mutate( latitude = lat_d + lat_m/60 + lat_s/3600,
          longitude = lon_d + lon_m/60 + lon_s/3600,
          latitude = ifelse(lat_dir == "S", -latitude, latitude),
          longitude = ifelse(lon_dir == "W", -longitude, longitude))

# Remove the original DMS coordinate columns
pop_coord <- select( pop_coord, 
                     -lat_d, -lat_m, -lat_s, -lat_dir,
                     -lon_d, -lon_m, -lon_s, -lon_dir)

# Coordinate Validation ---------------------------------------------------

# Note: Records containing NAs were excluded because they interfere
# with the CoordinateCleaner validation step.

# Separate records with missing coordinates
pop_coord_complete <- pop_coord %>%
  filter(!is.na(latitude) & !is.na(longitude))

pop_coord_missing <- pop_coord %>%
  filter(is.na(latitude) | is.na(longitude))

# Check coordinates using CoordinateCleaner
# clean_coordinates(): flags or removes common geographical errors found in
# biological collection databases
coord_check <- clean_coordinates(
  x = pop_coord_complete,
  lon = "longitude",
  lat = "latitude",
  species = "Species"
)

view(coord_check) # Check column names

# Check records flagged as geographic outliers
coord_outliers <- coord_check %>%
  filter(.otl == FALSE)

View(coord_outliers)

# Note: It flagged 71 of 873 records, including 56 sea coordinates 
# and 15 geographic outliers. Since some records are from marine systems, 
# sea coordinates are not necessarily errors in our Dataset. Geographic outliers indicate 
# records that are spatially unusual compared to other records of the same 
# species in the dataset. I checked these records, and the coordinates do not 
# appear to be incorrect.

# Check if coordinates fall within Brazil

# Note: I could not find a way to perform this using CoordinateCleaner,
# so I used the sf package instead.

# Download Brazil polygon
# ne_countries(): Download country polygons from Natural Earth
brazil <- ne_countries(country = "Brazil", returnclass = "sf")

# Convert coordinates to spatial points
# st_as_sf(): Convert data frame to simple features object
# crs = 4326 is used because the coordinate reference system is EPSG:4326 (WGS 84)
pop_points <- st_as_sf(pop_coord_complete, 
                       coords = c("longitude", "latitude"), crs = 4326)

# Identify records in Brazil
# st_within(): Identify points within polygons
pop_points$in_brazil <- st_within(pop_points, brazil, sparse = FALSE)[, 1]

view(pop_points) # Check column names

# Check records located outside Brazil
# st_drop_geometry(): Drop geometry column from simple features object
pop_points %>%
  filter(!in_brazil) %>%
  st_drop_geometry() %>%
  select(Population, Species, in_brazil, Locality)

# Note: Some records were flagged as being outside Brazil because their
# coordinates did not fall within the Brazil polygon. After checking these
# records, the coordinates appeared to be correct and most were from coastal
# systems. The problem is that locations in the ocean are classified as
# outside the Brazil polygon, even when the coordinates are correct.


# Taxonomic Validation ----------------------------------------------------

# Note: FishBase validate_names() does not recognize species identified 
# only at the genus level or with taxonomic qualifiers (gr., aff., cf.).
# To avoid NAs in these cases, species names were validated while keeping 
# the original names in a separate column. When no match was found in FishBase, 
# the original name was retained. 

# Population Data
# Validate species names
species_validation <- tibble(
  Species = unique(pop_coord$Species),
  Species_valid = validate_names(unique(pop_coord$Species)))

# Keep the original name when FishBase does not find a match
species_validation <- species_validation %>%
  mutate(
    Species_valid = if_else(
      is.na(Species_valid),
      Species,
      Species_valid))

# Add validated names as a new column to the dataset
pop_tax <- pop_coord %>%
  left_join(species_validation, by = "Species")

# Remove the original Species column and rename the validated species column
pop_tax <- pop_tax %>%
  select(-Species) %>%
  rename(Species = Species_valid)

#Interaction Data
# Validate species names
species_validation <- tibble(
  Species = unique(int_typo_final$Species),
  Species_valid = validate_names(unique(int_typo_final$Species)))

# Keep the original name when FishBase does not find a match
species_validation <- mutate(species_validation, 
                             Species_valid = if_else
                             (is.na(Species_valid), 
                               Species, Species_valid))


# Add validated names as a new column to the dataset
int_tax <- int_typo_final %>%
    left_join(species_validation, by = "Species")
  
# Remove the original Species column and rename the validated species column
int_tax <- int_tax %>%
  select(-Species) %>%
  rename(Species = Species_valid)


# Taxonomy Enrichment -----------------------------------------------------

# Extract taxonomic classification from FishBase
taxa <- load_taxa()

#Population Data
# Extract taxonomic information for species
species_tax <- taxa %>%
  filter(Species %in% unique(pop_tax$Species)) %>%
  select(Species, Genus, Family, Order, Class)

# Add taxonomic information to the dataset
pop_tax <- pop_tax %>%
  left_join(species_tax, by = "Species")

# Solving NAs problem - Separate the Genus from species that FishBase does not recognize as valid
pop_tax <- pop_tax %>%
  mutate(Genus_search = word(Species, 1))

# Fill NAs with the Genus
pop_tax <- pop_tax %>%
  mutate(
    Genus = if_else(is.na(Genus), Genus_search, Genus)
  )

# Create a genus-level taxonomy table from FishBase
genus_tax <- taxa %>%
  select(Genus, Family, Order, Class) %>%
  distinct(Genus, .keep_all = TRUE)

# Add missing Family, Order and Class information based on Genus
pop_tax <- pop_tax %>%
  left_join(genus_tax, by = "Genus", suffix = c("", "_genus")) %>%
  mutate(
    Family = if_else(is.na(Family), Family_genus, Family),
    Order = if_else(is.na(Order), Order_genus, Order),
    Class = if_else(is.na(Class), Class_genus, Class)
  ) %>%
  select(-ends_with("_genus"))

#Remove Genus_Search column
pop_tax <- select(pop_tax, -Genus_search)

#Interactions Data
# Extract taxonomic information for species
species_tax_int <- taxa %>%
  filter(Species %in% unique(int_tax$Species)) %>%
  select(Species, Genus, Family, Order, Class)

# Add taxonomic information to the dataset
int_tax <- int_tax %>%
  left_join(species_tax_int, by = "Species")

# Solving NAs problem - Separate the Genus from species that FishBase does not recognize as valid
int_tax <- int_tax %>%
  mutate(Genus_search = word(Species, 1))

# Fill NAs with the Genus
int_tax <- int_tax %>%
  mutate(
    Genus = if_else(is.na(Genus), Genus_search, Genus)
  )

# Create a genus-level taxonomy table from FishBase
genus_tax <- taxa %>%
  select(Genus, Family, Order, Class) %>%
  distinct(Genus, .keep_all = TRUE)

# Add missing Family, Order and Class information based on Genus
int_tax <- int_tax %>%
  left_join(genus_tax, by = "Genus", suffix = c("", "_genus")) %>%
  mutate(
    Family = if_else(is.na(Family), Family_genus, Family),
    Order = if_else(is.na(Order), Order_genus, Order),
    Class = if_else(is.na(Class), Class_genus, Class)
  ) %>%
  select(-ends_with("_genus"))

#Remove Genus_Search column
int_tax <- select(int_tax, -Genus_search)

# Note: I think there is a faster way to do this, but I couldn't 
# find an alternative way that worked.


# Saving Cleaned Datasets -------------------------------------------------

# Creating new Objects just to organize the final Datasets and see if everything is corrected
interactions_final <- int_tax
population_final <- pop_tax

view(population_final)
view(interactions_final)

# Saving Population .csv
write.csv(
  population_final,
  here("population_final.csv"),
  row.names = FALSE
)

# Saving Interactions .csv
write.csv(
  interactions_final,
  here("interactions_final.csv"),
  row.names = FALSE
)


############################### END ##########################################




