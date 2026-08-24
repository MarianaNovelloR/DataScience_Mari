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
#install.packages("stringi")


# Loading Packages --------------------------------------------------------

library(tidyverse)         # Data manipulation and visualization
library(stringdist)        # Match similar names
library(rfishbase)         # FishBase data access - taxonomy tools
library(sf)                # Spatial data handling
library(CoordinateCleaner) # Coordinate cleaning
library(rnaturalearth)     # World Map Data from Natural Earth
library(stringi)           # Text standardization


# Load utility functions --------------------------------------------------
source("01.Script/Utils.R") 


# Import Database ----------------------------------------------------------

popul <- read.csv("02.Raw_Data/Populations_merged.csv")         #Population Data
trophic_int <- read.csv("02.Raw_Data/Interactions_merged.csv")  #Interactions Data


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
# The clean_text() function (stored in utils.R) was applied to standardize
# text variables (lowercase, white space, and accents).

# Population Data
pop_textclean <- clean_text(
  pop_reduced,
  cols_to_lower = c("Type","Habitat"))

# Interaction Data
int_textclean <- clean_text(
  int_reduced,
  cols_to_lower = c("Quantified_at_the",
                    "Sampled_at_the",
                    "Method",
                    "Source",
                    "Origin",
                    "Category"))

# Note: Species and Locality were not converted to lowercase
# at this stage to avoid interfering with subsequent taxonomic and
# coordinate validation.


# Checking categories for typos -------------------------------------------
# lapply(): applies a function across all columns
# unique(): lists all the distinct categories

# Population
lapply(pop_textclean, unique)

# Interactions
lapply(int_textclean, unique)

# Note: The data contains typos, values assigned to the wrong columns, and
# incorrect dates that are in the future.


# Fixing swapped classifications ------------------------------------------
# filter(): select a subset of rows from a data frame
# mutate(): in this case creates a temporary column to store original Type values
# ifelse(): in this case swaps incorrectly assigned values between Type and Habitat

# Population Data
# Check which rows the categories were swapped
freshwater_rows <- filter(pop_textclean, Habitat == "freshwater") #Habitat and Type were swapped

# Correcting swapped classifications between Type and Habitat
pop_typo <- pop_textclean %>%
  mutate(temp = Type,
         Type = ifelse(Habitat == "freshwater" 
                       & Type %in% c("river", "reservoir", "lagoon"),
                       Habitat, Type),
         Habitat = ifelse(Habitat == "freshwater" 
                          & temp %in% c("river", "reservoir", "lagoon"), 
                          temp, Habitat)) %>% 
  select(-temp) # Remove the temporary column after the correction


# Typo Correction (automatic) ---------------------------------------------
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
int_invalid_values <- int_textclean %>%
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
# The find_typo_suggestions() function (stored in Utils.R) was used to identify possible corrections based on string similarity

# Population Data
pop_typo_suggestions <- find_typo_suggestions(
  pop_invalid_values,
  pop_categories
)

pop_typo_suggestions

# Interactions Data
typo_suggestions_int <- find_typo_suggestions(
  int_invalid_values,
  int_categories
)

typo_suggestions_int

# Automatically correct high-similarity matches
# The apply_typo_corrections() function (stored in Utils.R) was used to automatically correct high-confidence matches

# Population Data
pop_typo <- apply_typo_corrections(
  pop_typo,
  pop_typo_suggestions,
  threshold = 0.80
)

# Interactions Data
int_typo <- apply_typo_corrections(
  int_textclean,
  typo_suggestions_int,
  threshold = 0.80
)

# Note: Check the categories before choosing the similarity threshold.
# I chose the 0.80 threshold after reviewing the suggested matches.
# In my case, values below 0.80 often resulted in incorrect corrections,
# so I decided to keep them for manual review.

# Filter remaining invalid values for manual review
# The create_manual_review() function (stored in Utils.R) was used to identify values that still require manual review

# Population Data
pop_manual_review <- create_manual_review(
  pop_typo,
  pop_categories
)

pop_manual_review # check remaining typos

# Interactions Data
int_manual_review <- create_manual_review(
  int_typo,
  int_categories
)

int_manual_review # check remaining typos


# Species Typo Correction (automatic) -------------------------------------

# FishBase species names were used as a reference "dictionary" to identify
# possible typos mistakes before taxonomic validation.

# Get FishBase species names
taxa <- species_names()
fish_species_names <- unique(taxa$Species)

# Find possible species name corrections
# The find_species_typos() function (stored in Utils.R) was used to identify possible corrections based on FishBase species names

# Population Data
pop_species_typo_suggestions <- find_species_typos(
  pop_typo,
  fish_species_names
)

pop_species_typo_suggestions

# Interactions Data
int_species_typo_suggestions <- find_species_typos(
  int_typo,
  fish_species_names
)

int_species_typo_suggestions


# Automatically correct high-confidence matches
# The apply_species_corrections() function (stored in Utils.R) was used to automatically correct high-confidence matches

# Population Data
pop_species_typo <- apply_species_corrections(
  pop_typo,
  pop_species_typo_suggestions,
  threshold = 0.95
)

# Interactions Data
int_species_typo <- apply_species_corrections(
  int_typo,
  int_species_typo_suggestions,
  threshold = 0.95
)


# Species that still require manual review
# Population Data
pop_species_manual_review <- pop_species_typo_suggestions %>%
  filter(Similarity < 0.95)

pop_species_manual_review # check remaining typos

# Interactions Data
int_species_manual_review <- int_species_typo_suggestions %>%
  filter(Similarity < 0.95)

int_species_manual_review # check remaining typos


# Note: It is important to check the suggested matches before choosing
# the similarity threshold. The threshold value is passed to the
# apply_species_corrections() function.

# I chose 0.95 after checking the suggested matches (for both datasets).
# There were several species names with similarities between 0.90 and
# 0.94 that were matched incorrectly. Some correct matches also had
# similarities below 0.95, but I preferred to review these manually
# rather than risk incorrect corrections.


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
# the original names whenever no FishBase match was found.

# The validate_species_taxonomy() function (stored in Utils.R) was used to perform species name validation.

# Population Data
pop_tax <- validate_species_taxonomy(pop_coord)

# Interactions Data
int_tax <- validate_species_taxonomy(int_typo_final)


# Taxonomy Enrichment -----------------------------------------------------

# Extract taxonomic classification from FishBase
taxa <- load_taxa()

# The add_taxonomy() function (stored in Utils.R) was used to add
# taxonomic information from FishBase and fill missing classifications
# using genus-level information when species-level matches were unavailable.

# Population Data
pop_tax <- add_taxonomy(pop_tax, taxa)

# Interactions Data
int_tax <- add_taxonomy(int_tax, taxa)


# Saving Cleaned Datasets -------------------------------------------------

# Creating new Objects just to organize the final Datasets
interactions_final <- int_tax
population_final <- pop_tax

# Saving Population .csv
write.csv(population_final,"03.Output/population_final.csv", row.names = FALSE)

# Saving Interactions .csv
write.csv(interactions_final,"03.Output/interactions_final.csv", row.names = FALSE)


