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
# Latin-ASCII: Converts characters from the Latin alphabet into ASCII equivalents (unaccented letters, numbers, and symbols)
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