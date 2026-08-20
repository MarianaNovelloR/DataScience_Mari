# Biology 835AQ – Data Science in Ecology_MNR

## Project Description
Large-scale databases often rely on manually extracted data, which can contain typographical errors, inconsistent terminology, multiple categorization schemes, and outdated taxonomy. Extracting data from multiple sources can also result in different classifications or identifications for the same item. Careful data cleaning and standardization are therefore essential to ensure data quality, reproducibility, and reliable downstream analyses. However, performing these tasks manually is time-consuming, highlighting the need for efficient protocols and automated standardization approaches. To address this, the project aims to ** develop a reproducible cleaning** **protocol** for standardizing data for analysis, as well as an **LLM-assisted workflow for classifying prey items** in a trophic web database.

## Repository Structure

### 01.Script
- `CleaningProtocol/` – Scripts for data cleaning, validation, and standardization.
- `LLM/` – Script for LLM-assisted classification of prey items.

### 02.Raw_Data
- `Interactions_merged.csv` – Original interaction data compiled from multiple sources.
- `Population_merged.csv` – Original population data compiled from multiple sources.
- `categories_manual.csv` – Manually classified prey items used to compare with the LLM classifications.

### 03.Output
- `interactions_final.csv` – Cleaned and standardized interaction dataset.
- `populations_final.csv` – Cleaned and standardized population dataset.
- `prey_classification_cache.csv` – Cache containing prey items that have already been classified by the LLM. New classifications are added to this file to avoid repeating API requests.

## Data Organization

### Interactions Dataset

Explicar as colunas

### Population Dataset

Explicar as colunas

### Manual Classification Dataset

Explicar as colunas

## Workflow

The **Cleaning Protocol must be run before the LLM classification script**. Data cleaning and standardization are required before the LLM step because the LLM classification is applied to the cleaned interaction dataset.

The general workflow is:

1. Run the cleaning protocol using the raw datasets in `02.Raw_Data/`.
2. The cleaned interaction and population datasets are saved in `03.Output/`.
3. Run the LLM classification script using the cleaned interaction dataset.
4. The LLM classifies unique prey items into the predefined categories.
5. Classifications are saved in `prey_classification_cache.csv`.
6. If new prey items are added to the dataset, the LLM script identifies and classifies only the new items, while keeping the previous classifications in the cache.
7. The LLM classifications can then be compared with the manual classifications to assess model accuracy.

## Prey Classification Categories

The LLM classifies prey items into six predefined categories:

- **Fish** – Fish or fish-derived material.
- **Invertebrate** – Aquatic or terrestrial invertebrate animals.
- **Plant** – Non-algal plant material.
- **Algae** – Algae or algal material.
- **Detritus** – Organic detrital material.
- **Other** – Items that do not fit into the categories above.

## Contact
  
mariananovelloro@gmail.com
