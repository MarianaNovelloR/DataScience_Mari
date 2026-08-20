# Biology 835AQ – Data Science in Ecology_MNR

## Project Description
Large-scale databases often rely on manually extracted data, which can contain typographical errors, inconsistent terminology, multiple categorization schemes, and outdated taxonomy. Extracting data from multiple sources can also result in different classifications or identifications for the same item. Careful data cleaning and standardization are therefore essential to ensure data quality, reproducibility, and reliable downstream analyses. However, performing these tasks manually is time-consuming, highlighting the need for efficient protocols and automated standardization approaches. To address this, the project aims to develop a **reproducible cleaning protocol** for standardizing data for analysis, as well as an **LLM-assisted workflow for classifying prey items** in a trophic web database.

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

The `interactions_final.csv` dataset contains standardized information on predator–prey interactions. Each row represents a trophic interaction between a fish population and a prey item.

| Column | Description |
|---|---|
| `Interaction` | Unique identification number of the interaction. |
| `Input_by` | Name of the person who entered the information into the database. |
| `Quantified_at_the` | Scale at which the trophic interaction was quantified. |
| `Sampled_at_the` | Scale at which the fish were sampled. |
| `Main_Source` | Identification code of the publication from which the interaction data were obtained. |
| `Individual` | Identification code of the individual fish, when available. |
| `Population` | Identification number of the population associated with the interaction. |
| `Species` | Scientific name of the predator species. |
| `Sample_size` | Number of stomachs analyzed. |
| `Method` | Method used to characterize the trophic interaction. |
| `Source` | Source of the prey item. |
| `Origin` | Origin of the prey item. |
| `Category` | Food category assigned to the prey item in the original publication. |
| `Prey` | Prey item identified in the interaction. |
| `Strength` | Value representing the contribution or importance of the prey item in the population's diet. |
| `Index` | Index or method used to quantify interaction strength. |
| `Obs` | Additional observations or relevant information about the interaction. |

### Population Dataset

The `populations_final.csv` dataset contains standardized information describing the fish populations represented in the interaction database. Each row represents a population and contains information on species, sampling location, environment, sampling period, and methodological details.

| Column | Description |
|---|---|
| `Population` | Unique identification number of the population. |
| `Includes` | Indicates whether the population data were included from a publication and/or data contribution. |
| `Species` | Scientific name of the fish species. |
| `N` | Number of stomachs analyzed, corresponding to `Sample_size` in the Interactions dataset. |
| `Population_description` | Description of the population, including information distinguishing multiple populations of the same species. |
| `lat_d` | Latitude degrees of the sampling location. |
| `lat_m` | Latitude minutes of the sampling location. |
| `lat_s` | Latitude seconds of the sampling location. |
| `lat_dir` | Latitude direction (N/S). |
| `lon_d` | Longitude degrees of the sampling location. |
| `lon_m` | Longitude minutes of the sampling location. |
| `lon_s` | Longitude seconds of the sampling location. |
| `lon_dir` | Longitude direction (E/W). |
| `Locality` | Name of the locality where the population was sampled. |
| `Type` | Type of environment in which the population occurred. |
| `Habitat` | Type of water body where the population was sampled. |
| `Sampling_statement` | Excerpt from the source describing fish sampling procedures. |
| `Benchwork` | Excerpt from the source describing laboratory procedures. |
| `Data_analysis` | Excerpt from the source describing the analysis of trophic interaction data. |
| `MonthStart` | Month when sampling started. |
| `YearStart` | Year when sampling started. |
| `MonthEnd` | Month when sampling ended. |
| `YearEnd` | Year when sampling ended. |
| `Frequency` | Frequency of sampling. |
| `Habitat size` | Size of the sampled habitat, when available. |
| `Obs` | Additional observations or relevant information about the population. |

### Manual Classification Dataset

The `categories_manual.csv` dataset contains manually assigned prey classifications used as a reference dataset for evaluating the LLM classifications. Each row corresponds to a prey item in the original interaction database, allowing the manual classifications to be directly compared with the classifications generated by the LLM.

| Column | Description |
|---|---|
| `Prey` | Prey item identified in the interaction database. |
| `Categories_general` | Manually assigned prey category used as the reference classification. |

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
