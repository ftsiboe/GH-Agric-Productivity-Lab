# Agricultural Productivity in Ghana: Datasets

## Project Overview

For details about the project and its broader context, please refer to the main [GH-Agric-Productivity-Lab README](../README.md) in the repository's root.

This directory contains the replication data files for the agricultural productivity project in Ghana. The objective is to provide a transparent and reproducible analysis framework that validates and extends the findings presented in published research.

See the [LICENSE](../LICENSE) file in the repository's root for details.

## Available Datasets

The following datasets are available in this directory:

- **`harmonized_crop_farmer_data.dta`**: Contains data on crop production and farmer characteristics.
- **`harmonized_conflict_data.dta`**: Provides information on community-level peace and social cohesion.
- **`harmonized_disability_data.dta`**: Offers details on disabilities affecting farmers and their households.
- **`harmonized_education_data.dta`**: Includes data on farmers’ educational backgrounds.
- **`harmonized_extension_services_data.dta`**: Documents extension services provided to farmers at the community level.
- **`harmonized_land_tenure_data.dta`**: Contains information on land acquisition and tenure agreements among farmers.
- **`harmonized_offfarm_work_data.dta`**: Covers data on the participation of farmers and household members in off-farm work.
- **`harmonized_resources_extraction_data.dta`**: Details community-level activities related to resource extraction.

## Merging Datasets

Datasets can be merged using common keys available in each file. When merging with the main dataset (`harmonized_crop_farmer_data.dta`), the following keys are available (when present):

- **Surveyx** (character)
- **EaId** (numeric)
- **HhId** (numeric)
- **Mid** (numeric)

Below are examples of how to merge datasets in both R and Stata.

### Example in R

```r
# Load necessary libraries
library(dplyr)
library(haven)

# Load the main dataset
crop_farmer_data <- read_dta("harmonized_crop_farmer_data.dta")

# Load another dataset (e.g., harmonized_conflict_data.dta)
conflict_data <- read_dta("harmonized_conflict_data.dta")

# Ensure common keys are of the same type
crop_farmer_data <- crop_farmer_data %>%
  mutate(Surveyx = as.character(Surveyx),
         EaId = as.numeric(EaId),
         HhId = as.numeric(HhId),
         Mid = as.numeric(Mid))

conflict_data <- conflict_data %>%
  mutate(Surveyx = as.character(Surveyx),
         EaId = as.numeric(EaId),
         HhId = as.numeric(HhId),
         Mid = as.numeric(Mid))

# Merge the datasets using the common keys
merged_data <- left_join(crop_farmer_data, conflict_data,
                         by = c("Surveyx", "EaId", "HhId", "Mid"))

# Save the merged dataset
write_dta(merged_data, "merged_data.dta")

```
### Example in Stata

```r
* Load the main dataset
use "harmonized_crop_farmer_data.dta", clear

* Merge with another dataset (e.g., harmonized_conflict_data.dta)
merge 1:1 Surveyx EaId HhId Mid using "harmonized_conflict_data.dta"

* Review the merge results
tab _merge

* Save the merged dataset
save "merged_data.dta", replace


```

*Maintained by [ftsiboe](https://github.com/ftsiboe)*
