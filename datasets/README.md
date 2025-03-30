# Agricultural-Productivity-in-Ghana:Datasets

## Project Overview
The objective is to ascertain whether observed production shortfalls in Ghana are solely due to farmer inefficiency, technology gaps, or some combination of the two.

The project consists of a series of articles using samples of farmers drawn from multiple cross-sectional population-based surveys conducted throughout Ghana from 1987 to 2017. These articles aim to estimate meta-stochastic-frontier (MSF) models to determine the sources of crop production shortfalls in Ghana.

By putting the sources of crop production shortfalls in Ghana on a solid empirical footing, it helps assess the validity of these sources while informing policy dialogue where production needs to be increased.


## Available Datasets
The following datasets are available in this directory:

- `harmonized_conflict_data.dta`: Contains data on conflicts and their impact on agricultural productivity.
- `harmonized_crop_farmer_data.dta`: Includes data on crop production and farmer characteristics.
- `harmonized_disability_data.dta`: Provides information on farmers with disabilities and their production efficiency.
- `harmonized_education_data.dta`: Contains data on education levels and their correlation with farm productivity.
- `harmonized_extension_services_data.dta`: Data on extension services provided to farmers.
- `harmonized_land_tenure_data.dta`: Information on land tenure and its effect on agricultural productivity.
- `harmonized_offfarm_work_data.dta`: Includes data on off-farm work and its impact on farm productivity.
- `harmonized_resources_extraction_data.dta`: Contains data on resource extraction activities and agricultural performance.

## Merging Datasets
To merge other datasets with `harmonized_crop_farmer_data.dta`, you can use the following common keys where present: `Surveyx` (character), `EaId` (numeric), `HhId` (numeric), and `Mid` (numeric).

### Example in R

```r
# Load necessary libraries
library(dplyr)
library(haven)

# Load the main dataset
crop_farmer_data <- read_dta("harmonized_crop_farmer_data.dta")

# Load another dataset, e.g., harmonized_conflict_data.dta
conflict_data <- read_dta("harmonized_conflict_data.dta")

# Convert common keys to the same type if necessary
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
merged_data <- left_join(crop_farmer_data, conflict_data, by = c("Surveyx", "EaId", "HhId", "Mid"))

# Save the merged dataset
write_dta(merged_data, "merged_data.dta")

```
### Example in Stata

```r
* Load the main dataset
use "harmonized_crop_farmer_data.dta", clear

* Load another dataset, e.g., harmonized_conflict_data.dta
merge 1:1 Surveyx EaId HhId Mid using "harmonized_conflict_data.dta"

* Check the merge result
tab _merge

* Save the merged dataset
save "merged_data.dta", replace

```
