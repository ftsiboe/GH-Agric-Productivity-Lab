# Agricultural Productivity in Ghana: Datasets

## Project Overview

For an overview of the broader project context, please refer to the main [GH-Agric-Productivity-Lab README](../README.md) in the repository root.

This directory contains replication data files for the agricultural productivity project in Ghana. The objective is to provide a transparent and reproducible analysis framework that validates and extends the findings from published research.

See the [LICENSE](../LICENSE) file in the repository root for details.

We welcome constructive feedback on these datasets and are actively seeking collaborations to further research using this data.

## Data Sources and Construction

The project relies on farm-level data drawn from population-based surveys administered periodically (approximately every five years) in Ghana. These surveys include all seven rounds of the Ghana Living Standards Surveys (GLSS). Each GLSS follows a two-stage sampling procedure:
1. **Stage 1:** Enumeration Areas (EAs) are selected as primary sampling units (PSUs) for Ghana's ten regions using probability proportional to population size.
2. **Stage 2:** Within each PSU, a list of households is collated to form the secondary sampling units (SSUs), from which 15 households are systematically selected.

The surveys were designed to provide nationally and regionally representative indicators. The sample sizes for GLSS1 to GLSS7 are as follows:

| Survey Name | Period fielded    | Households | Nousehold members       |
|-------------|---------|------------|-------------------------|
| [GLSS1](https://microdata.statsghana.gov.gh/index.php/catalog/7)       | 1987/88 | 3,147      | 15,492  |
| [GLSS2](https://microdata.statsghana.gov.gh/index.php/catalog/4)       | 1988/89 | 3,194      | 14,924  |
| [GLSS3](https://microdata.statsghana.gov.gh/index.php/catalog/12)       | 1991/92 | 4,523      | 20,403  |
| [GLSS4](https://microdata.statsghana.gov.gh/index.php/catalog/14)       | 1998/99 | 5,998      | 26,411  |
| [GLSS5](https://microdata.statsghana.gov.gh/index.php/catalog/5)       | 2005/06 | 8,687      | 37,128  |
| [GLSS6](https://microdata.statsghana.gov.gh/index.php/catalog/72)       | 2012/13 | 16,772     | 72,372  |
| [GLSS7](https://microdata.statsghana.gov.gh/index.php/catalog/97)       | 2016/17 | 14,009     | 59,864  |

Further details on the sampling and data collection are available in the survey documentation, which is also accessible at [National Data Archive](https://microdata.statsghana.gov.gh/index.php/home) of the [Ghana Statistical Service](https://statsghana.gov.gh/).

The GLSS surveys have been harmonized into a comprehensive farmer-level dataset, as described in the report [**Nationally Representative Farm/Household Level Dataset on Crop Production in Ghana from 1987-2017**](https://github.com/ftsiboe/GH-Agric-Productivity-Lab/blob/master/datasets/30%20Years%20of%20Crop%20Production%20in%20Ghana%20-%20Final.pdf).

The project gratefully acknowledges the [Ghana Statistical Service](https://statsghana.gov.gh/). for making the Ghana Living Standards Survey dataset publicly available. 

## Available Datasets

The following datasets are available in this directory:

- **`harmonized_crop_farmer_data.dta`**: Contains data on crop production and farmer characteristics.
- **`harmonized_conflict_data.dta`** : Provides information on community-level peace and social cohesion.
- **`harmonized_disability_data.dta`** : Offers details on disabilities affecting farmers and their households.
- **`harmonized_education_data.dta`** : Includes data on farmers’ educational backgrounds.
- **`harmonized_extension_services_data.dta`** : Documents extension services provided to farmers at the community level.
- **`harmonized_land_tenure_data.dta`**  : Contains information on land acquisition and tenure agreements among farmers.
- **`harmonized_offfarm_work_data.dta`** : Covers data on the participation of farmers and household members in off-farm work.
- **`harmonized_resources_extraction_data.dta`**  : Details community-level activities related to resource extraction.

## Merging Datasets

Datasets can be merged using the following common keys (when available):

- **Surveyx** (character)
- **EaId** (numeric)
- **HhId** (numeric)
- **Mid** (numeric)

Below are examples of merging datasets in both R and Stata.

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
