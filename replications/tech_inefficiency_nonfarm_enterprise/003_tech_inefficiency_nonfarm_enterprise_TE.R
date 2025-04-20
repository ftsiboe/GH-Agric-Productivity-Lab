
# Author: Ftsiboe
# Repository: https://github.com/ftsiboe/GH-Agric-Productivity-Lab
# File: 003_tech_inefficiency_financial_inclusion_TE.R
# Date: 2025-04-05
#
# Purpose:
# This script is designed to analyze the technical inefficiency and financial inclusion 
# in agricultural productivity in Ghana. It prepares and processes the data, 
# runs specified models, and generates results for further analysis.
#
# How to Cite:
# If you use this code or data in your research, please cite the following work:
# Tsiboe, Francis. (2025). "The Impact of Financial Inclusion on Agricultural Productivity and Efficiency in Ghana.". 
# Available at: https://github.com/ftsiboe/GH-Agric-Productivity-Lab
#


# Clear all objects from the workspace and run garbage collection
rm(list=ls(all=TRUE));gc()

# Set the working directory based on the operating system
setwd(ifelse(Sys.info()['sysname'] =="Windows","C:/GitHub/GH-Agric-Productivity-Lab",
             paste0("/homes/",Sys.info()['user'],"/Articles/GH/GH_AgricProductivityLab/")))
PROJECT <- getwd()  # Store the current working directory

# Source helper functions from an external R script
source(paste0(getwd(),"/codes/helpers_tech_inefficiency.R"))

# Set the working directory to the specific replication study folder
setwd(paste0(getwd(),"/replications/tech_inefficiency_nonfarm_enterprise"))

# Create directories for saving results
dir.create("results")
dir.create("results/te")

# Load and prepare the dataset
DATA <- Fxn_DATA_Prep(as.data.frame(haven::read_dta("data/tech_inefficiency_nonfarm_enterprise_data.dta")))

# Filter the dataset for pooled crop data and create a treatment variable
DATA <- DATA[as.character(haven::as_factor(DATA$CropID)) %in% "Pooled",]
DATA$Treat <- as.numeric(DATA$nonfarm_hh > 0)

# Define lists of variable names for different categories
Arealist <- names(DATA)[grepl("Area_",names(DATA))]
Arealist <- Arealist[Arealist %in% paste0("Area_",c("Beans","Cassava","Cocoa","Cocoyam","Maize","Millet","Okra","Palm","Peanut",
                                                    "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam"))]

Emch <- c("Survey","Region","Ecozon","Locality","Female")
Scle <- c("AgeYr","YerEdu","HHSizeAE","FmleAERt","Depend","CrpMix",Arealist)
Fixd <- c("OwnLnd","Ethnic","Marital","Religion","Head","Credit")

# Filter the dataset for complete cases based on selected variables
DATA <- DATA[complete.cases(DATA[c("Surveyx","EaId","HhId","Mid","UID","Weight","Treat",Emch,Scle,Fixd)]),]

# Display summary statistics for the selected variables
summary(DATA[c(Emch,Scle,Fixd)])

# Load model specifications from an RDS file
m.specs <- readRDS("results/mspecs.rds")

# Check if the script is run within a SLURM job and perform calculations accordingly
if(Sys.getenv("SLURM_JOB_NAME") %in% c("te_all","te_fin")){
  if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
    m.specs <- m.specs[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
  }
  lapply(1:nrow(m.specs),Fxn_te_cals)
}

# Generate summary results if the script is run within a SLURM summary job
if(Sys.getenv("SLURM_JOB_NAME") %in% c("te_sum")){
  Fxn_te_summary()
}

