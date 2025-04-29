###############################################################################
# Title: Technical Inefficiency & Financial Inclusion Analysis Replication
# Author: Francis Tsiboe
# Email: ftsiboe@hotmail.com
#
# Purpose:
# This script prepares the data and runs a series of model estimations for a 
# replication study on technical inefficiency with a focus on financial inclusion.
# It reads raw data, processes it, creates model specifications, iterates over 
# these specifications to perform estimations, and saves the results for further analysis.
#
# How to Cite: If you find this repository useful, please star this project and cite our papers.
#
# Usage:
# Run this script within an R environment. It will set up the appropriate working
# directories based on the operating system, load necessary helper functions, 
# prepare data, and then conduct the estimation procedures. Results are saved as RDS 
# files in the "results/estimations" folder.
###############################################################################

# Clear all objects from the workspace and run garbage collection
rm(list = ls(all = TRUE))
gc()

# Set the working directory based on the operating system
setwd(ifelse(Sys.info()['sysname'] =="Windows","C:/GitHub/GH-Agric-Productivity-Lab",
             paste0("/homes/",Sys.info()['user'],"/Articles/GH/GH_AgricProductivityLab/")))
PROJECT <- getwd()

# Load helper functions for technical inefficiency analysis
source(paste0(getwd(), "/codes/helpers_tech_inefficiency.R"))

# Change working directory to the replication folder for tech inefficiency & financial inclusion
setwd(paste0(getwd(), "/replications/tech_inefficiency_nonfarm_enterprise"))

# Create directories to store results and estimations if they do not exist already
dir.create("results", showWarnings = FALSE)
dir.create("results/estimations", showWarnings = FALSE)

# Load and prepare the dataset using a helper function.
# The raw data is read from a Stata (.dta) file.
DATA <- Fxn_DATA_Prep(as.data.frame(haven::read_dta("data/tech_inefficiency_nonfarm_enterprise_data.dta")))

# Convert the 'EduCat' variable to character for consistency in further processing
DATA$EduCat <- as.character(DATA$EduCat)

# Obtain forms for function and distribution specifications used in estimation
FXNFORMS  <- Fxn_SF_forms()$FXNFORMS
DISTFORMS <- Fxn_SF_forms()$DISTFORMS

# =============================================================================
# Generate Model Specifications (SPECS)
# =============================================================================
# This anonymous function creates a set of unique model specifications based on 
# defined criteria (e.g., technical variable "Credit", levels, and other grouping variables).
function(){
  
  mainD <- 1
  mainF <- 2
  
  # Generate initial unique model specifications using a helper function
  SPECS <- unique(Fxn_SPECS(TechVarlist = c("nonfarm_hh","nonfarm_close","nonfarm_member","nonfarm_spouse","nonfarm_self", "nonfarm_child"), mainD = mainD, mainF = mainF))
  
  # Create two subsets of SPECS for 'fullset' and 'optimal' and then combine them.
  SPECS <- rbind(
    data.frame(SPECS[(SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% "nonfarm_hh" & 
                        SPECS$level %in% "Pooled"),], nnm = "fullset"),
    data.frame(SPECS[(SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% "nonfarm_hh" & 
                        SPECS$level %in% "Pooled"),], nnm = "optimal"),
    data.frame(SPECS[!(SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% "nonfarm_hh" & 
                         SPECS$level %in% "Pooled"),], nnm = "optimal"))
  
  # Exclude specifications with 'CropID' if the level is not "Pooled"
  SPECS <- SPECS[!(SPECS$disasg %in% c("CropID") & !SPECS$level %in% "Pooled"),]
  #SPECS <- SPECS[!SPECS$disasg %in% c( "Female","Region","Ecozon","EduCat","EduLevel","AgeCat"),]
  
  # Remove SPECS that have already been estimated (i.e., their result files exist)
  SPECS <- SPECS[!(paste0(SPECS$disasg, "_", SPECS$level, "_", SPECS$TechVar, "_",
                          names(FXNFORMS)[SPECS$f], "_", names(DISTFORMS)[SPECS$d], "_", SPECS$nnm, ".rds") %in%
                     list.files("results/estimations/")), ]
  
  # Reset row names for consistency
  row.names(SPECS) <- 1:nrow(SPECS)
  
  # Save the specifications to an RDS file for later use
  saveRDS(SPECS, file = "results/SPECS.rds")
}

# Load the saved specifications for further processing
SPECS <- readRDS("results/SPECS.rds")

# If running on a SLURM cluster, subset the specifications based on the task ID
if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  SPECS <- SPECS[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")), ]
}

# =============================================================================
# Loop Over Model Specifications and Run Estimations
# =============================================================================
lapply(
  c(1:nrow(SPECS)),
  function(fit){
    # fit <- 2
    # Extract the current specification parameters
    f       <- SPECS$f[fit]
    d       <- SPECS$d[fit]
    disasg  <- SPECS$disasg[fit]
    level   <- SPECS$level[fit]
    TechVar <- SPECS$TechVar[fit]
    nnm     <- SPECS$nnm[fit]
    # nnm <- "optimal"
    # Construct the expected filename for the current model estimation
    result_filename <- paste0(disasg, "_", level, "_", TechVar, "_", 
                              names(FXNFORMS)[f], "_", names(DISTFORMS)[d], "_", nnm, ".rds")
    
    # Proceed only if the result file does not already exist
    if(!result_filename %in% list.files("results/estimations/")){
      
      # -------------------------------
      # Data Preparation for Estimation
      # -------------------------------
      # Subset the data based on the current specification
      data <- DATA[DATA[, SPECS$disasg[fit]] %in% SPECS$level[fit], ]
      
      # Remove rows with NA values for the technical variable under study
      data <- data[!is.na(data[, SPECS$TechVar[fit]]), ]
      
      # Create a numeric coding for the technical variable
      data$Tech <- as.numeric(as.integer(as.factor(as.character(data[, SPECS$TechVar[fit]]))))
      
      # If the disaggregation is not by 'CropID', further restrict to the "Pooled" category
      if(!SPECS$disasg[fit] %in% "CropID") data <- data[data[, "CropID"] %in% "Pooled", ]
      
      # Create a lookup table (key) for the technical variable
      TechKey <- unique(data[c("Tech", SPECS$TechVar[fit])])
      TechKey <- TechKey[order(TechKey$Tech), ]
      
      # -------------------------------
      # Create Crop Dummy Variables
      # -------------------------------
      # For a list of specified crops, generate a dummy variable if the area is greater than 0
      for(crop in c("Beans", "Cassava", "Cocoa", "Cocoyam", "Other", "Millet", 
                    "Okra", "Palm", "Peanut", "Pepper", "Plantain", "Rice", 
                    "Sorghum", "Tomatoe", "Yam", "Maize")){
        data[, paste0("CROP_", crop)] <- ifelse(data[, paste0("Area_", crop)] > 0, crop, NA)
      }
      
      # Identify area variables with a mean greater than 0.03
      ArealistX <- names(data)[grepl("Area_", names(data))]
      ArealistX <- ArealistX[ArealistX %in% paste0("Area_", c("Beans", "Cassava", "Cocoa", "Cocoyam",
                                                              "Other", "Millet", "Okra", "Palm", "Peanut",
                                                              "Pepper", "Plantain", "Rice", "Sorghum",
                                                              "Tomatoe", "Yam", "Other"))]
      ArealistX <- apply(data[names(data)[names(data) %in% ArealistX]], 2, mean) > 0.03
      ArealistX <- names(ArealistX)[ArealistX %in% TRUE]
      
      # Create a combined area variable if at least one valid area variable exists
      if(length(ArealistX) > 0){ 
        data$Area_Other <- 1 - rowSums(data[c(ArealistX[!ArealistX %in% "Area_Other"], "Area_Maize")], na.rm = TRUE)
        ArealistX <- unique(c(ArealistX, "Area_Other"))
      }
      
      # -------------------------------
      # Draw Estimations
      # -------------------------------
      # Load the draw list which specifies sampling or bootstrap iterations
      drawlist <- readRDS("results/drawlist.rds")

      # Run the estimations using the function 'Fxn_draw_estimations'
      res <- lapply(
        unique(drawlist$ID)[1:3], Fxn_draw_estimations,
        data              = data,
        surveyy           = F,
        intercept_shifters = list(Svarlist = ArealistX, Fvarlist = c("Survey", "Ecozon")),
        intercept_shiftersM = list(Svarlist = NULL, Fvarlist = c("Survey", "Ecozon")),
        drawlist          = drawlist,
        wvar              = "Weight",
        yvar              = "HrvstKg",
        xlist             = c("Area", "SeedKg", "HHLaborAE", "HirdHr", "FertKg", "PestLt"),
        ulist             = list(Svarlist = c("lnAgeYr", "lnYerEdu", "CrpMix"),
                                 Fvarlist = c("Female", "Survey", "Ecozon", "Extension", "EqipMech", "OwnLnd", "Credit")),
        ulistM            = list(Svarlist = c("lnAgeYr", "lnYerEdu", "CrpMix"),
                                 Fvarlist = c("Female", "Survey", "Ecozon", "Extension", "EqipMech", "OwnLnd", "Credit")),
        UID               = c("UID", "Survey", "CropID", "HhId", "EaId", "Mid"),
        f                 = f,
        d                 = d,
        tvar              = TechVar,
        nnm               = nnm
      )
       
      # Summarize the estimation draws using a helper function
      res <- Fxn.draw_summary(res = res, TechKey = TechKey)
      
      # Append specification metadata to each element in the summary results
      for(xx in 1:length(res)){
        tryCatch({
          res[[xx]][, "FXN"]    <- names(FXNFORMS)[f]
          res[[xx]][, "DIS"]    <- names(DISTFORMS)[d]
          res[[xx]][, "disasg"] <- disasg
          res[[xx]][, "level"]  <- level
          res[[xx]][, "TCH"]    <- TechVar
          res[[xx]][, "TCHLvel"]<- factor(res[[xx]][, "Tech"],
                                          levels = c(-999, TechKey$Tech, 999),
                                          labels = c("National", TechKey[, 2], "Meta"))
        }, error = function(e){}
        )
      }

      
      # Optional block: Print specific summaries for different estimation types (TE, TGR, MTE)
      function(){
        Main <- res$ef_mean
        Main <- Main[Main$Survey %in% "GLSS0", ]
        Main <- Main[!Main$sample %in% "unmatched", ]
        Main <- Main[Main$stat %in% "wmean", ]
        Main <- Main[Main$CoefName %in% "efficiencyGap_pct", ]
        Main <- Main[Main$restrict %in% "Restricted", ]
        Main <- Main[Main$estType %in% "teBC", ]
        Main[Main$type %in% "TGR", c("sample", "type", "Tech", "Estimate")]
        Main[Main$type %in% "TE", c("sample", "type", "Tech", "Estimate")]
        Main[Main$type %in% "MTE", c("sample", "type", "Tech", "Estimate")]
      }
      
      # Add a name to the result based on the current specification
      res[["names"]] <- paste0(disasg, "_", level, "_", TechVar, "_", 
                               names(FXNFORMS)[f], "_", names(DISTFORMS)[d], "_", nnm)
      
      # If using the full set, remove some components not needed for the output summary
      if(!(TechVar %in% "nonfarm_hh" & nnm %in% "optimal" & level %in% "Pooled" & disasg %in% "CropID" & f %in% 2 & d %in% 1)){
        res$rk_dist <- NULL
        res$rk_mean <- NULL
        res$rk_samp <- NULL
        res$el_samp <- NULL
        res$ef_samp <- NULL 
      }
      
      # Save the estimation results to an RDS file within the results/estimations folder
      saveRDS(res, file = paste0("results/estimations/", result_filename))
      
    }
    # Return the index (fit) of the current specification (useful for logging or SLURM)
    return(fit)
  }
)
