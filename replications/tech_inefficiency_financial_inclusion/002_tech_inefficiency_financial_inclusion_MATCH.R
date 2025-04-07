# Author: ftsiboe
# Date: 2025-04-05
# Repository: GH-Agric-Productivity-Lab
# File: 002_tech_inefficiency_financial_inclusion_MATCH.R
# 
# Purpose:
# This script is designed to analyze the technical inefficiency and financial inclusion of farmers in Ghana. 
# It prepares data, matches samples, and performs various analyses to investigate the factors contributing to 
# production shortfalls.
#
# How to Cite:
# When using this code, please cite the repository and the resulting published work as follows:
# ftsiboe. (2025). GH-Agric-Productivity-Lab. GitHub repository. https://github.com/ftsiboe/GH-Agric-Productivity-Lab
# ftsiboe. (2025). Technical Inefficiency and Financial Inclusion in Ghanaian Agriculture. Journal of Agricultural Studies.

rm(list=ls(all=TRUE));gc()
setwd(ifelse(Sys.info()['sysname'] =="Windows",paste0("C:/Users/",Sys.info()['user'],"/Documents/GitHub/GH-Agric-Productivity-Lab"),
             paste0("/homes/",Sys.info()['user'],"/Articles/GH/GH_AgricProductivityLab/")))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/helpers_tech_inefficiency.R"))
setwd(paste0(getwd(),"/replications/tech_inefficiency_financial_inclusion"))
dir.create("results")
dir.create("results/matching")
DATA <- Fxn_DATA_Prep(as.data.frame(haven::read_dta("data/tech_inefficiency_financial_inclusion_data.dta")))

DATA <- DATA[as.character(haven::as_factor(DATA$CropID)) %in% "Pooled",]

DATA$Treat <- DATA$credit_hh > 0

Arealist <- names(DATA)[grepl("Area_",names(DATA))]
Arealist <- Arealist[Arealist%in% paste0("Area_",c("Beans","Cassava","Cocoa","Cocoyam","Maize","Millet","Okra","Palm","Peanut",
                                                   "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam"))]

Emch <- c("Survey","Region","Ecozon","Locality","Female")
Scle <- c("AgeYr","YerEdu","HHSizeAE","FmleAERt","Depend","CrpMix",Arealist,"HHFinWorker","BankKm","RoadKm","TrnprtKm" )
Fixd <- c("OwnLnd","Ethnic","Marital","Religion","Head","Insured","Banked","FinWorker",names(DATA)[grepl("InstTyp_",names(DATA))],
          names(DATA)[grepl("AccTyp_",names(DATA))],names(DATA)[grepl("PrdTyp_",names(DATA))])

Emch.formula  <- paste0(paste0("factor(",Emch,")"),collapse = "+")
Match.formula <- paste0("Treat~",paste0(c(Scle),collapse = "+"))
for(var in c(Fixd)){ Match.formula<-paste0(Match.formula,"+factor(",var,")")}

DATA <- DATA[complete.cases(DATA[c("Surveyx","EaId","HhId","Mid","UID","Weight","Treat",Emch,Scle,Fixd)]),]
summary(DATA[c(Emch,Scle,Fixd)])

if(Sys.getenv("SLURM_JOB_NAME") %in% "drawlist"){
  m.specs <- Fxn_draw_spec(drawN=100,DATA=DATA,myseed=myseed)
  saveRDS(m.specs$m.specs,file="results/mspecs.rds")
  saveRDS(m.specs$drawlist,file="results/drawlist.rds")
}

m.specs <- readRDS("results/mspecs.rds")

# m.specs <- m.specs[! paste0(REPO,"Results/matching/Match",stringr::str_pad(m.specs$ARRAY,4,pad="0"),".rds") %in%
#                      list.files(paste0(REPO,"Results/matching"),full.names = T),]

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  m.specs <- m.specs[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}

if(Sys.getenv("SLURM_JOB_NAME") %in% c("match_all","match_fin")){
  lapply(
    1:nrow(m.specs), #
    function(i,DATA){
      tryCatch({
        # i <- 1;m.data <- DATA
        Sampels <- Fxn_Sampels(DATA=DATA,Emch=Emch,Scle=Scle,Fixd=Fixd,m.specs=m.specs,i=i,drawlist=readRDS("results/drawlist.rds"))
        if(! m.specs$boot[i] %in% 0){Sampels[["m.out"]] <- NULL}
        saveRDS(Sampels,file=paste0("results/matching/Match",stringr::str_pad(m.specs$ARRAY[i],4,pad="0"),".rds"))
      }, error=function(e){})
      return(i)
    },DATA=DATA)
  
  # 
}

if(Sys.getenv("SLURM_JOB_NAME") %in% c("cov_bal")){
  Fxn_Covariate_balance()
}

# unlink(list.files(getwd(),pattern =paste0(".out"),full.names = T))
