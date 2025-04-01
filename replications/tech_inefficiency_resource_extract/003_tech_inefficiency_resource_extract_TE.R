rm(list=ls(all=TRUE));gc()
setwd(ifelse(Sys.info()['sysname'] =="Windows",paste0("C:/Users/",Sys.info()['user'],"/Documents/GitHub/GH-Agric-Productivity-Lab"),
             paste0("/homes/",Sys.info()['user'],"/Articles/GH/GH_AgricProductivityLab/")))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/helpers_tech_inefficiency.R"))
setwd(paste0(getwd(),"/replications/tech_inefficiency_resource_extract"))
dir.create("results")
dir.create("results/te")

DATA <- Fxn_DATA_Prep(as.data.frame(haven::read_dta("data/tech_inefficiency_resource_extract_data.dta")))
DATA <- DATA[as.character(haven::as_factor(DATA$CropID)) %in% "Pooled",]
DATA$Treat <- as.numeric(DATA$extraction_any %in% 1)

Arealist <- names(DATA)[grepl("Area_",names(DATA))]
Arealist <- Arealist[Arealist%in% paste0("Area_",c("Beans","Cassava","Cocoa","Cocoyam","Maize","Millet","Okra","Palm","Peanut",
                                                   "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam"))]

Emch <- c("Survey","Region","Ecozon","Locality","Female")
Scle <- c("AgeYr","YerEdu","HHSizeAE","FmleAERt","Depend","CrpMix",Arealist)
Fixd <- c("Credit","OwnLnd","Ethnic","Marital","Religion","Head")

m.specs <- readRDS("results/mspecs.rds")

if(Sys.getenv("SLURM_JOB_NAME") %in% c("te_all","te_res")){
  if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
    m.specs <- m.specs[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
  }
  lapply(1:nrow(m.specs),Fxn_te_cals)
}

if(Sys.getenv("SLURM_JOB_NAME") %in% c("te_sum")){
  Fxn_te_summary()
}

