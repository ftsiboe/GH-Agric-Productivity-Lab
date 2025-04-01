rm(list=ls(all=TRUE));gc()
setwd(ifelse(Sys.info()['sysname'] =="Windows",paste0("C:/Users/",Sys.info()['user'],"/Documents/GitHub/GH-Agric-Productivity-Lab"),
             paste0("/homes/",Sys.info()['user'],"/Articles/GH/GH_AgricProductivityLab/")))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/helpers_tech_inefficiency.R"))
setwd(paste0(getwd(),"/replications/tech_inefficiency_disability"))
dir.create("results")
dir.create("results/te")
DATA <- Fxn_DATA_Prep(as.data.frame(haven::read_dta("data/tech_inefficiency_disability_data.dta")))

DATA <- DATA[as.character(haven::as_factor(DATA$CropID)) %in% "Pooled",]
DATA$Treat <- as.numeric(DATA$disabled %in% 1)

Arealist <- names(DATA)[grepl("Area_",names(DATA))]
Arealist <- Arealist[Arealist%in% paste0("Area_",c("Beans","Cassava","Cocoa","Cocoyam","Maize","Millet","Okra","Palm","Peanut",
                                                   "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam"))]

Emch <- c("Survey","Region","Ecozon","Locality","Female")
Scle <- c("AgeYr","YerEdu","HHSizeAE","FmleAERt","Depend","CrpMix",Arealist)
Fixd <- c("Credit","OwnLnd","Ethnic","Marital","Religion","Head")

DATA <- DATA[complete.cases(DATA[c("Surveyx","EaId","HhId","Mid","UID","Weight","Treat",Emch,Scle,Fixd)]),]
summary(DATA[c(Emch,Scle,Fixd)])

m.specs <- readRDS("results/mspecs.rds")

if(Sys.getenv("SLURM_JOB_NAME") %in% c("te_all","te_disa")){
  if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
    m.specs <- m.specs[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
  }
  lapply(1:nrow(m.specs),Fxn_te_cals)
}

if(Sys.getenv("SLURM_JOB_NAME") %in% c("te_sum")){
  Fxn_te_summary()
}


