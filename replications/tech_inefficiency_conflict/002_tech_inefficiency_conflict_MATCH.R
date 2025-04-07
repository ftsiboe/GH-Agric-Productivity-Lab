rm(list=ls(all=TRUE));gc()
setwd(ifelse(Sys.info()['sysname'] =="Windows",paste0("C:/Users/",Sys.info()['user'],"/Documents/GitHub/GH-Agric-Productivity-Lab"),
             paste0("/homes/",Sys.info()['user'],"/Articles/GH/GH_AgricProductivityLab/")))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/helpers_tech_inefficiency.R"))
setwd(paste0(getwd(),"/replications/tech_inefficiency_conflict"))
dir.create("results")
dir.create("results/estimations")

DATA <- Fxn_DATA_Prep(as.data.frame(haven::read_dta("data/tech_inefficiency_conflict_data.dta")))

if(Sys.getenv("SLURM_JOB_NAME") %in% "drawlist"){
  m.specs <- Fxn_draw_spec(drawN=100,DATA=DATA,myseed=myseed)
  saveRDS(m.specs$m.specs,file="results/mspecs.rds")
  saveRDS(m.specs$drawlist,file="results/drawlist.rds")
}

m.specs <- readRDS("results/mspecs.rds")

