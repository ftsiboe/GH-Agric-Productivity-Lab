#------------------------------------
# Preliminaries                   ####
rm(list=ls(all=TRUE));gc()
setwd(ifelse(Sys.info()['sysname'] =="Windows",paste0("C:/Users/",Sys.info()['user'],"/Documents/GitHub/GH-Agric-Productivity-Lab"),
             paste0("/homes/",Sys.info()['user'],"/Articles/GH/GH_AgricProductivityLab/")))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/figures_and_tables.R"))
setwd(paste0(getwd(),"/replications/tech_inefficiency_conflict"))
dir.create("results")
dir.create("results/figures")
dir.create("results/figuresData")
#mspecs_optimal <- readRDS("results/mspecs_optimal.rds")
Keep.List<-c("Keep.List",ls())
Keep.List<-c("Keep.List",ls())
#------------------------------------
# Index                           ####
data <- as.data.frame(haven::read_dta("data/tech_inefficiency_conflict_data.dta"))
data$IndexTech <- data$index0CAT+1
weights <- as.data.frame(
  data.table::rbindlist(
    lapply(
      0:5,function(z) {
        w <- colMeans(data[names(data)[grepl(paste0("w",z,"_"),names(data))]],na.rm=T)
        w <- data.frame(wname = gsub(paste0("w",z,"_"),paste0("w",z,"xxx"),names(w)),weight=c(t(w)))
        return(w)
      }), fill = TRUE))

weights <- rbind(data.frame(wname = c("w6xxxs13eq14"),weight=1),weights)

weights <- tidyr::separate(weights,"wname",into=c("indicator","variable"),sep="xxx",remove=F)

domains    <- weights[weights$indicator %in% "w0",]
domains$indnum <- as.numeric(gsub("[^0-9]","",domains$variable))
domains <- domains[c("indnum","weight")]
names(domains) <- c("indnum","domain_weight")
indicators <- weights[!weights$indicator %in% "w0",]
indicators$indnum <- as.numeric(gsub("[^0-9]","",indicators$indicator))
indicators <- indicators[c("indnum","variable","weight")]
names(indicators) <- c("indnum","indicator_name","indicator_weight")

weights <- dplyr::inner_join(indicators,domains, by=c("indnum"))
weights$contribution <- weights$indicator_weight*weights$domain_weight

headcount <- data
headcount$IndexTech <- 0
headcount <- doBy::summaryBy(list(weights$indicator_name,"IndexTech"),data=rbind(headcount,data),FUN=c(length,mean,sd))
headcount <- headcount %>%  tidyr::gather(stat, value, 2:ncol(headcount))
headcount <- tidyr::separate(headcount,"stat",into=c("indicator_name","stat"),sep="[.]")
headcount$stat <- paste0(headcount$stat,headcount$IndexTech)
headcount <- headcount[c("indicator_name","stat","value")] %>%  tidyr::spread(stat, value)
headcount <- dplyr::inner_join(weights,headcount, by=c("indicator_name"))
headcount <- headcount[order(-headcount$indicator_weight),]
headcount <- headcount[order(-headcount$domain_weight),]

saveRDS(headcount,file=paste0("results/figuresData/index_headcounts.rds"))

wb <- openxlsx::loadWorkbook("results/tech_inefficiency_conflict_results.xlsx")
openxlsx::writeData(wb, sheet = "Index",headcount , colNames = T)
openxlsx::saveWorkbook(wb,"results/tech_inefficiency_conflict_results.xlsx",overwrite = T)

#------------------------------------
# Main Specification              ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- tab_main_specification(list.files("results/estimations/",pattern = "CropID_Pooled",full.names = T))
wb <- openxlsx::loadWorkbook("results/tech_inefficiency_conflict_results.xlsx")
openxlsx::writeData(wb, sheet = "msf",res , colNames = T, startCol = "A", startRow = 1)
openxlsx::saveWorkbook(wb,"results/tech_inefficiency_conflict_results.xlsx",overwrite = T)
#------------------------------------
# Fig - Heterogeneity             ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- readRDS("results/estimations/CropID_Pooled_index0CAT_TL_hnormal.rds")$disagscors
res$disasg <- as.character(res$disagscors_var)
res$level <- as.character(res$disagscors_level)
res <- res[res$estType %in% "teBC",]
res <- res[res$Survey %in% "GLSS0",]
res <- res[res$restrict %in% "Restricted",]
res <- res[res$stat %in% "mean",]
res <- res[res$CoefName %in% "disag_efficiencyGap_pct",]
res <- res[c("disasg","level","FXN","DIS","Survey","input","TCH","Tech","CoefName","Estimate","Estimate.sd","jack_pv")]
res$Tech <- factor(res$Tech,levels = 1:3,c("Low","Medium","High"))

fig <- fig_heterogeneity00(res=res,y_title="Percentage difference (base = low)\n")

fig[["genderAge"]] <- fig[["genderAge"]] + theme(axis.text.x = element_text(size = 5.5)) +
  scale_fill_manual(name="Score:",values = c("thistle","violet","purple")) +
  scale_color_manual(name="Score:",values = c("thistle","violet","purple")) +
  scale_shape_manual(name="Peace and social cohesion:",values = c(21,22,23,24,25,8,4))
ggsave("results/figures/heterogeneity_genderAge.png", fig[["genderAge"]],dpi = 600,width = 8.3, height = 4.7)

fig[["crop_region"]] <- fig[["crop_region"]] + theme(axis.text.x = element_text(size = 5.5)) +
  scale_fill_manual(name="Score:",values = c("thistle","violet","purple")) +
  scale_color_manual(name="Score:",values = c("thistle","violet","purple")) +
  scale_shape_manual(name="Peace and social cohesion:",values = c(21,22,23,24,25,8,4))
ggsave("results/figures/heterogeneity_crop_region.png", fig[["crop_region"]],dpi = 600,width = 8.3, height = 6.2)
#------------------------------------
# Fig - Robustness                ####           
rm(list= ls()[!(ls() %in% c(Keep.List))])
data <- as.data.frame(
  data.table::rbindlist(
    lapply(
      c("results/estimations/CropID_Pooled_index0CAT_CD_hnormal.rds",
        list.files("results/estimations/",pattern = "CropID_Pooled_index0CAT_TL_",full.names = T)),
      function(file) {
        tryCatch({
          # file <- list.files("Results/Estimations/",pattern = "TL_hnormal_optimal.rds",full.names = T)[1]
          ef_mean <- readRDS(file)$ef_mean
          #ef_mean <- ef_mean[ef_mean$stat %in% "wmean",]
          #ef_mean <- ef_mean[ef_mean$estType %in% "teBC",]
          ef_mean <- ef_mean[ef_mean$type %in% c("TE","TGR","MTE"),]
          ef_mean <- ef_mean[ef_mean$CoefName %in% c("efficiencyGap_pct"),]
          #ef_mean <- ef_mean[ef_mean$restrict %in% c("Restricted"),]
          ef_mean <- ef_mean[ef_mean$Survey %in% c("GLSS0"),]
          ef_mean$file <- file 
          return(ef_mean)
        }, error = function(e){return(NULL)})
      }), fill = TRUE))

data <- data %>% group_by(sample,Tech,type,estType,Survey,stat,CoefName,restrict,FXN,DIS, disasg,level,TCH,TCHLvel) %>%
  mutate(Estimate.length_max = max(Estimate.length, na.rm = TRUE)) %>% ungroup() %>% as.data.frame(.)

data <- data[data$Estimate.length_max == data$Estimate.length,]

production <- unique(data[(data$DIS %in% "hnormal" & data$stat %in% "wmean" & data$estType %in% "teBC" & 
                             data$restrict %in% c("Restricted")),])
production$options <- ifelse(production$FXN %in% "CD","Cobb-Douglas production function",NA)
production$options <- ifelse(production$FXN %in% "TL","Translog production function",production$options)
production$options <- ifelse(production$FXN %in% "LN","Linear production function",production$options)
production$options <- ifelse(production$FXN %in% "QD","Quadratic production function",production$options)
production$options <- ifelse(production$FXN %in% "GP","Generalized production function",production$options)
production$options <- ifelse(production$FXN %in% "TP","Transcendental production function",production$options)
production <- production[c("Tech","options","type","Estimate","Estimate.sd","jack_pv")]
production$dimension <- "(A) production"
production

distribution <- unique(data[(data$FXN %in% "TL" & data$stat %in% "wmean" & data$estType %in% "teBC" & 
                               data$restrict %in% c("Restricted")),])
distribution$options <- ifelse(distribution$DIS %in% "hnormal","Half normal distribution",NA)
distribution$options <- ifelse(distribution$DIS %in% "tnormal","Truncated normal distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "tnormal_scaled","Scaled truncated normal distribution with the",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "exponential","Exponential distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "rayleigh","Rayleigh distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "uniform","Uniform distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "gamma","Gamma distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "lognormal","Log normal distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "weibull","Weibull distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "tslaplace","Truncated skewed Laplace distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "genexponential","Generalized exponential distribution",distribution$options)
distribution <- distribution[c("Tech","options","type","Estimate","Estimate.sd","jack_pv")]
distribution$Estimate.sd <- ifelse(distribution$options %in% c("Rayleigh distribution","Truncated normal distribution"),NA,distribution$Estimate.sd)
distribution$dimension <- "(B) distribution"
distribution

efficiency <- unique(data[(data$DIS %in% "hnormal" & data$stat %in% "wmean" & 
                             data$restrict %in% c("Restricted")),])
efficiency$options <- ifelse(efficiency$estType %in% "teJLMS","Jondrow et al. (1982) efficiency",NA)
efficiency$options <- ifelse(efficiency$estType %in% "teBC","Battese and Coelli (1988) efficiency",efficiency$options)
efficiency$options <- ifelse(efficiency$estType %in% "teMO","Conditional model efficiency",efficiency$options)
efficiency <- efficiency[c("Tech","options","type","Estimate","Estimate.sd","jack_pv")]
efficiency$dimension <- "(C) efficiency"
efficiency

tendency <- unique(data[(data$DIS %in% "hnormal" & data$estType %in% "teBC" & 
                           data$restrict %in% c("Restricted")),])
tendency <- tendency[!tendency$stat %in% "mode",]
tendency$options <- ifelse(tendency$stat %in% "wmean","Weighted mean efficiency aggregation",NA)
tendency$options <- ifelse(tendency$stat %in% "mean","Simple mean efficiency aggregation",tendency$options)
tendency$options <- ifelse(tendency$stat %in% "median","Median efficiency aggregation",tendency$options)
#tendency$options <- ifelse(tendency$stat %in% "mode","modal efficiency aggregation",tendency$options)
tendency <- tendency[c("Tech","options","type","Estimate","Estimate.sd","jack_pv")]
tendency$dimension <- "(D) tendency"

Restricted <- unique(data[(data$FXN %in% "TL" & data$DIS %in% "hnormal" & data$stat %in% "wmean" & data$estType %in% "teBC"),])
Restricted$options <- paste0(Restricted$restrict," production function")
Restricted <- Restricted[c("Tech","options","type","Estimate","Estimate.sd","jack_pv")]
Restricted$dimension <- "(F) Production function properties"
Restricted

dataF <- rbind(efficiency,production,distribution,tendency,Restricted)

dataF <- dataF[c("dimension","options","Tech","type","Estimate","Estimate.sd","jack_pv")]

saveRDS(dataF,file=paste0("results/figuresData/robustness.rds"))

wb <- openxlsx::loadWorkbook("results/tech_inefficiency_conflict_results.xlsx")
openxlsx::writeData(wb, sheet = "robustness",dataF , colNames = T)
openxlsx::saveWorkbook(wb,"results/tech_inefficiency_conflict_results.xlsx",overwrite = T)

#------------------------------------
# Fig - Distribution              ####  
rm(list= ls()[!(ls() %in% c(Keep.List))])
dataFrq <- readRDS("results/estimations/CropID_Pooled_index0CAT_TL_hnormal.rds")
dataFrq <- dataFrq$ef_dist
dataFrq <- dataFrq[dataFrq$estType %in% "teBC",]
dataFrq <- dataFrq[dataFrq$Survey %in% "GLSS0",]
dataFrq <- dataFrq[dataFrq$stat %in% "weight",]
dataFrq <- dataFrq[dataFrq$restrict %in% "Restricted",]
dataFrq$Tech <- factor(as.numeric(as.character(dataFrq$TCHLvel)),levels = 0:2,c("Low","Medium","High"))
fig_dsistribution(dataFrq=dataFrq,colset=c("thistle","violet","purple"))
#------------------------------------
