
rm(list=ls(all=TRUE));gc()
setwd(ifelse(Sys.info()['sysname'] =="Windows","C:/GitHub/GH-Agric-Productivity-Lab",
             paste0("/homes/",Sys.info()['user'],"/Articles/GH/GH_AgricProductivityLab/")))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/figures_and_tables.R"))
setwd(paste0(getwd(),"/replications/tech_inefficiency_land_tenure"))
dir.create("results")
dir.create("results/figures")
dir.create("results/figuresData")
mspecs_optimal <- readRDS("results/mspecs_optimal.rds")
Keep.List<-c("Keep.List",ls())

# Main Specification   
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- tab_main_specification()
wb <- openxlsx::loadWorkbook("results/tech_inefficiency_land_tenure_results.xlsx")
openxlsx::writeData(wb, sheet = "msf",res , colNames = T, startCol = "A", startRow = 1)
openxlsx::saveWorkbook(wb,"results/tech_inefficiency_land_tenure_results.xlsx",overwrite = T)

# Fig - Heterogeneity          
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- readRDS("results/estimations/CropID_Pooled_OwnLnd_TL_hnormal_optimal.rds")$disagscors
res$disasg <- as.character(res$disagscors_var)
res$level <- as.character(res$disagscors_level)
res <- res[res$estType %in% "teBC",]
res <- res[res$Survey %in% "GLSS0",]
res <- res[res$restrict %in% "Restricted",]
res <- res[res$stat %in% "mean",]
res <- res[!res$sample %in% "unmatched",]
res <- res[res$CoefName %in% "disag_efficiencyGap_lvl",]
res <- res[c("disasg","level","FXN","DIS","Survey","input","TCH","Tech","CoefName","Estimate","Estimate.sd","jack_pv")]

fig <- fig_heterogeneity00(res=res,y_title="Level difference (No ownership minus some ownership)\n")
fig[["genderAge"]] <- fig[["genderAge"]] + theme(axis.text.x = element_text(size = 5.5))
ggsave("results/figures/heterogeneity_crop_region.png", fig[["crop_region"]],dpi = 600,width = 8, height = 5)
ggsave("results/figures/heterogeneity_genderAge.png", fig[["genderAge"]],dpi = 600,width = 8, height = 5)

res <- res[(res$disasg %in% c("LndAq","ShrCrpCat")),c("disasg","level","input","Estimate","Estimate.sd","jack_pv")]
wb <- openxlsx::loadWorkbook("results/tech_inefficiency_land_tenure_results.xlsx")
openxlsx::writeData(wb, sheet = "effects_by_right_share",res , colNames = T, startCol = "A", startRow = 1)
openxlsx::saveWorkbook(wb,"results/tech_inefficiency_land_tenure_results.xlsx",overwrite = T)

# Fig - Robustness              
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_robustness(y_title="\nDifference (%) [No ownership minus some ownership]",
               res_list = c("results/estimations/CropID_Pooled_OwnLnd_CD_hnormal_optimal.rds",
                            list.files("results/estimations/",pattern = "CropID_Pooled_OwnLnd_TL_",full.names = T)))

# Fig - Matching TE      
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_input_te(y_title="\nGap associated with land ownership (%)",tech_lable=c("Full sample", "No ownership sample", "some ownership sample"))

# Fig - Covariate balance 
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_covariate_balance()




# Fig - TREND 
ef_mean <- readRDS("results/estimations//CropID_Pooled_OwnLnd_TL_hnormal_optimal.rds")$ef_mean
ef_mean <- ef_mean[ef_mean$stat %in% "wmean", ]
ef_mean <- ef_mean[ef_mean$estType %in% "teBC", ]
ef_mean$estm_type <- "ef_mean"
ef_mean$level_type <- gsub("efficiency", "", ef_mean$CoefName)
ef_mean$level_type <- ifelse(ef_mean$level_type %in% "", "level", ef_mean$level_type)
ef_mean$CoefName <- ef_mean$type
ef_mean <- ef_mean[c("TCH", "FXN", "DIS", "estm_type", "level_type", "sample", "Survey", "restrict", "Tech", "CoefName", "Estimate", "Estimate.sd", "jack_pv")]
ef_mean <- ef_mean[ef_mean$restrict %in% "Restricted", ]
ef_mean <- ef_mean[ef_mean$sample %in% ifelse(mspecs_optimal$link %in% NA,mspecs_optimal$distance,mspecs_optimal$link),]
ef_mean <- ef_mean[ef_mean$level_type %in% "Gap_lvl", ]
ef_mean <- ef_mean[!ef_mean$CoefName %in% "TE0", ]
ef_mean <- ef_mean[!ef_mean$Survey %in% "GLSS0", ]

ef_mean$type <- as.numeric(as.character(factor(ef_mean$CoefName, levels = c("TGR", "TE","MTE"), labels = 1:3)))
ef_mean$type <- factor(ef_mean$type, levels = 1:3,
                     labels = c("Technology gap ratio", "Technical efficiency", "Meta-technical-efficiency"))

ggplot(
  data = ef_mean,
  aes(x = Survey, y = Estimate*100, group = type, fill = type, color = type, shape = type)) +
  geom_point() +
  geom_line() +
  scale_fill_manual("Sample:", values = c("thistle", "violet", "purple")) +
  scale_color_manual("Sample:", values = c("thistle", "violet", "purple")) +
  scale_shape_manual("Sample:", values = c(21, 25, 24, 22, 23, 3, 4, 8, 11)) +
  labs(title = "", x = "", y = "", caption = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 8, colour = "black"),
        axis.text.y = element_text(size = 6, colour = "black"),
        plot.caption = element_text(size = 11, hjust = 0, vjust = 0, face = "italic"),
        strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))









# Fig - Distribution 
dataFrq <- readRDS("results/estimations/CropID_Pooled_credit_hh_TL_hnormal_fullset.rds")
dataFrq <- dataFrq$ef_dist
dataFrq <- dataFrq[dataFrq$estType %in% "teBC",]
dataFrq <- dataFrq[dataFrq$Survey %in% "GLSS0",]
dataFrq <- dataFrq[dataFrq$stat %in% "weight",]
dataFrq <- dataFrq[dataFrq$restrict %in% "Restricted",]
dataFrq$Tech <- factor(as.numeric(as.character(dataFrq$TCHLvel)),levels = 0:1,labels = c("No-Credit","Credit"))
fig_dsistribution(dataFrq)


rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- readRDS("results/estimations/CropID_Pooled_credit_hh_TL_hnormal_optimal.rds")$disagscors
res$disasg <- res$disagscors_var
res$level <- res$disagscors_level
res <- res[res$estType %in% "teBC",]
res <- res[res$Survey %in% "GLSS0",]
res <- res[res$restrict %in% "Restricted",]
res <- res[res$stat %in% "mean",]
res <- res[!res$sample %in% "unmatched",]
res <- res[res$CoefName %in% "disag_efficiencyGap_pct",]
res <- res[res$CoefName %in% "disag_efficiencyGap_pct",]
res <- res[res$input %in% "MTE",]

reg <- res[res$disagscors_var %in% "Region",]
reg <- reg[order(reg$Estimate),]
paste0(paste0(reg$level," (",round(reg$Estimate,2),"%)"),collapse = ", ")

CROP <- res[res$disagscors_var %in% "CROP",]
CROP <- CROP[order(CROP$Estimate),]
paste0(paste0(CROP$level," (",round(CROP$Estimate,2),"%)"),collapse = ", ")

