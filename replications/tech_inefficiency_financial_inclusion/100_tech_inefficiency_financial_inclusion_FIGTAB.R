
rm(list=ls(all=TRUE));gc()
setwd(ifelse(Sys.info()['sysname'] =="Windows","C:/GitHub/GH-Agric-Productivity-Lab",
             paste0("/homes/",Sys.info()['user'],"/Articles/GH/GH_AgricProductivityLab/")))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/figures_and_tables.R"))
setwd(paste0(getwd(),"/replications/tech_inefficiency_financial_inclusion"))
dir.create("results")
dir.create("results/figures")
dir.create("results/figuresData")
mspecs_optimal <- readRDS("results/mspecs_optimal.rds")
Keep.List<-c("Keep.List",ls())

# Main Specification   
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- tab_main_specification()
wb <- openxlsx::loadWorkbook("results/tech_inefficiency_financial_inclusion_results.xlsx")
openxlsx::writeData(wb, sheet = "msf",res , colNames = T, startCol = "A", startRow = 1)
openxlsx::saveWorkbook(wb,"results/tech_inefficiency_financial_inclusion_results.xlsx",overwrite = T)

# Fig - Heterogeneity          
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- readRDS("results/estimations/CropID_Pooled_credit_hh_TL_hnormal_optimal.rds")$disagscors
res$disasg <- as.character(res$disagscors_var)
res$level <- as.character(res$disagscors_level)
res <- res[res$estType %in% "teBC",]
res <- res[res$Survey %in% "GLSS0",]
res <- res[res$restrict %in% "Restricted",]
res <- res[res$stat %in% "mean",]
res <- res[!res$sample %in% "unmatched",]
res <- res[res$CoefName %in% "disag_efficiencyGap_pct",]
res <- res[c("disasg","level","FXN","DIS","Survey","input","TCH","Tech","CoefName","Estimate","Estimate.sd","jack_pv")]

fig <- fig_heterogeneity00(res=res,y_title="Percentage Difference (no-credit less Credit)\n")
fig[["genderAge"]] <- fig[["genderAge"]] + theme(axis.text.x = element_text(size = 5.5))
ggsave("results/figures/heterogeneity_crop_region.png", fig[["crop_region"]],dpi = 600,width = 8, height = 5)
ggsave("results/figures/heterogeneity_genderAge.png", fig[["genderAge"]],dpi = 600,width = 8, height = 5)


data <- res[(res$disasg %in% "FinIdxCat"),]
data$x <- factor(as.numeric(as.character(data$level)),levels = 1:5,
                              labels = c("Very low\n(Bottom 20%)",
                                         "Low\n(20–40%)",
                                         "Medium\n(40–60%)",
                                         "High\n(60–80%%)",
                                         "Very\nhigh (Top 20%)"))
data$input <- factor(data$input, levels = c("TGR","TE","MTE"), labels = c("Technology gap ratio", "Technical efficiency", "Meta-technical-efficiency"))

fig <- ggplot(data = data, aes(x = x, y = Estimate, group = input, shape = input, colour = input, fill = input)) +
  geom_errorbar(aes(ymax = Estimate + Estimate.sd, ymin = Estimate - Estimate.sd), width = 0.25) +
  geom_point(size = 2) +
  labs(title = "", x = "\nQuintile category of the continuous financial inclusion index", 
       y = "Percentage Difference (no-credit less Credit)\n", caption = "") +
  scale_fill_manual(name = "", values = c("thistle", "violet", "purple")) +
  scale_color_manual(name = "", values = c("thistle", "violet", "purple")) +
  scale_shape_manual(name = "", values = c(21, 22, 23, 24, 25, 8, 4)) +
  ers_theme() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, color = "black"),
        axis.title.x = element_text(size = 10, color = "black"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))
ggsave("results/figures/heterogeneity_financial_inclusion.png", fig,dpi = 600,width = 6.8, height = 5)


res <- res[(res$disasg %in% c("Applied","Refused","Accept","Proces","Insured","Banked") | 
              grepl("InstTyp_",res$disasg) | grepl("AccTyp_",res$disasg) | grepl("PrdTyp_",res$disasg) |
              grepl("Source_",res$disasg) | grepl("Collateral_",res$disasg) | grepl("Use_",res$disasg) | 
              grepl("Refusal_",res$disasg) | grepl("Bank_Info_",res$disasg) | grepl("NonBanked_Why_",res$disasg)),]
res <- res[(res$level %in% "1"),]
res <- res[order(res$disasg),]

res$level <- ifelse(res$disasg %in% "NonBanked_Why_1", "Don’t have enough money or income", res$level)
res$level <- ifelse(res$disasg %in% "NonBanked_Why_2", "Don’t have regular income", res$level)
res$level <- ifelse(res$disasg %in% "NonBanked_Why_7", "Not necessary/interested", res$level)
res$level <- ifelse(res$disasg %in% "NonBanked_Why_3", "Financial institutions are too far away", res$level)
res$level <- ifelse(res$disasg %in% "NonBanked_Why_8", "Process cumbersome", res$level)
res$level <- ifelse(res$disasg %in% "NonBanked_Why_6", "Unaware of any", res$level)
res$level <- ifelse(res$disasg %in% "NonBanked_Why_4", "Low or no income", res$level)
res$level <- ifelse(res$disasg %in% "NonBanked_Why_5", "Mistrust", res$level)
res$level <- ifelse(res$disasg %in% "NonBanked_Why_9", "Spouse", res$level)
res$level <- ifelse(res$disasg %in% "NonBanked_Why_10", "Too young", res$level)

res$level <- ifelse(res$disasg %in% "AccTyp_Save", "Savings", res$level)
res$level <- ifelse(res$disasg %in% "AccTyp_Curnt", "Current or cheque", res$level)
res$level <- ifelse(res$disasg %in% "AccTyp_Invst", "Investment", res$level)
res$level <- ifelse(res$disasg %in% "Bank_Info_1", "Colleagues/relatives", res$level)
res$level <- ifelse(res$disasg %in% "Bank_Info_7", "Radio", res$level)
res$level <- ifelse(res$disasg %in% "Bank_Info_8", "Representative from the financial institution", res$level)
res$level <- ifelse(res$disasg %in% "Bank_Info_2", "Community/assoc. leaders", res$level)
res$level <- ifelse(res$disasg %in% "Bank_Info_3", "Employer/union", res$level)
res$level <- ifelse(res$disasg %in% "Bank_Info_10", "Television", res$level)
res$level <- ifelse(res$disasg %in% "Bank_Info_5", "Newspaper/magazine", res$level)
res$level <- ifelse(res$disasg %in% "Bank_Info_4", "Family/friend", res$level)
res$level <- ifelse(res$disasg %in% "Bank_Info_6", "Non-governmental organization (NGO)", res$level)
res$level <- ifelse(res$disasg %in% "Bank_Info_9", "Self", res$level)
res$level <- ifelse(res$disasg %in% "Applied", "Loan applied", res$level)
res$level <- ifelse(res$disasg %in% "Accept", "Loan application Accepted", res$level)
res$level <- ifelse(res$disasg %in% "Refused", "Loan application Rejected", res$level)
res$level <- ifelse(res$disasg %in% "Proces", "Loan application Processing", res$level)
res$level <- ifelse(res$disasg %in% "Banked", "Has bank account/contributing to a scheme ", res$level)
res$level <- ifelse(res$disasg %in% "Insured", "Insured", res$level)

res$level <- ifelse(res$disasg %in% "Refusal_3", "Collateral/Trust", res$level)
res$level <- ifelse(res$disasg %in% "Refusal_6", "Other", res$level)
res$level <- ifelse(res$disasg %in% "Refusal_4", "Inappropriate Purpose", res$level)
res$level <- ifelse(res$disasg %in% "Refusal_2", "Loan Amount Requested Was Too Big", res$level)
res$level <- ifelse(res$disasg %in% "Refusal_5", "Previous Debt Problems", res$level)

res$level <- ifelse(res$disasg %in% "InstTyp_Bank", "Commercial/community/rural bank", res$level)
res$level <- ifelse(res$disasg %in% "InstTyp_Momo", "Mobile money", res$level)
res$level <- ifelse(res$disasg %in% "InstTyp_Susu", "Susu scheme", res$level)
res$level <- ifelse(res$disasg %in% "InstTyp_Save", "Savings and loans Scheme", res$level)
res$level <- ifelse(res$disasg %in% "InstTyp_Coop", "Cooperative/credit Union", res$level)
res$level <- ifelse(res$disasg %in% "InstTyp_Invt", "Investment/mortgage", res$level)
res$level <- ifelse(res$disasg %in% "PrdTyp_Cheq", "Cheque book", res$level)
res$level <- ifelse(res$disasg %in% "PrdTyp_ATM", "ATM card", res$level)
res$level <- ifelse(res$disasg %in% "PrdTyp_Ebnk", "e-banking", res$level)

res$disasg <- ifelse(res$disasg %in% c("Banked","Insured"),"Banking and insurance",res$disasg)
res$disasg <- ifelse(grepl("NonBanked_Why_",res$disasg),"Reasons for no bank account and contributing to a loan/savings scheme",res$disasg)
res$disasg <- ifelse(grepl("InstTyp_",res$disasg),"Types of financial institution with accounts or contribution",res$disasg)
res$disasg <- ifelse(grepl("AccTyp_",res$disasg),"Type of account held in the financial institution",res$disasg)
res$disasg <- ifelse(grepl("PrdTyp_",res$disasg),"Transaction products utilized",res$disasg)
res$disasg <- ifelse(grepl("Bank_Info_",res$disasg),"Source of financial institution knowledge",res$disasg)

res$disasg <- ifelse(res$disasg %in% c("Applied","Refused","Accept","Proces"),"Loan application outcomes",res$disasg)
res$disasg <- ifelse(grepl("Source_",res$disasg),"Source of loan",res$disasg)
res$disasg <- ifelse(grepl("Use_",res$disasg),"Purpose for loan",res$disasg)
res$disasg <- ifelse(grepl("Collateral_",res$disasg),"Guarantee/collateral",res$disasg)
res$disasg <- ifelse(grepl("Refusal_",res$disasg),"Reson for refusal",res$disasg)
res$disasg <- ifelse(grepl("WhyNoLoan_",res$disasg),"Reason for not aplying a loan",res$disasg)

# res <- res[c("disasg","level","input","Estimate","Estimate.sd","jack_pv")]
# wb <- openxlsx::loadWorkbook("results/tech_inefficiency_financial_inclusion_results.xlsx")
# openxlsx::writeData(wb, sheet = "msf",res , colNames = T, startCol = "A", startRow = 1)
# openxlsx::saveWorkbook(wb,"results/tech_inefficiency_financial_inclusion_results.xlsx",overwrite = T)


# Fig - Robustness              
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_robustness(y_title="\nDifference (%) [no-credit less Credit]",
               res_list = c("results/estimations/CropID_Pooled_credit_hh_CD_hnormal_optimal.rds",
                            list.files("results/estimations/",pattern = "CropID_Pooled_credit_hh_TL_",full.names = T)))

# Fig - Matching TE      
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_input_te(y_title="\nGap associated with having credit (%)",tech_lable=c("Full sample", "No-credit sample", "Credit sample"))

# Fig - Covariate balance 
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_covariate_balance()

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

