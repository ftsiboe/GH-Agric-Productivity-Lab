#------------------------------------
# Preliminaries                   ####
rm(list=ls(all=TRUE));gc()
setwd(ifelse(Sys.info()['sysname'] =="Windows",getwd(),"/homes/ftsiboe/Articles/GH/GH_AgricProductivityLab/"))
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
# Fig - Distribution              ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
dataFrq <- readRDS("results/estimations/CropID_Pooled_index0CAT_TL_hnormal.rds")$dataFrq
#dataFrq <- dataFrq[dataFrq$sample %in% c("cloglog","unmatched"),]
dataFrq <- dataFrq[dataFrq$estType %in% "teBC",]
dataFrq <- dataFrq[dataFrq$Survey %in% "GLSS0",]
dataFrq$Tech <- factor(as.numeric(as.character(dataFrq$TCHLvel)),levels = 0:2,labels = c("None","Low","High"))

dataFrq$input <- ifelse(dataFrq$input %in% "(ii) Technical efficiency [group]","(i) Technical efficiency",dataFrq$input)
dataFrq$input <- ifelse(dataFrq$input %in% "(iii) Technology gap ratio","(ii) Technology gap ratio",dataFrq$input)
dataFrq$input <- ifelse(dataFrq$input %in% "(iv) Meta-technical-efficiency","(iii) Meta-technical-efficiency",dataFrq$input)
dataFrq$input <- ifelse(dataFrq$input %in% "(i) Technical efficiency [national]",
                        "(iv) Technical efficiency [Naïve]",dataFrq$input)

dataFrq$Survey <- ifelse(dataFrq$Survey %in% "GLSS6","(A) 2012/2013",dataFrq$Survey)
dataFrq$Survey <- ifelse(dataFrq$Survey %in% "GLSS7","(B) 2016/17",dataFrq$Survey)
dataFrq$Survey <- ifelse(dataFrq$Survey %in% "GLSS0","(C) Mean of A and B",dataFrq$Survey)

dataFrq$sample <- ifelse(dataFrq$sample %in% "unmatched","Unmatched",dataFrq$sample)
dataFrq$sample <- ifelse(dataFrq$sample %in% "logit","Logit [PS]",dataFrq$sample)
dataFrq$sample <- ifelse(dataFrq$sample %in% "cauchit","Cauchit [PS]",dataFrq$sample)
dataFrq$sample <- ifelse(dataFrq$sample %in% "probit","Probit [PS]",dataFrq$sample)
dataFrq$sample <- ifelse(dataFrq$sample %in% "cloglog","Complementary\nLog-Log [PS]",dataFrq$sample)
dataFrq$sample <- ifelse(dataFrq$sample %in% "euclidean","Euclidean",dataFrq$sample)
dataFrq$sample <- ifelse(dataFrq$sample %in% "robust_mahalanobis","Robust\nMahalanobis",dataFrq$sample)
dataFrq$sample <- ifelse(dataFrq$sample %in% "scaled_euclidean","Scaled\nEuclidean",dataFrq$sample)
dataFrq$sample <- ifelse(dataFrq$sample %in% "mahalanobis","Mahalanobis",dataFrq$sample)

xlabs <- unique(dataFrq[c("range","Frqlevel")])
xlabs <- xlabs[xlabs$Frqlevel %in% seq(1,20,5),]

Fig <- ggplot(data=dataFrq,aes(x=Frqlevel,y=est_weight, fill = Tech,color=Tech,shape=Tech,group=Tech)) +
  geom_bar(stat="identity",position="stack") +
  #geom_density(stat="identity",position="jitter",alpha=0.3)+
  #geom_errorbar(aes(x=Frqlevel,ymax = est_weight + est_weight.sd*1.96, ymin = est_weight - est_weight.sd*1.96), width = 0.25,colour="blue") +
  facet_wrap(~ input  , scales = "free_y") +
  scale_fill_manual(name="Education exposure:",values = c("thistle","violet","purple")) +
  scale_color_manual(name="Education exposure:",values = c("thistle","violet","purple")) +
  scale_shape_manual(name="",values = c(21,22,23,24,25,8,4)) +
  scale_x_continuous(breaks = xlabs$Frqlevel,labels = xlabs$range) +
  labs(title= "", x = "", y = "", caption = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=11),
        legend.title=element_text(size=11),
        axis.title.y=element_text(size=11),
        axis.title.x=element_text(size=11),
        axis.text.x = element_text(size = 7), #
        axis.text.y = element_text(size=6),
        plot.caption = element_text(size=11,hjust = 0 ,vjust = 0, face = "italic"),
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))
Fig

if(Sys.info()['sysname'] =="Windows"){ggsave("Results/Figs/score_distributions.png", Fig,dpi = 600,width = 6, height = 7)}
if(Sys.info()['sysname'] !="Windows"){ggsave("Results/Figs/score_distributions.pdf", Fig,dpi = 600,width = 6, height = 7)}

#------------------------------------
# Fig - Heterogeneity             ####

rm(list= ls()[!(ls() %in% c(Keep.List))])
source("ers_theme.R")

list.files("results/estimations/",pattern = "index0CAT",full.names = T)

res <- list.files("results/estimations/",pattern = "index0CAT_TL_hnormal",full.names = T)

res <- as.data.frame(
  data.table::rbindlist(
    lapply(
      res,
      function(file) {
        DONE <- NULL
        tryCatch({ 
          # file <- res[1]
          res <- readRDS(file)$Estescors
          res <- res[res$stat %in% "wmean",]
          res <- res[res$Survey %in% "GLSS0",]
          res <- res[res$CoefName %in% "efficiencyGap_pct",]
          res <- res[res$estType %in% "teBC",]
          # res <- res[res$sample %in% "probit",]
          res <- res[c("sample","disasg","level","FXN","DIS","Survey","input","TCH","Tech","CoefName","Estimate","Estimate.sd","draw_pv")]
          DONE <- res
        }, error=function(e){})
        return(DONE)
      }), fill = TRUE))

res <- rbind(res[(res$sample %in% "unmatched" & res$disasg %in% c("Ecozon","Female","AgeCat","EduLevel")),],
             res[(res$sample %in% "unmatched" & res$disasg %in% c("CropID","Region")),])
res <- res[!res$level %in% "Pooled",]
res$level <- ifelse(res$disasg %in% "AgeCat" & res$level == "1","Farmer aged\n35 or less",res$level)
res$level <- ifelse(res$disasg %in% "AgeCat" & res$level == "2","Farmer aged\n36 to 59",res$level)
res$level <- ifelse(res$disasg %in% "AgeCat" & res$level == "3","Farmer aged\n60 or more",res$level)

res$level <- ifelse(res$disasg %in% "Female" & res$level == "1","Female\nfarmer",res$level)
res$level <- ifelse(res$disasg %in% "Female" & res$level == "0","Male\nfarmer",res$level)

res$level <- ifelse(res$disasg %in% "EduLevel" & res$level == "0","Farmer with\nno formal\neducation",res$level)
res$level <- ifelse(res$disasg %in% "EduLevel" & res$level == "1","Farmer with\nprimary education",res$level)
res$level <- ifelse(res$disasg %in% "EduLevel" & res$level == "2","Farmer with\njunior secondary\nschool education",res$level)
res$level <- ifelse(res$disasg %in% "EduLevel" & res$level == "3","Farmer with\nsenior secondary\nschool education",res$level)
res$level <- ifelse(res$disasg %in% "EduLevel" & res$level == "4","Farmer with\npost senior\nsecondary school\neducation",res$level)

eff_fig_fxn <- function(disasg,type=NULL,xsize=7,title=""){
  # disasg <- c("AgeCat","Female");type<-"farmer"
  data   <- unique(rbind(res[(res$disasg %in% "CropID" & res$level %in% "Pooled"),],res[res$disasg %in% disasg,]))
  myrank <- data[data$input %in% "MTE",]
  myrank <- myrank[myrank$Tech %in% min(data$Tech,na.rm=T),]
  
  if("farmer" %in% type){
    myrank <- myrank[order(myrank$level),]
    myrank <- rbind(myrank[myrank$level %in% "Pooled",c("disasg","level","FXN","DIS","Survey","TCH")],
                    myrank[myrank$disasg %in% "Female",c("disasg","level","FXN","DIS","Survey","TCH")],
                    myrank[myrank$disasg %in% "AgeCat",c("disasg","level","FXN","DIS","Survey","TCH")],
                    myrank[myrank$level %in% "Farmer with\nno formal\neducation",c("disasg","level","FXN","DIS","Survey","TCH")],
                    myrank[myrank$level %in% "Farmer with\nprimary education",c("disasg","level","FXN","DIS","Survey","TCH")],
                    myrank[myrank$level %in% "Farmer with\njunior secondary\nschool education",c("disasg","level","FXN","DIS","Survey","TCH")],
                    myrank[myrank$level %in% "Farmer with\nsenior secondary\nschool education",c("disasg","level","FXN","DIS","Survey","TCH")],
                    myrank[myrank$level %in% "Farmer with\npost senior\nsecondary school\neducation",c("disasg","level","FXN","DIS","Survey","TCH")])
  }
  if(is.null(type)){
    myrank <- myrank[order(myrank$Estimate),c("disasg","level","FXN","DIS","Survey","TCH")]
    myrank <- rbind(myrank[myrank$level %in% "Pooled",],myrank[!myrank$level %in% "Pooled",])
  }
  myrank$x1 <- 1:nrow(myrank)
  data <- dplyr::inner_join(myrank,data,by=names(myrank)[names(myrank) %in% names(data)])
  data$x2<-ifelse(data$input =="TGR",1,NA)
  data$x2<-ifelse(data$input =="TE",2,data$x2)
  data$x2<-ifelse(data$input =="MTE",3,data$x2)
  data$x <- as.integer(as.factor(paste0(stringr::str_pad(data$x1,pad="0",3),stringr::str_pad(data$x2,pad="0",3))))
  data <- data[order(data$x),]
  myrank <- unique(data[c("x2","x","x1","input","level","disasg")])
  myrank_lines <- data[data$input %in% "MTE",]
  myrank <- data[data$input %in% "TE",]
  
  data$input<- factor(data$x2,levels = 1:3,labels = c("Technology gap ratio","Technical efficiency","Meta-technical-efficiency"))
  data$Tech <- factor(as.numeric(as.character(data$Tech)),levels = 1:3,labels = c("None","Low","High"))
  ggplot(data=data,aes(x = x,y=Estimate ,group=Tech,shape=Tech,colour=input,fill=input)) +
    geom_vline(xintercept=myrank_lines$x[1:(nrow(myrank_lines)-1)]+0.5, lwd=0.5, lty=5,color = "#808080") +
    geom_errorbar(aes(ymax = Estimate + Estimate.sd, ymin = Estimate - Estimate.sd), width = 0.25) +
    geom_point(size=2.5) + 
    #facet_wrap(~ input,ncol=1,scales = "free_y") +
    scale_x_continuous(breaks = myrank$x,labels = myrank$level) +
    labs(title=title,x="", y ="",caption = "") +
    scale_fill_manual(name="Score:",values = c("thistle","violet","purple")) +
    scale_color_manual(name="Score:",values = c("thistle","violet","purple")) +
    scale_shape_manual(name="Peace and social cohesion:",values = c(21,22,23,24,25,8,4)) +
    ers_theme() +
    theme(axis.title= element_text(size=9,color="black"),
          plot.title  = element_text(size = 8),
          axis.text.y = element_text(size = 7),
          axis.text.x = element_text(size = xsize), #
          axis.title.y= element_text(size=8,color="black"),
          legend.position="none",
          legend.title=element_text(size=7),
          legend.text=element_text(size=7),
          plot.caption = element_text(size=8),
          strip.text = element_text(size = 10),
          strip.background = element_rect(fill = "white", colour = "black", size = 1))
  
}

grobs <- ggplotGrob(eff_fig_fxn(disasg = "CropID",xsize=5.5,title="(A) Major crops")+theme(legend.position="bottom"))$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
Ylab<-ggplot()+geom_text(aes(x=0,y=0),label="Percentage Difference (Disabled less Nondisabled)\n",size=3,angle=90)+theme_void()

marg <- c(0.05,0.5,-0.5,0.5)
fig.CropID   <- eff_fig_fxn(disasg = "CropID",xsize=5.5,title="(A) Major crops")
fig.Location <- eff_fig_fxn(disasg = "Region",title="(B) Administrative regions")
fig <- cowplot::plot_grid(
  fig.CropID + theme(plot.margin = unit(marg,"cm"))  ,
  fig.Location + theme(plot.margin = unit(marg,"cm")) ,
  ncol=1, align="v",rel_heights=c(1,1),
  greedy=F)
fig <- cowplot::plot_grid(fig,legend,ncol=1,rel_heights=c(1,0.1))
fig <- cowplot::plot_grid(Ylab,fig,nrow=1,rel_widths =c(0.002,0.03))
if(Sys.info()['sysname'] =="Windows"){ggsave("Results/Figs/heterogeneity_crop_region.png", fig,dpi = 600,width = 8, height = 5)}
if(Sys.info()['sysname'] !="Windows"){ggsave("Results/Figs/heterogeneity_crop_region.pdf", fig,dpi = 600,width = 8, height = 5)}

fig.Region   <- eff_fig_fxn(disasg = c("Region"),xsize=7) +
  labs(title="",x="", y ="Percentage Difference (Disabled less Nondisabled)\n",caption = "") +
  theme(legend.position="bottom")
if(Sys.info()['sysname'] =="Windows"){ggsave("Results/Figs/heterogeneity_Region.png", fig.Region,dpi = 600,width = 8, height = 5)}
if(Sys.info()['sysname'] !="Windows"){ggsave("Results/Figs/heterogeneity_Region.pdf", fig.Region,dpi = 600,width = 8, height = 5)}

fig.Farmer   <- eff_fig_fxn(disasg = c("AgeCat","Female","EduLevel"),
                            type="farmer",xsize=6) +
  labs(title="",x="", y ="Percentage Difference (Disabled less Nondisabled)\n",caption = "") +
  theme(legend.position="bottom")
if(Sys.info()['sysname'] =="Windows"){ggsave("Results/Figs/heterogeneity_genderAge.png", fig.Farmer,dpi = 600,width = 8, height = 5)}
if(Sys.info()['sysname'] !="Windows"){ggsave("Results/Figs/heterogeneity_genderAge.pdf", fig.Farmer,dpi = 600,width = 8, height = 5)}

fig.Crop   <- eff_fig_fxn(disasg = c("CropID"),xsize=7) +
  labs(title="",x="", y ="Percentage Difference (Disabled less Nondisabled)\n",caption = "") +
  theme(legend.position="bottom")
if(Sys.info()['sysname'] =="Windows"){ggsave("Results/Figs/heterogeneity_Crop.png", fig.Crop,dpi = 600,width = 8, height = 5)}
if(Sys.info()['sysname'] !="Windows"){ggsave("Results/Figs/heterogeneity_Crop.pdf", fig.Crop,dpi = 600,width = 8, height = 5)}

#------------------------------------


#------------------------------------
#------------------------------------
#------------------------------------
#------------------------------------
#------------------------------------
#------------------------------------
# Main Specification   
# rm(list= ls()[!(ls() %in% c(Keep.List))])
# res <- tab_main_specification()
# wb <- openxlsx::loadWorkbook("results/tech_inefficiency_conflict_results.xlsx")
# openxlsx::writeData(wb, sheet = "msf",res , colNames = T, startCol = "A", startRow = 1)
# openxlsx::saveWorkbook(wb,"results/tech_inefficiency_disability_results.xlsx",overwrite = T)

# Fig - Heterogeneity          
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig <- fig_heterogeneity00(res=readRDS("results/estimations/CropID_Pooled_index0CAT_TL_hnormal.rds")$disagscors,
                    y_title="Percentage Difference (Disabled less non-Disabled)\n")
fig[["genderAge"]] <- fig[["genderAge"]] + theme(axis.text.x = element_text(size = 5.5))
ggsave("results/figures/heterogeneity_crop_region.png", fig[["crop_region"]],dpi = 600,width = 8, height = 5)
ggsave("results/figures/heterogeneity_genderAge.png", fig[["genderAge"]],dpi = 600,width = 8, height = 5)

# Fig - Robustness              
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_robustness(y_title="\nDifference (%) [Disabled less non-Disabled]",
               res_list = c("results/estimations/CropID_Pooled_disabled_CD_hnormal_optimal.rds",
                            list.files("results/estimations/",pattern = "CropID_Pooled_disabled_TL_",full.names = T)))

# Fig - Matching TE      
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_input_te(y_title="\nEducation gap (%)",tech_lable=c("Full sample", "Disabled sample", "non-Disabled sample"))

# Fig - Covariate balance 
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_covariate_balance()

# Fig - Distribution 
dataFrq <- readRDS("results/estimations/CropID_Pooled_disabled_TL_hnormal_fullset.rds")
dataFrq <- dataFrq$ef_dist
dataFrq <- dataFrq[dataFrq$estType %in% "teBC",]
dataFrq <- dataFrq[dataFrq$Survey %in% "GLSS0",]
dataFrq <- dataFrq[dataFrq$stat %in% "weight",]
dataFrq <- dataFrq[dataFrq$restrict %in% "Restricted",]
dataFrq$Tech <- factor(as.numeric(as.character(dataFrq$TCHLvel)),levels = 0:1,labels = c("non-Disabled","Disabled"))
fig_dsistribution(dataFrq)


rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- readRDS("results/estimations/CropID_Pooled_disabled_TL_hnormal_optimal.rds")$disagscors
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










