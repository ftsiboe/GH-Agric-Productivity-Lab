rm(list=ls(all=TRUE));gc()
setwd(ifelse(Sys.info()['sysname'] =="Windows",paste0("C:/Users/",Sys.info()['user'],"/Documents/GitHub/GH-Agric-Productivity-Lab"),
             paste0("/homes/",Sys.info()['user'],"/Articles/GH/GH_AgricProductivityLab/")))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/helpers_tech_inefficiency.R"))
setwd(paste0(getwd(),"/replications/tech_inefficiency_land_tenure"))
dir.create("results")
dir.create("results/estimations")

DATA <- Fxn_DATA_Prep(as.data.frame(haven::read_dta("data/tech_inefficiency_land_tenure_data.dta")))

DATA$LndOwn <- as.integer(DATA$LndOwn)
DATA$OwnLnd <- as.integer(DATA$OwnLnd)
DATA$ShrCrpCat <- as.integer(DATA$ShrCrpCat)
DATA$LndRgt <- as.integer(DATA$LndRgt)
DATA$LndAq <- as.integer(DATA$LndAq)

FXNFORMS  <- Fxn_SF_forms()$FXNFORMS
DISTFORMS <- Fxn_SF_forms()$DISTFORMS

DATA$LndRgt <- ifelse(DATA$LndRgt == min(DATA$LndRgt,na.rm=T) & DATA$OwnLnd > min(DATA$OwnLnd,na.rm=T),NA,DATA$LndRgt)
table(DATA$OwnLnd,DATA$LndRgt)

function(){

  mainD <- 1
  mainF <- 2
  
  SPECS <- Fxn_SPECS(TechVarlist=c("OwnLnd","LndOwn","LndRgt"),  
                     mainD = mainD, mainF=mainF)
 
  SPECS <- rbind(
    data.frame(SPECS[ (SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% "OwnLnd" & SPECS$level %in% "Pooled"),], nnm="fullset"),
    data.frame(SPECS[ (SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% "OwnLnd" & SPECS$level %in% "Pooled"),], nnm="optimal"),
    data.frame(SPECS[!(SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% "OwnLnd" & SPECS$level %in% "Pooled"),], nnm="optimal"))
  
  SPECS <- SPECS[!(SPECS$disasg %in% c("CropID") & !SPECS$level %in% "Pooled"),]
  SPECS <- SPECS[!SPECS$disasg %in% c( "Female","Region","Ecozon","EduCat","EduLevel","AgeCat"),]
  
  SPECS <- SPECS[!(paste0(SPECS$disasg,"_",SPECS$level,"_",SPECS$TechVar,"_",names(FXNFORMS)[SPECS$f],"_",
                          names(DISTFORMS)[SPECS$d],"_",SPECS$nnm,".rds") %in% list.files("results/estimations/")),]
  
  row.names(SPECS) <- 1:nrow(SPECS)
  
  saveRDS(SPECS,file="results/SPECS.rds")
}

SPECS <- readRDS("results/SPECS.rds")

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  SPECS <- SPECS[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}

lapply(
  c(1:nrow(SPECS)),
  function(fit){
    # fit <- 2
    f <- SPECS$f[fit]
    d <- SPECS$d[fit]
    disasg <- SPECS$disasg[fit]
    level <- SPECS$level[fit]
    TechVar <- SPECS$TechVar[fit]
    nnm <- SPECS$nnm[fit]
    # nnm <- "optimal"
    if(!paste0(disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d],"_",nnm,".rds") %in% list.files("results/estimations/")){
      #tryCatch({ 
      
      # Data Preparation
      data <- DATA[DATA[,SPECS$disasg[fit]] %in% SPECS$level[fit],]
      data <- data[!data[,SPECS$TechVar[fit]] %in% NA,]
      data$Tech <- as.numeric(as.integer(as.factor(as.character(data[,SPECS$TechVar[fit]]))))
      if(!SPECS$disasg[fit] %in% "CropID") data <- data[data[,"CropID"] %in% "Pooled",]
      TechKey <- unique(data[c("Tech",SPECS$TechVar[fit])])
      TechKey <- TechKey[order(TechKey$Tech),]
      
      for(crop in c(c("Beans","Cassava","Cocoa","Cocoyam","Other","Millet","Okra","Palm","Peanut",
                      "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam","Maize"))){
        data[,paste0("CROP_",crop)] <- ifelse(data[,paste0("Area_",crop)] > 0, crop,NA)
      }
      
      ArealistX <- names(data)[grepl("Area_",names(data))]
      ArealistX <- ArealistX[ArealistX %in% paste0("Area_",c("Beans","Cassava","Cocoa","Cocoyam","Other","Millet","Okra","Palm","Peanut",
                                                             "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam","Other"))]
      
      ArealistX <- apply(data[names(data)[names(data) %in% ArealistX]],2,mean) > 0.03
      ArealistX <- names(ArealistX)[ArealistX %in% TRUE]
      if(length(ArealistX)>0){ 
        data$Area_Other <- 1 - rowSums(data[c(ArealistX[!ArealistX %in% "Area_Other"],"Area_Maize")],na.rm=T)
        ArealistX <- unique(c(ArealistX,"Area_Other"))
      }
      
      # draw estimations
      drawlist = readRDS("results/drawlist.rds")
      if(nnm %in% "fullset") drawlist <- drawlist[drawlist$ID<=50,]
 
      disagscors_list <- NULL
      
      if(TechVar %in% "OwnLnd" &  nnm %in% "optimal" & level %in% "Pooled" & disasg %in% "CropID" & f %in% 2 & d %in% 1){
        disagscors_list <- c("Ecozon","Region","AgeCat","EduLevel","Female",
                             names(data)[grepl("CROP_",names(data))],"LndAq","ShrCrpCat")
        disagscors_list <- unique(disagscors_list[disagscors_list %in% names(data)])
      }
      
      res <- lapply(
        unique(drawlist$ID),Fxn_draw_estimations,
        data = data,
        surveyy  = TRUE,
        intercept_shifters  = list(Svarlist=ArealistX,Fvarlist=c("Ecozon")),
        intercept_shiftersM = list(Svarlist=NULL,Fvarlist=c("Ecozon")),
        drawlist = drawlist,
        wvar = "Weight",
        yvar = "HrvstKg",
        xlist = c("Area", "SeedKg", "HHLaborAE","HirdHr","FertKg","PestLt"),
        ulist = list(Svarlist=c("lnAgeYr","lnYerEdu","CrpMix"),Fvarlist=c("Female","Ecozon","Extension","Credit","EqipMech")),
        ulistM= list(Svarlist=c("lnAgeYr","lnYerEdu","CrpMix"),Fvarlist=c("Female","Ecozon","Extension","Credit","EqipMech")),
        UID   = c("UID", "Survey", "CropID", "HhId", "EaId", "Mid"),
        disagscors_list   = disagscors_list,
        f     = f,
        d     = d,
        tvar  = TechVar,
        nnm   = nnm) 
      
      # resX <- res
      # resX[["names"]] <- paste0(disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d],"_",nnm)
      # saveRDS(resX,file=paste0("Results/boots/",disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d],"_",nnm,".rds"))
      
      # draw summary [START FROM HERE]
      res <- Fxn.draw_summary(res=res,TechKey=TechKey)
      
      for(xx in 1:length(res)){
        tryCatch({
          res[[xx]][,"FXN"]     <- names(FXNFORMS)[f]
          res[[xx]][,"DIS"]     <- names(DISTFORMS)[d]
          res[[xx]][,"disasg"]  <- disasg
          res[[xx]][,"level"]   <- level
          res[[xx]][,"TCH"]     <- TechVar
          res[[xx]][,"TCHLvel"] <- factor(res[[xx]][,"Tech"],levels = c(-999,TechKey$Tech,999),labels = c("National",TechKey[,2],"Meta"))
        }, error=function(e){})
      }
      
      function(){
        Main <- res$ef_mean
        Main <- Main[Main$Survey %in% "GLSS0",]
        Main <- Main[!Main$sample %in% "unmatched",]
        Main <- Main[Main$stat %in% "wmean",]
        Main <- Main[Main$CoefName %in% "efficiencyGap_pct",]
        Main <- Main[Main$restrict %in% "Restricted",]
        Main <- Main[Main$estType %in% "teBC",]
        Main[Main$type %in% "TGR",c("sample","type","Tech","Estimate")]
        Main[Main$type %in% "TE",c("sample","type","Tech","Estimate")]
        Main[Main$type %in% "MTE",c("sample","type","Tech","Estimate")]
      }
      
      res[["names"]] <- paste0(disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d],"_",nnm)
      
      if(!(TechVar %in% "OwnLnd" & nnm %in% "optimal" & level %in% "Pooled" & disasg %in% "CropID" & f %in% 2 & d %in% 1)){
        res$rk_dist <- NULL
        res$rk_mean <- NULL
        res$rk_samp <- NULL
        res$el_samp <- NULL
        res$ef_samp <- NULL 
      }
      
      saveRDS(res,file=paste0("results/estimations/",disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d],"_",nnm,".rds"))
      
      #}, error=function(e){})
    }
    return(fit)
  })

# unlink(list.files(getwd(),pattern =paste0(".out"),full.names = T))

