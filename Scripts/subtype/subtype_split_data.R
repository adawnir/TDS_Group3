# break up denoised files into site chunks

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

## Load packages
LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}

LoadPackages(c("tidyverse","data.table","dplyr","tibble"))

lung.0 <- readRDS("../Results/denoised/lung.0_denoised.rds")
lung.1 <- readRDS("../Results/denoised/lung.1_denoised.rds")
lung.2 <- readRDS("../Results/denoised/lung.2_denoised.rds")
subtype=readRDS("../Results/lung_subtype.rds")

lung.0<-rownames_to_column(lung.0,var="eid")
lung.1<-rownames_to_column(lung.1,var="eid")
lung.2<-rownames_to_column(lung.2,var="eid")

lung.0$eid <- as.integer(lung.0$eid)
lung.1$eid <- as.integer(lung.1$eid)
lung.2$eid <- as.integer(lung.2$eid)

arr0=paste0(rep("lung.",3),0:2)
arr=paste0(rep(c("insitu.lung","insitu.trachea","mal.trachea","mal.lung","mal.lower","mal.main","mal.middle","mal.overlap","mal.upper"),each=3),".",0:2)

lung.0 <- left_join(lung.0,subtype, by="eid")
lung.1 <- left_join(lung.1,subtype, by="eid")
lung.2 <- left_join(lung.2,subtype, by="eid")

# separate by subtype
subtypes <- levels(subtype$subtype)
levels(subtype$subtype) <- c("insitu.lung","insitu.trachea","mal.trachea","mal.lung","mal.lower","mal.main","mal.middle","mal.overlap","mal.upper")

insitu.lung.0=subset(lung.0, subtype==subtypes[1])
insitu.lung.1=subset(lung.1, subtype==subtypes[1])
insitu.lung.2=subset(lung.2, subtype==subtypes[1])

insitu.trachea.0=subset(lung.0, subtype==subtypes[2]) # no trachea cases
insitu.trachea.1=subset(lung.1, subtype==subtypes[2]) # no trachea cases
insitu.trachea.2=subset(lung.2, subtype==subtypes[2]) # no trachea cases

mal.trachea.0=subset(lung.0, subtype==subtypes[3])
mal.trachea.1=subset(lung.1, subtype==subtypes[3])
mal.trachea.2=subset(lung.2, subtype==subtypes[3])

mal.lung.0=subset(lung.0, subtype==subtypes[4])
mal.lung.1=subset(lung.1, subtype==subtypes[4])
mal.lung.2=subset(lung.2, subtype==subtypes[4])

mal.lower.0=subset(lung.0, subtype==subtypes[5])
mal.lower.1=subset(lung.1, subtype==subtypes[5])
mal.lower.2=subset(lung.2, subtype==subtypes[5])

mal.main.0=subset(lung.0, subtype==subtypes[6])
mal.main.1=subset(lung.1, subtype==subtypes[6])
mal.main.2=subset(lung.2, subtype==subtypes[6])

mal.middle.0=subset(lung.0, subtype==subtypes[7])
mal.middle.1=subset(lung.1, subtype==subtypes[7])
mal.middle.2=subset(lung.2, subtype==subtypes[7])

mal.overlap.0=subset(lung.0, subtype==subtypes[8])
mal.overlap.1=subset(lung.1, subtype==subtypes[8])
mal.overlap.2=subset(lung.2, subtype==subtypes[8])

mal.upper.0=subset(lung.0, subtype==subtypes[9])
mal.upper.1=subset(lung.1, subtype==subtypes[9])
mal.upper.2=subset(lung.2, subtype==subtypes[9])

# i'm going to save all of them
# JOIN WITH ALL controls
controls0 <- subset(lung.0, case_status=="0")
controls1 <- subset(lung.1, case_status=="0")
controls2 <- subset(lung.2, case_status=="0")

insitu.lung.0<-rbind(insitu.lung.0,controls0)
insitu.lung.1<-rbind(insitu.lung.1,controls1)
insitu.lung.2<-rbind(insitu.lung.2,controls2)

insitu.trachea.0<-rbind(insitu.trachea.0,controls0)
insitu.trachea.1<-rbind(insitu.trachea.1,controls1)
insitu.trachea.2<-rbind(insitu.trachea.2,controls2)

mal.trachea.0<-rbind(mal.trachea.0,controls0)
mal.trachea.1<-rbind(mal.trachea.1,controls1)
mal.trachea.2<-rbind(mal.trachea.2,controls2)

mal.lung.0<-rbind(mal.lung.0,controls0)
mal.lung.1<-rbind(mal.lung.1,controls1)
mal.lung.2<-rbind(mal.lung.2,controls2)

mal.lower.0<-rbind(mal.lower.0,controls0)
mal.lower.1<-rbind(mal.lower.1,controls1)
mal.lower.2<-rbind(mal.lower.2,controls2)

mal.main.0<-rbind(mal.main.0,controls0)
mal.main.1<-rbind(mal.main.1,controls1)
mal.main.2<-rbind(mal.main.2,controls2)

mal.middle.0<-rbind(mal.middle.0,controls0)
mal.middle.1<-rbind(mal.middle.1,controls1)
mal.middle.2<-rbind(mal.middle.2,controls2)

mal.overlap.0<-rbind(mal.overlap.0,controls0)
mal.overlap.1<-rbind(mal.overlap.1,controls1)
mal.overlap.2<-rbind(mal.overlap.2,controls2)

mal.upper.0<-rbind(mal.upper.0,controls0)
mal.upper.1<-rbind(mal.upper.1,controls1)
mal.upper.2<-rbind(mal.upper.2,controls2)

ifelse(dir.exists("../Results/strat_site_denoised"),"",dir.create("../Results/strat_site_denoised"))
for (i in 1:length(arr)) {
  saveRDS(eval(parse(text=arr[i])),paste0("../Results/strat_site_denoised/",arr[i],"_denoised.rds"))
}
