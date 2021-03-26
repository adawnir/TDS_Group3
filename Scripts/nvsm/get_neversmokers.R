# split smoking data
# by ines on march 23

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

bladder.0 <- readRDS("../Results/denoised/bladder.0_denoised.rds")
bladder.1 <- readRDS("../Results/denoised/bladder.1_denoised.rds")

# rename all the smoking variables
names(lung.0)[10] <- "nvsm"
names(lung.1)[2] <- "nvsm"
names(bladder.0)[10] <- "nvsm"
names(bladder.1)[2] <- "nvsm"

names(lung.0)[9] <- "nvsmy"
names(bladder.0)[9] <- "nvsmy"
# there are two never smoker categories. but this is somewhat irrelevant, only exist in null model. so i'll ignore them for now

# rename smoking variables in first adjusted model
names(lung.1)[3] <- "pvsmn"
names(bladder.1)[3] <- "pvsmn"
names(lung.1)[4] <- "pvsmy"
names(bladder.1)[4] <- "pvsmy"
names(lung.1)[5] <- "sm"
names(bladder.1)[5] <- "sm"

lung.0<-rownames_to_column(lung.0,var="eid")
lung.1<-rownames_to_column(lung.1,var="eid")

bladder.0<-rownames_to_column(bladder.0,var="eid")
bladder.1<-rownames_to_column(bladder.1,var="eid")

lung.0$eid <- as.integer(lung.0$eid)
lung.1$eid <- as.integer(lung.1$eid)

bladder.0$eid <- as.integer(bladder.0$eid)
bladder.1$eid <- as.integer(bladder.1$eid)

arr=paste0(rep(c("lung","bladder"),each=2),".nvsm.",0:1)

# only look at neversmokers
lung.nvsm.0<-subset(lung.0,nvsm==1)
lung.nvsm.1<-subset(lung.1,lung.0$nvsm==1) #have to do this differently since denoised

bladder.nvsm.0<-subset(bladder.0,nvsm==1)
bladder.nvsm.1<-subset(bladder.1,bladder.0$nvsm==1)

ifelse(dir.exists("../Results/nvsm_denoised"),"",dir.create("../Results/nvsm_denoised"))
for (i in 1:length(arr)) {
  saveRDS(eval(parse(text=arr[i])),paste0("../Results/nvsm_denoised/",arr[i],"_denoised.rds"))
}
