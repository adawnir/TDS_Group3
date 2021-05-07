### TDS Project -- Stratify denoised data
## Programme created by Rin Wada on 21 March
# Edited by Fergal to run for education stratification

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

## Load packages
library(tidyverse)

## Load data
for (m in 1:4){
  arr=paste0(rep(c("lung","bladder"),each=2),".",1:2)[m]
  dat = readRDS(paste0("../Results/denoised/",arr,"_denoised.rds"))
  assign(arr,dat) # Assign name
}

lung=readRDS("../Results/multivar_data_lung.rds")
bladder=readRDS("../Results/multivar_data_bladder.rds")

# Recode case/control status
lung$case_status = as.factor(ifelse(as.character(lung$case_status) == "control",0,1))
bladder$case_status = as.factor(ifelse(as.character(bladder$case_status) == "control",0,1))

for (m in 1:4){
  arr=paste0(rep(c("lung","bladder"),each=2),".",1:2)[m] ### Change name for other stratification
  dat_name=rep(c("lung","bladder"),each=2)[m]
  original = eval(parse(text = dat_name))
  denoised = eval(parse(text = arr))
  # Subsample data sets: adjust case size to smaller data set (high ed and low ed)
  original.l=original[original$education=="Low",]
  original.h=original[original$education=="High",]
  set.seed(100)
  s1=sample(which(original.l$case_status=="1"),
            size=sum(original.l$case_status=="1"), replace=FALSE)
  sub=c(s1,which(original.l$case_status=="0"))
  lowed=rownames(original.l)[sub]
  set.seed(100)
  s0=sample(which(original.h$case_status=="0"),
            size=sum(original.l$case_status=="0"), replace=FALSE)
  sub=c(which(original.h$case_status=="1"),s0)
  highed=rownames(original.h)[sub]
  # Subset denoised data by stratification
  denoised.h = denoised[which(rownames(denoised) %in% highed),]
  assign(gsub("\\.",".h.",arr),denoised.h)
  denoised.l = denoised[which(rownames(denoised) %in% lowed),]
  assign(gsub("\\.",".l.",arr),denoised.l)
}

table(lung.h.1$case_status)
table(lung.l.1$case_status)
table(lung.h.2$case_status)
table(lung.l.2$case_status)
table(bladder.h.1$case_status)
table(bladder.l.1$case_status)
table(bladder.h.2$case_status)
table(bladder.l.2$case_status)

#Remove the education variables before analysis
bladder.h.1 <- subset(bladder.h.1, select = -c(educationLow, educationHigh))
bladder.h.2 <- subset(bladder.h.2, select = -c(educationLow, educationHigh))
bladder.l.1 <- subset(bladder.l.1, select = -c(educationLow, educationHigh))
bladder.l.2 <- subset(bladder.l.2, select = -c(educationLow, educationHigh))
lung.h.1 <- subset(lung.h.1, select = -c(educationLow, educationHigh))
lung.h.2 <- subset(lung.h.2, select = -c(educationLow, educationHigh))
lung.l.1 <- subset(lung.l.1, select = -c(educationLow, educationHigh))
lung.l.2 <- subset(lung.l.2, select = -c(educationLow, educationHigh))

ifelse(dir.exists("../Results/strat_edu_denoised"),"",dir.create("../Results/strat_edu_denoised")) ### Change path for other stratification
for (m in 1:8){
  arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("h","l"),each=2),".",c(1,2))[m] ### Change name for other stratification
  saveRDS(eval(parse(text=arr)),paste0("../Results/strat_edu_denoised/",arr,"_denoised.rds")) ### Change path for other stratification
}



