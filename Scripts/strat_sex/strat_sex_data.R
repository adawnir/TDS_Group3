### TDS Project -- Stratify denoised data
## Programme created by Rin Wada on 21 March

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
  # Subsample data sets: adjust case size to smaller data set (female and male)
  original.m=original[original$sex=="Male",]
  original.f=original[original$sex=="Female",]
  set.seed(100)
  s1=sample(which(original.m$case_status=="1"),
            size=sum(original.f$case_status=="1"), replace=FALSE)
  sub=c(s1,which(original.m$case_status=="0"))
  male=rownames(original.m)[sub]
  set.seed(100)
  s0=sample(which(original.f$case_status=="0"),
            size=sum(original.m$case_status=="0"), replace=FALSE)
  sub=c(which(original.f$case_status=="1"),s0)
  female=rownames(original.f)[sub]
  # Subset denoised data by stratification
  denoised.f = denoised[which(rownames(denoised) %in% female),]
  assign(gsub("\\.",".f.",arr),denoised.f)
  denoised.m = denoised[which(rownames(denoised) %in% male),]
  assign(gsub("\\.",".m.",arr),denoised.m)
}

table(lung.f.1$case_status)
table(lung.m.1$case_status)
table(lung.f.2$case_status)
table(lung.m.2$case_status)
table(bladder.f.1$case_status)
table(bladder.m.1$case_status)
table(bladder.f.2$case_status)
table(bladder.m.2$case_status)

ifelse(dir.exists("../Results/strat_sex_denoised"),"",dir.create("../Results/strat_sex_denoised")) ### Change path for other stratification
for (m in 1:8){
  arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("f","m"),each=2),".",c(1,2))[m] ### Change name for other stratification
  saveRDS(eval(parse(text=arr)),paste0("../Results/strat_sex_denoised/",arr,"_denoised.rds")) ### Change path for other stratification
}


