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

for (m in 1:4){
  arr=paste0(rep(c("lung","bladder"),each=2),".",1:2)[m] ### Change name for other stratification
  dat_name=rep(c("lung","bladder"),each=2)[m]
  original = eval(parse(text = dat_name))
  denoised = eval(parse(text = arr))
  ### Change subset function for other stratification
  female = rownames(original)[original$sex=="Female"] 
  denoised.f = denoised[which(rownames(denoised) %in% female),]
  assign(gsub("\\.",".f.",arr),denoised.f)
  denoised.m = denoised[-which(rownames(denoised) %in% female),]
  assign(gsub("\\.",".m.",arr),denoised.m)
}

ifelse(dir.exists("../Results/strat_sex_denoised"),"",dir.create("../Results/strat_sex_denoised")) ### Change path for other stratification
for (m in 1:8){
  arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("f","m"),each=2),".",c(1,2))[m] ### Change name for other stratification
  saveRDS(eval(parse(text=arr)),paste0("../Results/strat_sex_denoised/",arr,"_denoised.rds")) ### Change path for other stratification
}


