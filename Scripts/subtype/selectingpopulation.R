# select full population models for sites
# by ines on april 21st

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

## Load packages
library(tidyverse)

## Load data
for (m in 1:4){
  arr=paste0(rep(c("mal.lower","mal.upper"),each=2),".",1:2)[m] ### Change name for other stratification
  dat=readRDS(paste0("../Results/strat_site_denoised/",arr,"_denoised.rds")) ### Change path for other stratification
  assign(arr,dat) # Assign name
}


# makes samples from full population equal in size to nvsm datasets
for (m in 1:4){
  arr0=paste0(rep(c("mal.lower","mal.upper"),each=2),".",1:2)[m] ### Change name for other stratification
  denoised = eval(parse(text = arr0))
  
  # Subsample data sets: adjust case size to neversmoker dataset
  s1=sample(which(denoised$case_status=="1"), # choses number of controls from full = number of controls in nvsm
            size=sum(denoised$case_status=="1"), replace=FALSE)
  set.seed(100)
  s0=sample(which(denoised$case_status=="0"), # choses number of controls from full = number of controls in nvsm
            size=sum(denoised$case_status=="1"), replace=FALSE)
  sub=c(s1,s0)
  chosen=rownames(denoised)[sub] # samples are indices, not rownames, so must get indices they correspond to
  nvsm = denoised[which(rownames(denoised) %in% chosen),] # get rows they correspond to
  assign(gsub("\\.full",".subset", arr0),nvsm) # save dataset as the subset of the full data set (should = size of lung & bladder)
}

# save these,
for (m in 1:4){
  arr=paste0(rep(c("mal.lower","mal.upper"),each=2),".",1:2)[m] ### Change name for other stratification
  saveRDS(arr, paste0("../Results/strat_site_split/",arr,"_denoised.rds")) ### Change path for other stratification
}