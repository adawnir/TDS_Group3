# select full population models
# AFTER denoising nvsm, BEFORE denoising base model to compare to
# by ines on april 21st


rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

## Load packages
library(tidyverse)

lung = readRDS("../Results/nvsm/lung_base_denoised_nvsm.rds")
bladder= readRDS("../Results/nvsm/bladder_base_denoised_nvsm.rds")

## Load data

lung.full=readRDS("../Results/nvsm/base/multivar_data_lung.rds")
bladder.full=readRDS("../Results/nvsm/base/multivar_data_bladder.rds")

# Recode case/control status
lung.full$case_status = as.factor(ifelse(as.character(lung.full$case_status) == "control",0,1))
bladder.full$case_status = as.factor(ifelse(as.character(bladder.full$case_status) == "control",0,1))

# makes samples from full population equal in size to nvsm datasets
for (m in 1:2){
  arr=c("lung","bladder")[m] ### Change name for other stratification
  dat_name=c("lung.full","bladder.full")[m]
  original = eval(parse(text = dat_name))
  denoised = eval(parse(text = arr))
  
  # Subsample data sets: adjust case size to neversmoker dataset
  set.seed(100)
  s1=sample(which(original$case_status=="1"), # choses number of cases from full = number of cases in nvsm
            size=sum(denoised$case_status=="1"), replace=FALSE)
  set.seed(100)
  s0=sample(which(original$case_status=="0"), # choses number of controls from full = number of controls in nvsm
            size=sum(denoised$case_status=="0"), replace=FALSE)
  sub=c(s1,s0)
  chosen=rownames(original)[sub] # samples are indices, not rownames, so must get indices they correspond to
  nvsm = original[which(rownames(original) %in% chosen),] # get rows they correspond to
  assign(gsub("\\.full",".subset", dat_name),nvsm) # save dataset as the subset of the full data set (should = size of lung & bladder)
}

# save these, to denoise them separately + run analysis
saveRDS(bladder.subset, "../Results/nvsm/base/multivar_data_bladder_subset.rds")
saveRDS(lung.subset, "../Results/nvsm/base/multivar_data_lung_subset.rds")

bids<-rownames(bladder.subset)
lids<-rownames(lung.subset)

saveRDS(bids, "../Results/nvsm/base/bladder.subset.ids.rds")
saveRDS(lids, "../Results/nvsm/base/lung.subset.ids.rds")