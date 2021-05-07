### TDS Project -- Univariate logistic regression analysis -- Parallelisation
### Programme created by Rin Wada on 26 Feb 2021 reviewed on 8 March

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(tidyverse)

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])

## Load data set
arr=paste0(rep(c("lung","bladder"),each=2),".",1:2)[m]
dat=readRDS(paste0("../Results/denoised/",arr,"_denoised.rds"))

## Make data set
Y=dat$case_status
X=as.matrix(dat[,-1])

### Logistic regression----
betas = pval = NULL

for(k in 1:ncol(X)){
  ## Split
  print(paste0("Feature: ",colnames(X)[k]))
  
  ## logistic
  glm = glm(Y ~ X[,k], family = "binomial")
  betas = c(betas, glm$coefficients[2])
  pval=c(pval, summary(glm)$coefficients[2,4])
}
names(pval)=names(betas)=colnames(X)

## Save
ifelse(dir.exists("../Results/denoised_univ"),"",dir.create("../Results/denoised_univ"))
saveRDS(betas, paste0("../Results/denoised_univ/",arr,"_betas.rds"))
saveRDS(pval, paste0("../Results/denoised_univ/",arr,"_pval.rds"))                            

