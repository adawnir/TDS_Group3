### TDS Project -- One-hot encoding and Denoise Data for Never smoker sensitivity analysis (RUN ON HPC)
## Programme created by Rin Wada on 1 April
# rerun on 15 and 21st of april :)

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

## Load packages
library(tidyverse)

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


## Load data set
arr=c("lung","bladder")[m]
dat=readRDS(paste0("../Results/nvsm/multivar_data_",arr,"_nvsm.rds"))
dat<-dat[complete.cases(dat),]
# Recode case/control status
dat$case_status = as.factor(ifelse(as.character(dat$case_status) == "control",0,1))

## Denoise data
foo = function(i, dat){
  var_name=colnames(dat)[i]
  print(var_name)
  confound=c("age_baseline","sex", "bmi")  # Change confounders accordingly
  if(is.numeric(dat[,i])){
    model = lm(as.formula(paste(var_name,paste(confound,collapse="+"),sep="~")),data=dat)
    res=model$residuals
  } else {
    X=as.data.frame(model.matrix(~., dat[,-1])[,-1])
    # Find all columns with var_name
    mygrep=grep(paste0("^",var_name),colnames(X))
    res=NULL
    for (j in mygrep){
      model = glm(as.formula(paste("X[,j]",paste(confound,collapse="+"),sep="~")),data=dat,
                  family="binomial")
      res=cbind(res,model$residuals)
    }
  }
  return(res)
}

## Run denoise function
dat.1=sapply(5:ncol(dat), foo, dat=dat)
dat.1=do.call("cbind",dat.1)

## One hot encoding for non-denoised data
tmp=as.data.frame(model.matrix(~.,dat[,-1])[,-1])
dat.0=cbind(dat$case_status,tmp)
colnames(dat.0)[1]="case_status"

# Rename columns and add Y
dat.1=cbind(dat.0$case_status,as.data.frame(dat.1))
colnames(dat.1)=colnames(dat.0)[c(1,9:ncol(dat.0))]

# Save data sets
ifelse(dir.exists("../Results/nvsm"),"",dir.create("../Results/nvsm"))
saveRDS(dat.0,paste0("../Results/nvsm/",arr,"_one_hot.rds"))
saveRDS(dat.1,paste0("../Results/nvsm/",arr,"_base_denoised.rds"))
