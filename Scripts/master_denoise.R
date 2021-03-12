### TDS Project -- Denoise Data (RUN ON HPC)
## Programme created by Rin Wada on 2 March

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

## Load packages
library(tidyverse)
library(glmnet)
library(caret)

# Load dataset
multi.0=readRDS("../Results/multivar_data.rds")
dim(multi.0)
str(multi.0)
### Make three data sets ----
### Original = 0; Denoised for age, sex and bmi = 1; Denoised for age, sex, bmi and smoking = 2

# Separate by case
lung.0=multi.0[multi.0$case_status!="bladder",]
bladder.0=multi.0[multi.0$case_status!="lung",]
# Recode case/control status
lung.0$case_status = as.factor(ifelse(as.character(lung.0$case_status) == "control",0,1))
bladder.0$case_status = as.factor(ifelse(as.character(bladder.0$case_status) == "control",0,1))

# toy
#set.seed(100)
#sub=createDataPartition(lung.0$case_status, p=0.1, list=F)
#lung.0=lung.0[sub,]

## Denoise data
foo = function(i, dat, smoking = FALSE){
  var_name=colnames(dat)[i]
  print(var_name)
  confound=c("age_baseline","sex", "bmi")  # Change confounders accordingly
  if(smoking==TRUE){
    confound=append(confound,"smoking")
  }
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

# Stop if warning
# options(warn=2)

## Run denoise function
lung.1=sapply(5:ncol(lung.0), foo, dat=lung.0)
lung.1=do.call("cbind",lung.1)
lung.2=sapply(6:ncol(lung.0), foo, dat=lung.0, smoking=T)
lung.2=do.call("cbind",lung.2)

bladder.1=sapply(5:ncol(bladder.0), foo, dat=bladder.0)
bladder.1=do.call("cbind",bladder.1)
bladder.2=sapply(6:ncol(bladder.0), foo, dat=bladder.0, smoking=T)
bladder.2=do.call("cbind",bladder.2)

## One hot encoding for non-denoised data
tmp=as.data.frame(model.matrix(~.,lung.0[,-1])[,-1])
lung.0=cbind(lung.0$case_status,tmp)
colnames(lung.0)[1]="case_status"

tmp=as.data.frame(model.matrix(~.,bladder.0[,-1])[,-1])
bladder.0=cbind(bladder.0$case_status,tmp)
colnames(bladder.0)[1]="case_status"

# Rename columns and add Y
lung.1=cbind(lung.0$case_status,as.data.frame(lung.1))
colnames(lung.1)=colnames(lung.0)[c(1,9:ncol(lung.0))]
lung.2=cbind(lung.0$case_status,as.data.frame(lung.2))
colnames(lung.2)=colnames(lung.0)[c(1,13:ncol(lung.0))]

bladder.1=cbind(bladder.0$case_status,as.data.frame(bladder.1))
colnames(bladder.1)=colnames(bladder.0)[c(1,9:ncol(bladder.0))]
bladder.2=cbind(bladder.0$case_status,as.data.frame(bladder.2))
colnames(bladder.2)=colnames(bladder.0)[c(1,13:ncol(bladder.0))]

# Save data sets
ifelse(dir.exists("../Results/denoised"),"",dir.create("../Results/denoised"))
saveRDS(lung.0,"../Results/denoised/lung.0_denoised.rds")
saveRDS(lung.1,"../Results/denoised/lung.1_denoised.rds")
saveRDS(lung.2,"../Results/denoised/lung.2_denoised.rds")
saveRDS(bladder.0,"../Results/denoised/bladder.0_denoised.rds")
saveRDS(bladder.1,"../Results/denoised/bladder.1_denoised.rds")
saveRDS(bladder.2,"../Results/denoised/bladder.2_denoised.rds")
