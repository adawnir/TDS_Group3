### TDS Project -- Ridge, LASSO and Elastic Net Logistic Regression
## Programme created by Rin Wada on 25 Feb 2021

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

## Load packages
library(glmnet)
library(tidyverse)

# Load dataset
multivar_data=readRDS("../Results/multivar_data.rds")
dim(multivar_data)
str(multivar_data)

# Separate by case
lung_data=multivar_data[multivar_data$case_status!="bladder",]
bladder_data=multivar_data[multivar_data$case_status!="lung",]
# Recode case/control status
lung_data$case_status = as.factor(ifelse(as.character(lung_data$case_status) == "control",0,1))
bladder_data$case_status = as.factor(ifelse(as.character(bladder_data$case_status) == "control",0,1))

cases=c("lung","bladder")

for (c in 1:length(cases)){
  case=cases[c]
  print(case)
  dat=eval(parse(text=paste0(case,"_data")))
  Y=dat[,2]
  X=dat %>% select(-eid,-case_status)
  X=model.matrix(~., X)[,-1]
  dim(X)
  ## Ridge
  # Cross validation 10 fold
  set.seed(100)
  print("Running first ridge model")
  fit=cv.glmnet(X, Y, alpha=0, family='binomial')
  # Save plots
  ifelse(dir.exists("../Plots"),"",dir.create("../Plots"))
  png(paste0("../Plots/ridge_fit_",case,".png"), width = 400, height = 300) 
  par(mfrow=c(1,1))
  plot(fit)
  dev.off()
  # chosen lambda
  bestlam=fit$lambda.1se
  results=as.matrix(coef(fit,s=bestlam))
  saveRDS(results, paste0("../Results/ridge_", case,"_model_est.rds"))
 
  ## LASSO
  # age, sex and bmi
  penalty1=c(rep(0,2),rep(1,16),rep(0,5),rep(1,83)) 
  # age, sex, bmi and smoking
  penalty2=c(rep(0,2),rep(1,16),rep(0,5),rep(1,21),rep(0,2),rep(1,60))
  # Cross validation 10 fold
  set.seed(100)
  print("Running first lasso model")
  fit1=cv.glmnet(X, Y, alpha=1, family='binomial',penalty.factor=penalty1)
  print("Running second lasso model")
  fit2=cv.glmnet(X, Y, alpha=1, family='binomial',penalty.factor=penalty2)
  # Save plots
  ifelse(dir.exists("../Plots"),"",dir.create("../Plots"))
  png(paste0("../Plots/lasso_fit_",case,".png"), width = 800, height = 450) 
  par(mfrow=c(1,2))
  plot(fit1)
  plot(fit2)
  dev.off()
  # chosen lambda
  bestlam1=fit1$lambda.1se
  bestlam2=fit2$lambda.1se
  results1=as.matrix(coef(fit1,s=bestlam1))
  results2=as.matrix(coef(fit2,s=bestlam2))
  saveRDS(results1, paste0("../Results/lasso_", case,"_model1_est.rds"))
  saveRDS(results2, paste0("../Results/lasso_", case,"_model2_est.rds"))
}



