### TDS Project -- Stability selection LASSO Logistic Regression ARRAY JOB (RUN ON HPC)
## Programme created by Rin Wada on 14 March

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

## Load packages
LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}

LoadPackages(c("pheatmap","corpcor","abind","parallel",
               "RColorBrewer","igraph","ppcor","mvtnorm",
               "pROC","glasso","stabs","huge","pulsar",
               "QUIC","glassoFast","colorspace","glmnet","caret"))

source("penalisation_functions.R")

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


## Load data set
arr=paste0(rep(c("lung","bladder"),each=3),".",0:2)[m]
dat=readRDS(paste0("../Results/denoised/",arr,"_denoised.rds"))

## Make data set
y=dat$case_status
x=as.matrix(dat[,-1])
set.seed(100)
train=createDataPartition(y,p=0.8,list=F)
y_train=y[train]
x_train=x[train,]
y_test=y[-train]
x_test=x[-train, ]

# Create directories
ifelse(dir.exists("../Figures/lasso"),"",dir.create("../Figures/lasso"))
ifelse(dir.exists("../Results/lasso"),"",dir.create("../Results/lasso"))

# Running stability selection
if (grepl("0$",arr)){
  out.1=CalibrateRegression(xdata=x_train, ydata=y_train, K=100, tau=0.5, verbose=FALSE,
                            penalty.factor=c(rep(0,7),rep(1,ncol(x)-7)),
                            family="binomial")
  out.2=CalibrateRegression(xdata=x_train, ydata=y_train, K=100, tau=0.5, verbose=FALSE,
                            penalty.factor=c(rep(0,11),rep(1,ncol(x)-11)),
                            family="binomial")
  
  # Save plots
  pdf(paste0("../Figures/lasso/out_",arr,".1",".pdf"), height=5, width=12)
  CalibrationPlot(out.1)
  dev.off()
  pdf(paste0("../Figures/lasso/out_",arr,".2",".pdf"), height=5, width=12)
  CalibrationPlot(out.2)
  dev.off()
  
  # Calibrated selection proportions 
  selprop.1=CalibratedSelectionProportionsRegression(out.1, plot=FALSE)
  selprop.2=CalibratedSelectionProportionsRegression(out.2, plot=FALSE)
  
  # Extracting ID of calibrated lambda
  hat_lambda_id.1=GetArgmaxId(out.1)[1]
  hat_lambda_id.2=GetArgmaxId(out.2)[1]
  
  # Computing average beta coefficients from models with calibrated lambda
  average_beta.1=apply(out.1$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
  average_beta.2=apply(out.2$Beta[hat_lambda_id.2,,],1,FUN=function(x){mean(x[x!=0])})
  
  # Save
  saveRDS(out.1, paste0("../Results/lasso/out_",arr,".1",".rds"))
  saveRDS(out.2, paste0("../Results/lasso/out_",arr,".2",".rds"))
  saveRDS(average_beta.1, paste0("../Results/lasso/average_beta_",arr,".1",".rds"))
  saveRDS(average_beta.2, paste0("../Results/lasso/average_beta_",arr,".2",".rds"))
  saveRDS(selprop.1, paste0("../Results/lasso/selprop_",arr,".1",".rds"))
  saveRDS(selprop.2, paste0("../Results/lasso/selprop_",arr,".2",".rds"))
  
  # Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
  selprop_nonzero.1=selprop.1[selprop.1>0]
  myorder=names(selprop_nonzero.1)[sort.list(selprop_nonzero.1, decreasing = TRUE)]
  myaucs=NULL
  myaucs_table=NULL
  
  for (k in 1:length(myorder)){
    # Using the average beta as a beta, only computing the intercept in the model below (no impact on AUC)
    mymodel=glm(y_test~offset(x_test[,myorder[1:k],drop=FALSE]%*%
                                matrix(average_beta.1[myorder[1:k]],ncol=1)),
                family="binomial")
    myroc=roc(response=y_test, predictor=mymodel$fitted.values)
    myaucs=c(myaucs, myroc$auc)
    myaucs_table=rbind(myaucs_table, formatC(as.numeric(ci.auc(myroc)), format="f",
                                             digits=4))
  }
  rownames(myaucs_table)=myorder
  colnames(myaucs_table)=c("li","auc","ui")
  saveRDS(myaucs_table, paste0("../Results/lasso/auc_",arr,".1",".rds"))
  
  selprop_nonzero.2=selprop.2[selprop.2>0]
  myorder=names(selprop_nonzero.2)[sort.list(selprop_nonzero.2, decreasing = TRUE)]
  myaucs=NULL
  myaucs_table=NULL
  
  for (k in 1:length(myorder)){
    # Using the average beta as a beta, only computing the intercept in the model below (no impact on AUC)
    mymodel=glm(y_test~offset(x_test[,myorder[1:k],drop=FALSE]%*%
                                matrix(average_beta.2[myorder[1:k]],ncol=1)),
                family="binomial")
    myroc=roc(response=y_test, predictor=mymodel$fitted.values)
    myaucs=c(myaucs, myroc$auc)
    myaucs_table=rbind(myaucs_table, formatC(as.numeric(ci.auc(myroc)), format="f",
                                             digits=4))
  }
  rownames(myaucs_table)=myorder
  colnames(myaucs_table)=c("li","auc","ui")
  saveRDS(myaucs_table, paste0("../Results/lasso/auc_",arr,".2",".rds"))
} else{
  out=CalibrateRegression(xdata=x_train, ydata=y_train, K=100, tau=0.5, verbose=FALSE,
                          family="binomial")
  
  # Save plot
  pdf(paste0("../Figures/lasso/out_",arr,".pdf"), height=5, width=12)
  CalibrationPlot(out)
  dev.off()
  
  # Extracting ID of calibrated lambda
  hat_lambda_id=GetArgmaxId(out)[1]
  
  # Computing average beta coefficients from models with calibrated lambda
  average_beta=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})
  
  # Calibrated selection proportions 
  selprop=CalibratedSelectionProportionsRegression(out, plot=FALSE)
  
  # Save
  saveRDS(out, paste0("../Results/lasso/out_",arr,".rds"))
  saveRDS(average_beta, paste0("../Results/lasso/average_beta_",arr,".rds"))
  saveRDS(selprop, paste0("../Results/lasso/selprop_",arr,".rds"))
  
  # Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
  selprop_nonzero=selprop[selprop>0]
  myorder=names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
  myaucs=NULL
  myaucs_table=NULL
  
  for (k in 1:length(myorder)){
    # Using the average beta as a beta, only computing the intercept in the model below (no impact on AUC)
    mymodel=glm(y_test~offset(x_test[,myorder[1:k],drop=FALSE]%*%
                                matrix(average_beta[myorder[1:k]],ncol=1)),
                family="binomial")
    myroc=roc(response=y_test, predictor=mymodel$fitted.values)
    myaucs=c(myaucs, myroc$auc)
    myaucs_table=rbind(myaucs_table, formatC(as.numeric(ci.auc(myroc)), format="f",
                                             digits=4))
  }
  rownames(myaucs_table)=myorder
  colnames(myaucs_table)=c("li","auc","ui")
  saveRDS(myaucs_table, paste0("../Results/lasso/auc_",arr,".rds"))
}


