### TDS Project -- Stability selection sPLS splitting train/test dataset ARRAY JOB (RUN ON HPC)
## Programme created by Vivian on 22 March 

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

## Load packages
library(pheatmap)
library(RColorBrewer)
library(colorspace)
library(parallel)
library(mixOmics)
library(tidyverse)
library(sgPLS)
library(pROC)

#LoadPackages=function(packages){
#  for (i in 1:length(packages)){
#    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
#  }
#}

#LoadPackages(c("pheatmap", "RColorBrewer", "colorspace","parallel", 
#               "mixOmics", "sgPLS", "tidyverse", "pROC"))

source("penalisation_functions.R")
source("pls_functions.R")

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


## Load data set
arr=paste0(rep(c("lung_TT","bladder_TT"),each=2),".",1:2)[m]
dat=readRDS(paste0("../Results/PLS_319/train_test/",arr,".rds"))


## stabilioty selection
y_train <- dat[[1]][,1]
x_train <- dat[[1]][,-1]
y_test <- dat[[2]][,1]
x_test <- dat[[2]][,-1]

out_1 <- CalibrateRegression(xdata = x_train, ydata = y_train, Lambda=1:ncol(x_train), family="binomial", 
                             implementation="SparsePLS", K=100)

# Save plots
pdf(paste0("../Results/PLS_319/train_test_spls/sPLS_out_",arr,".pdf"), height=5, width=12)
CalibrationPlot(out_1)
dev.off()

# Calibrated selection proportions 
selprop.1=CalibratedSelectionProportionsRegression(out_1, plot=FALSE)

# Extracting ID of calibrated lambda
hat_lambda_id.1=GetArgmaxId(out_1)[1]

# Computing average beta coefficients from models with calibrated lambda
average_load.1=apply(out_1$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})

# Save
saveRDS(out_1, paste0("../Results/PLS_319/train_test_spls/sPLS_out_",arr, ".rds"))

saveRDS(average_load.1, paste0("../Results/PLS_319/train_test_spls/sPLS_load_",arr, ".rds"))

saveRDS(selprop.1, paste0("../Results/PLS_319/train_test_spls/sPLS_selprop_",arr,".rds"))

# Extracting average loadings (without recalibration)
myloadings_1 <- out_1$Beta[which.max(out_1$S),,]
myloadings_1 <- apply(myloadings_1,1,FUN=function(x){mean(x[x!=0])})
myloadings_1[is.na(myloadings_1)]=0
predictions_1 <- as.vector(as.matrix(x_test)%*%cbind(myloadings_1))
myroc=roc(response=y_test, predictor=predictions_1)

saveRDS(myroc, paste0("../Results/PLS_319/train_test_spls/sPLS_roc_",arr,".rds"))

# Extracting average loadings (with recalibration)
selected_1 <- CalibratedStableRegression(out_1)
selected_1 <- names(selected_1[selected_1 == 1])
mypls_1 <- plsda(X=x_train[,selected_1], Y=y_train, ncomp=1)
predictions_re_1 <- predict(mypls_1, newdata=x_test[,selected_1])
predictions_raw_1 <- predictions_re_1$predict[,2,1]
myroc_re_1 <- roc(response=y_test, predictor=predictions_raw_1)

saveRDS(myroc_re_1, paste0("../Results/PLS_319/train_test_spls/sPLS_roc_recali_",arr,".rds"))

# Misclassification rates based on max dist (with recalibration)
predictions_class_1 <- predictions_re_1$class$max.dist
mr_controls_1 <- sum(predictions_class_1 == 1)/length(predictions_class_1)
mr_cases_1 <- sum(predictions_class_1 == 0)/length(predictions_class_1)

saveRDS(mr_controls_1, paste0("../Results/PLS_319/train_test_spls/sPLS_mr_controls_",arr,".rds"))
saveRDS(mr_cases_1, paste0("../Results/PLS_319/train_test_spls/sPLS_mr_cases_",arr,".rds"))


## stabilioty selection

#if (grepl("1$",arr)){
#  y <- dat$case_status
#  x <- dat[,-1]
#  x <- x[, c(5:46, 1:4, 47:98)]
#  set.seed(100)
#  train=caret::createDataPartition(y,p=0.8,list=F) # Directly call caret do not load package
#  y_train=y[train]
#  x_train=x[train,]
#  y_test=y[-train]
#  x_test=x[-train, ]
#  out_1 <- CalibrateRegression(xdata = x_train, ydata = y_train, Lambda=1:ncol(x_train), family="binomial", 
#                              implementation="SparsePLS", K=100)
  
  # Save plots
#  pdf(paste0("../Figures/PLS/sPLS_TT_out_",arr,".pdf"), height=5, width=12)
#  CalibrationPlot(out_1)
#  dev.off()
  
  # Calibrated selection proportions 
#  selprop.1=CalibratedSelectionProportionsRegression(out_1, plot=FALSE)
  
  # Extracting ID of calibrated lambda
#  hat_lambda_id.1=GetArgmaxId(out_1)[1]
  
  # Computing average beta coefficients from models with calibrated lambda
#  average_load.1=apply(out_1$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
  
  # Save
#  saveRDS(out_1, paste0("../Results/PLS_319/sPLS_TT_out_",arr, ".rds"))
  
#  saveRDS(average_load.1, paste0("../Results/PLS_319/sPLS_TT_load_",arr, ".rds"))
  
#  saveRDS(selprop.1, paste0("../Results/PLS_319/sPLS_TT_selprop_",arr,".rds"))
  
  # Extracting average loadings (without recalibration)
#  myloadings_1 <- out_1$Beta[which.max(out_1$S),,]
#  myloadings_1 <- apply(myloadings_1,1,FUN=function(x){mean(x[x!=0])})
#  myloadings_1[is.na(myloadings_1)]=0
#  predictions_1 <- as.vector(as.matrix(x_test)%*%cbind(myloadings_1))
#  myroc=roc(response=y_test, predictor=predictions_1)
#  myroc <- as.numeric(myroc$auc)

#  saveRDS(myroc, paste0("../Results/PLS_319/sPLS_auc_",arr,".rds"))
  
  # Extracting average loadings (with recalibration)
#  selected_1 <- CalibratedStableRegression(out_1)
#  selected_1 <- names(selected_1[selected_1 == 1])
#  mypls_1 <- plsda(X=x_train[,selected_1], Y=y_train, ncomp=1)
#  predictions_re_1 <- predict(mypls_1, newdata=x_test[,selected_1])
#  predictions_raw_1 <- predictions_re_1$predict[,2,1]
#  myroc_re_1 <- roc(response=y_test, predictor=predictions_raw_1)
#  myroc_re_1 <- as.numeric(myroc_re_1$auc)
  
#  saveRDS(myroc_re_1, paste0("../Results/PLS_319/sPLS_auc_recali_",arr,".rds"))
  
  # Misclassification rates based on max dist (with recalibration)
#  predictions_class_1 <- predictions_re_1$class$max.dist
#  mr_controls_1 <- sum(predictions_class_1 == 1)/length(predictions_class_1)
#  mr_cases_1 <- sum(predictions_class_1 == 0)/length(predictions_class_1)
  
#  saveRDS(mr_controls_1, paste0("../Results/PLS_319/sPLS_mr_controls_",arr,".rds"))
#  saveRDS(mr_cases_1, paste0("../Results/PLS_319/sPLS_mr_cases_",arr,".rds"))
  
#} else {
#  y <- dat$case_status
#  x <- dat[,-1]
#  set.seed(100)
#  train=caret::createDataPartition(y,p=0.8,list=F) # Directly call caret do not load package
#  y_train=y[train]
#  x_train=x[train,]
#  y_test=y[-train]
#  x_test=x[-train, ]
#  out_2 <- CalibrateRegression(xdata = x_train, ydata = y_train, Lambda=1:ncol(x_train), family="binomial", 
#                               implementation="SparsePLS", K=100)
  
  # Save plots
#  pdf(paste0("../Figures/PLS/sPLS_TT_out_",arr,".pdf"), height=5, width=12)
#  CalibrationPlot(out_2)
#  dev.off()
  
  # Calibrated selection proportions 
  
#  selprop.2=CalibratedSelectionProportionsRegression(out_2, plot=FALSE)
  # Extracting ID of calibrated lambda
#  hat_lambda_id.2=GetArgmaxId(out_2)[1]
  
  # Computing average beta coefficients from models with calibrated lambda
  
#  average_load.2=apply(out_2$Beta[hat_lambda_id.2,,],1,FUN=function(x){mean(x[x!=0])})
  # Save
#  saveRDS(out_2, paste0("../Results/PLS_319/sPLS_TT_out_",arr, ".rds"))
  
#  saveRDS(average_load.2, paste0("../Results/PLS_319/sPLS_TT_load_",arr, ".rds"))
  
#  saveRDS(selprop.2, paste0("../Results/PLS_319/sPLS_TT_selprop_",arr,".rds"))
  
  # Extracting average loadings (without recalibration)
#  myloadings_2 <- out_2$Beta[which.max(out_2$S),,]
#  myloadings_2 <- apply(myloadings_2,1,FUN=function(x){mean(x[x!=0])})
#  myloadings_2[is.na(myloadings_2)]=0
#  predictions_2 <- as.vector(as.matrix(x_test)%*%cbind(myloadings_2))
#  myroc=roc(response=y_test, predictor=predictions_2)
#  myroc <- as.numeric(myroc$auc)
#  saveRDS(myroc, paste0("../Results/PLS_319/sPLS_auc_",arr,".rds"))
  
  # Extracting average loadings (with recalibration)
#  selected_2 <- CalibratedStableRegression(out_2)
#  selected_2 <- names(selected_2[selected_2 == 1])
#  mypls_2 <- plsda(X=x_train[,selected_2], Y=y_train, ncomp=1)
#  predictions_re_2 <- predict(mypls_2, newdata=x_test[,selected_2])
#  predictions_raw_2 <- predictions_re_2$predict[,2,1]
#  myroc_re_2 <- roc(response=y_test, predictor=predictions_raw_2)
#  myroc_re_2 <- as.numeric(myroc_re_2$auc)
  
#  saveRDS(myroc_re_2, paste0("../Results/PLS_319/sPLS_auc_recali_",arr,".rds"))
  
  # Misclassification rates based on max dist (with recalibration)
#  predictions_class_2 <- predictions_re_2$class$max.dist
#  mr_controls_2 <- sum(predictions_class_2 == 1)/length(predictions_class_2)
#  mr_cases_2 <- sum(predictions_class_2 == 0)/length(predictions_class_2)
  
#  saveRDS(mr_controls_2, paste0("../Results/PLS_319/sPLS_mr_controls_",arr,".rds"))
#  saveRDS(mr_cases_2, paste0("../Results/PLS_319/sPLS_mr_cases_",arr,".rds"))
#}



