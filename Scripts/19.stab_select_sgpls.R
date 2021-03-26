
### TDS Project -- Stability selection sgPLS ARRAY JOB (RUN ON HPC)
## Programme created by Vivian on 20 March 

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

## Load packages
LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}

LoadPackages(c("pheatmap", "RColorBrewer", "colorspace","parallel", 
               "mixOmics", "sgPLS", "tidyverse"))

source("PLS/template/penalisation_functions.R")
source("PLS/template/functions.R")

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


## Load data set
arr=paste0(rep(c("lung","bladder"),each=2),".",1:2)[m]
dat=readRDS(paste0("../Results/denoised/",arr,"_denoised.rds"))


## stabilioty selection
if (grepl("1$",arr)){
  y <- dat$case_status
  x <- dat[,-1]
  x <- x[, c(5:46, 1:4, 47:98)]
  
  Xgroups = c(18, 46, 54, 70)
  niter=100
  alpha_list=seq(0.05,0.95,by=0.05)
  stability_alpha=NULL
  for (k in 1:length(alpha_list)){
    myalpha=alpha_list[k]
    tmp=CalibrateRegression(xdata = x, ydata = y, Lambda=1:(length(Xgroups)+1), 
                            K = niter, family="binomial", implementation="SparseGroupPLS", 
                            ind.block.x = Xgroups, alpha.x=myalpha)
    assign(paste0("out_", k), tmp)
    stability_alpha=cbind(stability_alpha, tmp$S)
  }
  rownames(stability_alpha)=tmp$Lambda
  colnames(stability_alpha)=alpha_list
  hat_alpha_id=which.max(apply(stability_alpha,2,max))
  hat_alpha=alpha_list[hat_alpha_id]
  out_1 = eval(parse(text=paste0("out_", hat_alpha_id)))
  
  # Save plots
  pdf(paste0("../Figures/PLS/sgPLS_out_",arr,".pdf"), height=5, width=12)
  CalibrationPlot(out_1)
  dev.off()
  
  # Calibrated selection proportions 
  selprop.1=CalibratedSelectionProportionsRegression(out_1, plot=FALSE)
  
  # Extracting ID of calibrated lambda
  hat_lambda_id.1=GetArgmaxId(out_1)[1]
  
  # Computing average beta coefficients from models with calibrated lambda
  average_load.1=apply(out_1$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
  
  # Save
  saveRDS(out_1, paste0("../Results/PLS_319/sgPLS_out_",arr, ".rds"))
  
  saveRDS(average_load.1, paste0("../Results/PLS_319/sgPLS_load_",arr, ".rds"))
  
  saveRDS(selprop.1, paste0("../Results/PLS_319/sgPLS_selprop_",arr,".rds"))
  
} else {
  y <- dat$case_status
  x <- dat[,-1]
  
  Xgroups = c(18, 42, 50, 66)
  niter=100
  alpha_list=seq(0.05,0.95,by=0.05)
  stability_alpha=NULL
  for (k in 1:length(alpha_list)){
    myalpha=alpha_list[k]
    tmp=CalibrateRegression(xdata = x, ydata = y, Lambda=1:(length(Xgroups)+1), 
                            K = niter, family="binomial", implementation="SparseGroupPLS", 
                            ind.block.x = Xgroups, alpha.x=myalpha)
    assign(paste0("out_", k), tmp)
    stability_alpha=cbind(stability_alpha, tmp$S)
  }
  rownames(stability_alpha)=tmp$Lambda
  colnames(stability_alpha)=alpha_list
  hat_alpha_id=which.max(apply(stability_alpha,2,max))
  hat_alpha=alpha_list[hat_alpha_id]
  out_2 = eval(parse(text=paste0("out_", hat_alpha_id)))
  
  # Save plots
  pdf(paste0("../Figures/PLS/sgPLS_out_",arr,".pdf"), height=5, width=12)
  CalibrationPlot(out_2)
  dev.off()
  
  # Calibrated selection proportions 
  selprop.2=CalibratedSelectionProportionsRegression(out_2, plot=FALSE)
  
  # Extracting ID of calibrated lambda
  hat_lambda_id.2=GetArgmaxId(out_2)[1]
  
  # Computing average beta coefficients from models with calibrated lambda
  average_load.2=apply(out_2$Beta[hat_lambda_id.2,,],1,FUN=function(x){mean(x[x!=0])})
  
  # Save
  saveRDS(out_2, paste0("../Results/PLS_319/sgPLS_out_",arr, ".rds"))
  
  saveRDS(average_load.2, paste0("../Results/PLS_319/sgPLS_load_",arr, ".rds"))
  
  saveRDS(selprop.2, paste0("../Results/PLS_319/sgPLS_selprop_",arr,".rds"))
  
}



