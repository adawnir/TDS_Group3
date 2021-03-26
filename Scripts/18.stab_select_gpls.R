### TDS Project -- Stability selection gPLS ARRAY JOB (RUN ON HPC)
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

source("penalisation_functions.R")
source("pls_functions.R")

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


## Load data set
arr=paste0(rep(c("lung","bladder"),each=2),".",1:2)[m]
dat=readRDS(paste0("../Results/denoised/",arr,"_denoised.rds"))


## stabilioty selection
 if (grepl("1$",arr)){
   Xgroups = c(18, 46, 54, 70)
   y <- dat$case_status
   x <- dat[,-1]
   x <- x[, c(5:46, 1:4, 47:98)]
   out_1 <- CalibrateRegression(xdata = x, ydata = y, Lambda = 1:(length(Xgroups)+1), family = "binomial", 
                                implementation = "GroupPLS", ind.block.x = Xgroups, K=100)
   
   # Save plots
   pdf(paste0("../Figures/PLS/gPLS_out_",arr,".pdf"), height=5, width=12)
   CalibrationPlot(out_1)
   dev.off()
   
   # Calibrated selection proportions 
   selprop.1=CalibratedSelectionProportionsRegression(out_1, plot=FALSE)
   
   # Extracting ID of calibrated lambda
   hat_lambda_id.1=GetArgmaxId(out_1)[1]
   
   # Computing average beta coefficients from models with calibrated lambda
   average_load.1=apply(out_1$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
   
   # Save
   saveRDS(out_1, paste0("../Results/PLS_319/gPLS_out_",arr, ".rds"))
   
   saveRDS(average_load.1, paste0("../Results/PLS_319/gPLS_load_",arr, ".rds"))

   saveRDS(selprop.1, paste0("../Results/PLS_319/gPLS_selprop_",arr,".rds"))
   
 } else {
   Xgroups = c(18, 42, 50, 66)
   y <- dat$case_status
   x <- dat[,-1]
   out_2 <- CalibrateRegression(xdata = x, ydata = y, Lambda = 1:(length(Xgroups)+1), family = "binomial", 
                                implementation = "GroupPLS", ind.block.x = Xgroups, K=100)
   
   # Save plots
   pdf(paste0("../Figures/PLS/gPLS_out_",arr,".pdf"), height=5, width=12)
   CalibrationPlot(out_2)
   dev.off()
   
   # Calibrated selection proportions 
   selprop.2=CalibratedSelectionProportionsRegression(out_2, plot=FALSE)
   
   # Extracting ID of calibrated lambda
   hat_lambda_id.2=GetArgmaxId(out_2)[1]
   
   # Computing average beta coefficients from models with calibrated lambda
   average_load.2=apply(out_2$Beta[hat_lambda_id.2,,],1,FUN=function(x){mean(x[x!=0])})
   
   # Save
   saveRDS(out_2, paste0("../Results/PLS_319/gPLS_out_",arr, ".rds"))
   
   saveRDS(average_load.2, paste0("../Results/PLS_319/gPLS_load_",arr, ".rds"))
   
   saveRDS(selprop.2, paste0("../Results/PLS_319/gPLS_selprop_",arr,".rds"))
   
 }



