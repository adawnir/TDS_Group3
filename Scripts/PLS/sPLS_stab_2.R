
### TDS Project -- sPLS stability selection re-extract output on HPC
## Programme created by Vivian on 14 March


rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/PLS"
setwd(project_path)

source("template/penalisation_functions.R")
source("template/functions.R")

suppressPackageStartupMessages(library(mixOmics))
suppressPackageStartupMessages(library(sgPLS))
suppressPackageStartupMessages(library(tidyverse))

##### load data
lung.1 <- readRDS("../../Results/denoised/lung.1_denoised.rds")
lung.2 <- readRDS("../../Results/denoised/lung.2_denoised.rds")
bladder.1 <- readRDS("../../Results/denoised/bladder.1_denoised.rds")
bladder.2 <- readRDS("../../Results/denoised/bladder.2_denoised.rds")



##### Sparse PLS-DA--

X_2 <-  lung.2[, -1]
Y_2 <-  lung.2[, 1]

t0=Sys.time()
out_2=CalibrateRegression(xdata = X_2, ydata = Y_2, Lambda=1:ncol(X_2), family="binomial", 
                          implementation="SparsePLS", K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(out_2, "sPLS_stability_selection_lung.2.rds")


##### Sparse PLS-DA--

X2 <-  bladder.2[, -1]
Y2 <-  bladder.2[, 1]

t0=Sys.time()
out2=CalibrateRegression(xdata = X2, ydata = Y2, Lambda=1:ncol(X2), family="binomial", 
                         implementation="SparsePLS", K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(out2, "sPLS_stability_selection_bladder.2.rds")
