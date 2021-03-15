
### TDS Project -- sPLS stability selection re-extract output on HPC
## Programme created by Vivian on 12 March


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

X_1 <-  lung.1[, -1]
X_1 <- cbind(X_1[,5:46], X_1[,1:4], X_1[, 47:98])
Y_1 <-  lung.1[, 1]

t0=Sys.time()
out_1=CalibrateRegression(xdata = X_1, ydata = Y_1, Lambda=1:ncol(X_1), family="binomial", 
                          implementation="SparsePLS", K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(out_1, "sPLS_stability_selection_lung.1.rds")


##### Sparse PLS-DA--

X1 <-  bladder.1[, -1]
X1 <- cbind(X1[,5:46], X1[,1:4], X1[, 47:98])
Y1 <-  bladder.1[, 1]

t0=Sys.time()
out1=CalibrateRegression(xdata = X1, ydata = Y1, Lambda=1:ncol(X1), family="binomial", 
                         implementation="SparsePLS", K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(out1, "sPLS_stability_selection_bladder.1.rds")
