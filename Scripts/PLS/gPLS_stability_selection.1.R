
### TDS Project -- gPLS stability selection on HPC for extract output

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

##### Group index
Xgroups_1 = c(18, 46, 54, 70)
Xgroups_2 = c(18, 42, 50, 66)


##### gPLS
### bladder.1
X_g1 <-  bladder.1[, -1]
X_g1 <- cbind(X_g1[,5:46], X_g1[,1:4], X_g1[, 47:98])
Y_g1 <-  bladder.1[, 1]

t0=Sys.time()
g_out1=CalibrateRegression(xdata = X_g1, ydata = Y_g1, Lambda = 1:(length(Xgroups_1)+1), family = "binomial", 
                           implementation = "GroupPLS", ind.block.x = Xgroups_1, K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(g_out1, "gPLS_stability_selection_bladder.1.rds")

### bladder.2
X_g2 <-  bladder.2[, -1]
Y_g2 <-  bladder.2[, 1]

t0=Sys.time()
g_out2=CalibrateRegression(xdata = X_g2, ydata = Y_g2, Lambda = 1:(length(Xgroups_2)+1), family = "binomial", 
                           implementation = "GroupPLS", ind.block.x = Xgroups_2, K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))


saveRDS(g_out2, "gPLS_stability_selection_bladder.2.rds")


### lung.1
X_g_1 <-  lung.1[, -1]
X_g_1 <- cbind(X_g_1[,5:46], X_g_1[,1:4], X_g_1[, 47:98])
Y_g_1 <-  lung.1[, 1]

t0=Sys.time()
g_out_1=CalibrateRegression(xdata = X_g_1, ydata = Y_g_1, Lambda = 1:(length(Xgroups_1)+1), family = "binomial", 
                           implementation = "GroupPLS", ind.block.x = Xgroups_1, K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(g_out_1, "gPLS_stability_selection_lung.1.rds")


### lung.2
X_g_2 <-  lung.2[, -1]
Y_g_2 <-  lung.2[, 1]

t0=Sys.time()
g_out_2=CalibrateRegression(xdata = X_g_2, ydata = Y_g_2, Lambda = 1:(length(Xgroups_2)+1), family = "binomial", 
                            implementation = "GroupPLS", ind.block.x = Xgroups_2, K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(g_out_2, "gPLS_stability_selection_lung.2.rds")

