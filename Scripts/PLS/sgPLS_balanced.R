
### TDS Project -- sgPLS calibration on a balanced dataset

rm(list=ls())

project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
source("../pls_functions.R")

suppressPackageStartupMessages(library(mixOmics))
suppressPackageStartupMessages(library(sgPLS))
suppressPackageStartupMessages(library(tidyverse))

##### load data
lung.1 <- readRDS("../../Results/denoised/lung.1_denoised.rds")
lung.2 <- readRDS("../../Results/denoised/lung.2_denoised.rds")
bladder.1 <- readRDS("../../Results/denoised/bladder.1_denoised.rds")
bladder.2 <- readRDS("../../Results/denoised/bladder.2_denoised.rds")
 

##### create balanced data ---
set.seed(6)
control_bladder.1 <- bladder.1[which(bladder.1$case_status == 0), ]
control_bladder.1 <- bladder.1[sample(nrow(bladder.1), 800), ]
rr_bladder.1 <- rbind(control_bladder.1, bladder.1[which(bladder.1$case_status == 1),])

X_sg <-  rr_bladder.1[, -1]
X_sg <- cbind(X_sg[,5:46], X_sg[,1:4], X_sg[, 47:98])
Y_sg <-  rr_bladder.1[, 1]
Xgroups = c(18, 46, 54, 70)

X_sg <-  bladder.2[, -1]
Y_sg <-  bladder.2[, 1]
Xgroups = c(18, 42, 50, 66)

##### calibration sgPLS ---
set.seed(1)
Xgroups = c(18, 46, 54, 70)
res_sgplsda = CalibratesgPLSDA(dataX = X_sg, dataY = Y_sg,
                               ncomp = 1, Nrepeat = 100, Xgroups = Xgroups)
PlotCalib(res = res_sgplsda, type = "sgPLSDA")
res_sgplsda$NGroup
res_sgplsda$alpha

set.seed(1)
Xgroups = c(18, 46, 54, 70)
res_sgplsda = CalibratesgPLSDA(dataX = X_sg, dataY = Y_sg,
                               ncomp = 1, Nrepeat = 100, Xgroups = Xgroups)
PlotCalib(res = res_sgplsda, type = "sgPLSDA")
res_sgplsda$NGroup
res_sgplsda$alpha

##### sgPLS ---

MygPLSDA_pooled <- gPLSda(X_pooled, Y_pooled, ncomp = 1, ind.block.x = Xgroups, keepX = 1)
MygPLSDA_pooled$loadings$X
MysgPLSDA_pooled <- sgPLSda(X_pooled, Y_pooled, ncomp = 1, 
                            ind.block.x = Xgroups, keepX = 1, alpha.x = 0.5)
MysgPLSDA_pooled$loadings$X

saveRDS(res_sgplsda, "/rds/general/user/cl420/home/TDS/sgpls.rds")

