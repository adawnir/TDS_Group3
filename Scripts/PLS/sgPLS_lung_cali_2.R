
### TDS Project -- sgPLS calibration on original lung dataset

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


##### calibration sgPLS ---
X_sg <-  lung.2[, -1]
X_sg <- cbind(X_sg[,5:47], X_sg[,1:4], X_sg[, 48:98])
Y_sg <-  lung.2[, 1]

set.seed(1)
Xgroups = c(18, 47, 54, 70)
res_sgplsda = CalibratesgPLSDA(dataX = X_sg, dataY = Y_sg,
                               ncomp = 1, Nrepeat = 100, Xgroups = Xgroups)

saveRDS(res_sgplsda, "/rds/general/user/cl420/home/TDS/sgpls_2.rds")

