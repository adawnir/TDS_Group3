
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
X_1 <-  lung.1[, -1]
X_1 <- cbind(X_1[,5:46], X_1[,1:4], X_1[, 47:98])
Y_1 <-  lung.1[, 1]

set.seed(1)
Xgroups = c(18, 46, 54, 70)
sg_lung_1 = CalibratesgPLSDA(dataX = X_1, dataY = Y_1,
                               ncomp = 1, Nrepeat = 100, Xgroups = Xgroups)

saveRDS(sg_lung_1, "sgpls_lung_1.rds")


#X_2 <-  lung.2[, -1]
#Y_2 <-  lung.2[, 1]

#set.seed(1)
#Xgroups = c(18, 42, 50, 66)
#sg_lung_2 = CalibratesgPLSDA(dataX = X_2, dataY = Y_2, ncomp = 1, Nrepeat = 100, Xgroups = Xgroups)

#saveRDS(sg_lung_2, "sgpls_lung_2.rds")
