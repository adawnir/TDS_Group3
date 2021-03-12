
### TDS Project -- gPLS stability selection

rm(list=ls())

project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
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

# Group
Xgroups_1 = c(18, 46, 54, 70)
Xgroups_2 = c(18, 42, 50, 66)

GroupPLS_1 <- function(x, y, lambda, family="binomial", ncomp=1, ind.block.x, ...){
  mybeta=matrix(NA, nrow=length(lambda), ncol=ncol(x))
  for (k in 1:length(lambda)){
    nvarx=lambda[k]
    if (family=="binomial"){
      mymodel=gPLSda(X=x, Y=y, ncomp=ncomp, keepX=nvarx, ind.block.x = Xgroups_1, ...)
    } else {
      mymodel=gPLS(X=x, Y=y, ncomp=ncomp, keepX=nvarx, ind.block.x = Xgroups_1, ...)
    }
    mybeta[k,]=mymodel$loadings$X[,1]
  }
  colnames(mybeta)=colnames(x)
  rownames(mybeta)=paste0("s",0:(length(lambda)-1))
  
  return(mybeta)
}

# Group PLS-DA
X_g1 <-  bladder.1[, -1]
X_g1 <- cbind(X_g1[,5:46], X_g1[,1:4], X_g1[, 47:98])
Y_g1 <-  bladder.1[, 1]

X_g2 <-  lung.2[, -1]
Y_g2 <-  lung.2[, 1]

g_out1=CalibrateRegression(xdata=X_g1, ydata=Y_g1, Lambda=1:ncol(X_g1), family="binomial", 
                           implementation="GroupPLS", K=100)
t0=Sys.time()
g_out1=CalibrateRegression(xdata = X_g1, ydata = Y_g1, Lambda = 1:(length(Xgroups_1)+1), family = "binomial", 
                           implementation = "GroupPLS", ind.block.x = Xgroups_1, K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

t0=Sys.time()
g_out2=CalibrateRegression(xdata = X_g2, ydata = Y_g2, Lambda = 1:(length(Xgroups_2)+1), family = "binomial", 
                           implementation = "GroupPLS", ind.block.x = Xgroups_2, K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

matrix(NA, nrow=nrow(Lambda), ncol=1)

saveRDS(g_out1, "gPLS_stability_selection_lung.1.rds")

# Parameters calibrated by stability (number of variables and threshold in selection proportion)
print(GetArgmax(g_out1)) 
hat_params_g1 <- GetArgmax(g_out1)

# Calibrated selection proportions
print(CalibratedSelectionProportionsRegression(out))
selprop_g1 <- CalibratedSelectionProportionsRegression(g_out1, plot=FALSE)

# Stably selected variables
A_g1 <- CalibratedStableRegression(g_out1)

# Calibration plot
CalibrationPlot(g_out1) # 2-d version with lambda on the X-axis and threshold on the Y-axis
CalibrationPlot(g_out1, bi_dim=FALSE) # stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
# Use filename to save the plot as PDF (as commented line below)
# CalibrationPlot(out, filename=paste0(filepath, "Calibration_plot_unconstr_2d_",seed,".pdf"), width=14, height=7)

# Visualising selection proportion paths
stab_mat=out$selprop[,sort.list(apply(out$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat)=1:nrow(stab_mat)
pheatmap(stab_mat, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)

