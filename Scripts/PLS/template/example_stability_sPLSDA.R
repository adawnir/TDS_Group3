rm(list=ls())

setwd("~/Dropbox/Teaching/Computational_Epidemiology/2020-21/07-practical")

LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}

LoadPackages(c("pheatmap","glasso","glmnet","igraph","pROC","sgPLS"))
source("penalisation_functions.R")
source("~/Desktop/tosend_stability/functions.R")


# Data simulation
simul=SimulateXY(n=500, pk=100, family="binomial")
x=simul$X
y=simul$Y

# Sparse PLS-DA
t0=Sys.time()
out=CalibrateRegression(xdata=simul$X, ydata=simul$Y, Lambda=1:ncol(simul$X), family="binomial", 
                        implementation="SparsePLS", K=10)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

# Parameters calibrated by stability (number of variables and threshold in selection proportion)
print(GetArgmax(out)) 

# Calibrated selection proportions
print(CalibratedSelectionProportionsRegression(out))

# Stably selected variables
A=CalibratedStableRegression(out)

# Calibration plot
CalibrationPlot(out) # 2-d version with lambda on the X-axis and threshold on the Y-axis
CalibrationPlot(out, bi_dim=FALSE) # stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
# Use filename to save the plot as PDF (as commented line below)
# CalibrationPlot(out, filename=paste0(filepath, "Calibration_plot_unconstr_2d_",seed,".pdf"), width=14, height=7)

# Visualising selection proportion paths
stab_mat=out$selprop[,sort.list(apply(out$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat)=1:nrow(stab_mat)
pheatmap(stab_mat, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)

