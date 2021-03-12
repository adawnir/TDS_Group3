
### TDS Project -- sPLS stability selection

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


##### Sparse PLS-DA--

X_1 <-  lung.1[, -1]
X_1 <- cbind(X_1[,5:47], X_1[,1:4], X_1[, 48:98])
Y_1 <-  lung.1[, 1]

t0=Sys.time()
out_1=CalibrateRegression(xdata = X_1, ydata = Y_1, Lambda=1:ncol(X_1), family="binomial", 
                        implementation="SparsePLS", K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(out_1, "sPLS_stability_selection_lung.1.rds")

##### Parameters calibrated by stability--
##### number of variables and threshold in selection proportion

#print(GetArgmax(out)) 
hat_params_1 = GetArgmax(out_1)

## Calibrated selection proportions

#print(CalibratedSelectionProportionsRegression(out))
selprop_1 <- CalibratedSelectionProportionsRegression(out_1, plot=FALSE)

## Stably selected variables
A_1 = CalibratedStableRegression(out_1)


##### Visualisation of selection proportions--
png(filename = "../../PLS_plots/selection_proportions_lung.1.png", width = 1200, height = 900)
par(mar=c(15,5,1,1))
mycolours=ifelse(selprop_1 >= (1-hat_params_1[2]), yes=1, no=0) + ifelse(selprop_1 >= hat_params_1[2], yes=1, no=0)
plot(selprop_1, type="h", lwd=2, xaxt="n", las=1, ylim=c(0,1),
     xlab="", ylab="Selection Proportion", cex.lab=1.5,
     col=c("darkgrey", "skyblue2", "royalblue4")[mycolours+1])
abline(h=hat_params_1[2], col="red", lty=2)
abline(h=1-hat_params_1[2], col="red", lty=2)
for (i in 1:length(selprop_1)){
  axis(side=1, at=i, labels=names(selprop_1)[i], las=2, font = 2,
       col=ifelse(selprop_1[i]>= hat_params_1[2], yes="royalblue4", ifelse(selprop_1[i]  >= (1-hat_params_1[2]), yes="skyblue2", no="grey")), 
       col.axis=ifelse(selprop_1[i]>= hat_params_1[2], yes="royalblue4", ifelse(selprop_1[i]  >= (1-hat_params_1[2]), yes="skyblue2", no="grey")))
}
dev.off()


##### Calibration plot--
png(filename = "../../PLS_plots/CalibrationPlot_lung.1.png", width = 1200, height = 900)
CalibrationPlot(out_1) # 2-d version with lambda on the X-axis and threshold on the Y-axis
dev.off()

png(filename = "../../PLS_plots/Cali_plot_lung.1.png", width = 1200, height = 900)
CalibrationPlot(out_1, bi_dim=FALSE) # stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
dev.off()


##### Visualising selection proportion paths--
stab_mat_1=out_1$selprop[,sort.list(apply(out_1$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat_1)=1:nrow(stab_mat_1)
pheatmap(stab_mat_1, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
pheatmap(stab_mat_1, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, 
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/heatmap_selection_lung.1.pdf")
dev.off()




##### Sparse PLS-DA--

X_2 <- lung.2[, -1]
Y_2 <- lung.2[, 1]

t0=Sys.time()
out_2 = CalibrateRegression(xdata = X_2, ydata = Y_2, Lambda=1:ncol(X_2), family="binomial", 
                         implementation="SparsePLS", K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(out_2, "sPLS_stability_selection_lung.2.rds")

##### Parameters calibrated by stability--
##### number of variables and threshold in selection proportion

#print(GetArgmax(out)) 
hat_params_2 = GetArgmax(out_2)

## Calibrated selection proportions

#print(CalibratedSelectionProportionsRegression(out))
selprop_2 <- CalibratedSelectionProportionsRegression(out_2, plot=FALSE)

## Stably selected variables
A_2 = CalibratedStableRegression(out_2)


##### Visualisation of selection proportions--
png(filename = "../../PLS_plots/selection_proportions_lung.2.png", width = 1200, height = 900)
par(mar=c(15,5,1,1))
mycolours=ifelse(selprop_2 >= (1-hat_params_2[2]), yes=1, no=0) + ifelse(selprop_2 >= hat_params_2[2], yes=1, no=0)
plot(selprop_2, type="h", lwd=2, xaxt="n", las=1, ylim=c(0,1),
     xlab="", ylab="Selection Proportion", cex.lab=1.5,
     col=c("darkgrey", "skyblue2", "royalblue4")[mycolours+1])
abline(h=hat_params_2[2], col="red", lty=2)
abline(h=1-hat_params_2[2], col="red", lty=2)
for (i in 1:length(selprop_2)){
  axis(side=1, at=i, labels=names(selprop_2)[i], las=2, font = 2,
       col=ifelse(selprop_2[i]>= hat_params_2[2], yes="royalblue4", ifelse(selprop_2[i]  >= (1-hat_params_2[2]), yes="skyblue2", no="grey")), 
       col.axis=ifelse(selprop_2[i]>= hat_params_2[2], yes="royalblue4", ifelse(selprop_2[i]  >= (1-hat_params_2[2]), yes="skyblue2", no="grey")))
}
dev.off()


##### Calibration plot--
png(filename = "../../PLS_plots/CalibrationPlot_lung.2.png", width = 1200, height = 900)
CalibrationPlot(out_2) # 2-d version with lambda on the X-axis and threshold on the Y-axis
dev.off()

png(filename = "../../PLS_plots/Cali_plot_lung.2.png", width = 1200, height = 900)
CalibrationPlot(out_2, bi_dim=FALSE) # stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
dev.off()


##### Visualising selection proportion paths--
stab_mat_2=out_2$selprop[,sort.list(apply(out_2$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat_2)=1:nrow(stab_mat_2)
pheatmap(stab_mat_2, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
pheatmap(stab_mat_2, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, 
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/heatmap_selection_lung.2.pdf")
dev.off()
