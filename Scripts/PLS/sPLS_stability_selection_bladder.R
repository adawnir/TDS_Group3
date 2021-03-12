
### TDS Project -- sPLS stability selection

rm(list=ls())

source("template/penalisation_functions.R")
source("template/functions.R")


##### load data
bladder.1 <- readRDS("../../Results/denoised/bladder.1_denoised.rds")
bladder.2 <- readRDS("../../Results/denoised/bladder.2_denoised.rds")


##### Sparse PLS-DA--

X1 <-  bladder.1[, -1]
X1 <- cbind(X1[,5:47], X1[,1:4], X1[, 48:98])
Y1 <-  bladder.1[, 1]

t0=Sys.time()
out1=CalibrateRegression(xdata = X1, ydata = Y1, Lambda=1:ncol(X1), family="binomial", 
                         implementation="SparsePLS", K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(out1, "../../Results/sPLS_stability_selection_bladder.1.rds")

##### Parameters calibrated by stability--
##### number of variables and threshold in selection proportion

#print(GetArgmax(out)) 
hat_params1 = GetArgmax(out1)

## Calibrated selection proportions

#print(CalibratedSelectionProportionsRegression(out))
selprop1 <- CalibratedSelectionProportionsRegression(out1, plot=FALSE)

## Stably selected variables
A1 = CalibratedStableRegression(out1)


##### Visualisation of selection proportions--
png(filename = "selection_proportions_bladder.1.png", width = 1200, height = 900)
par(mar=c(15,5,1,1))
mycolours=ifelse(selprop1 >= (1-hat_params1[2]), yes=1, no=0) + ifelse(selprop1 >= hat_params1[2], yes=1, no=0)
plot(selprop1, type="h", lwd=2, xaxt="n", las=1, ylim=c(0,1),
     xlab="", ylab="Selection Proportion", cex.lab=1.5,
     col=c("darkgrey", "skyblue2", "royalblue4")[mycolours+1])
abline(h=hat_params1[2], col="red", lty=2)
abline(h=1-hat_params1[2], col="red", lty=2)
for (i in 1:length(selprop1)){
  axis(side=1, at=i, labels=names(selprop1)[i], las=2, font = 2,
       col=ifelse(selprop1[i]>= hat_params1[2], yes="royalblue4", ifelse(selprop1[i]  >= (1-hat_params1[2]), yes="skyblue2", no="grey")), 
       col.axis=ifelse(selprop1[i]>= hat_params1[2], yes="royalblue4", ifelse(selprop1[i]  >= (1-hat_params1[2]), yes="skyblue2", no="grey")))
}
dev.off()


##### Calibration plot--
png(filename = "../../PLS_plots/CalibrationPlot_bladder.1.png", width = 1200, height = 900)
CalibrationPlot(out1) # 2-d version with lambda on the X-axis and threshold on the Y-axis
dev.off()

png(filename = "../../PLS_plots/Cali_plot_bladder.1.png", width = 1200, height = 900)
CalibrationPlot(out1, bi_dim=FALSE) # stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
dev.off()


##### Visualising selection proportion paths--
stab_mat1=out1$selprop[,sort.list(apply(out1$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat1)=1:nrow(stab_mat1)
pheatmap(stab_mat1, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
pheatmap(stab_mat1, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, 
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/heatmap_selection_bladder.1.pdf")
dev.off()




##### Sparse PLS-DA--

X2 <- bladder.2[, -1]
Y2 <- bladder.2[, 1]

t0=Sys.time()
out2 = CalibrateRegression(xdata = X2, ydata = Y2, Lambda=1:ncol(X2), family="binomial", 
                           implementation="SparsePLS", K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(out2, "../../Results/sPLS_stability_selection_bladder.2.rds")

##### Parameters calibrated by stability--
##### number of variables and threshold in selection proportion

#print(GetArgmax(out)) 
hat_params2 = GetArgmax(out2)

## Calibrated selection proportions

#print(CalibratedSelectionProportionsRegression(out))
selprop2 <- CalibratedSelectionProportionsRegression(out2, plot=FALSE)

## Stably selected variables
A2 = CalibratedStableRegression(out2)


##### Visualisation of selection proportions--
png(filename = "selection_proportions_bladder.2.png", width = 1200, height = 900)
par(mar=c(14,5,1,1))
mycolours=ifelse(selprop2 >= (1-hat_params2[2]), yes=1, no=0) + ifelse(selprop2 >= hat_params2[2], yes=1, no=0)
plot(selprop2, type="h", lwd=2, xaxt="n", las=1, ylim=c(0,1),
     xlab="", ylab="Selection Proportion", cex.lab=1.5,
     col=c("darkgrey", "skyblue2", "royalblue4")[mycolours+1])
abline(h=hat_params2[2], col="red", lty=2)
abline(h=1-hat_params2[2], col="red", lty=2)
for (i in 1:length(selprop2)){
  axis(side=1, at=i, labels=names(selprop2)[i], las=2, font = 2,
       col=ifelse(selprop2[i]>= hat_params2[2], yes="royalblue4", ifelse(selprop2[i]  >= (1-hat_params2[2]), yes="skyblue2", no="grey")), 
       col.axis=ifelse(selprop2[i]>= hat_params2[2], yes="royalblue4", ifelse(selprop2[i]  >= (1-hat_params2[2]), yes="skyblue2", no="grey")))
}
dev.off()


##### Calibration plot--
png(filename = "../../PLS_plots/CalibrationPlot_bladder.2.png", width = 1200, height = 900)
CalibrationPlot(out2) # 2-d version with lambda on the X-axis and threshold on the Y-axis
dev.off()

png(filename = "../../PLS_plots/Cali_plot_bladder.2.png", width = 1200, height = 900)
CalibrationPlot(out2, bi_dim=FALSE) # stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
dev.off()


##### Visualising selection proportion paths--
stab_mat2=out2$selprop[,sort.list(apply(out2$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat2)=1:nrow(stab_mat2)
pheatmap(stab_mat2, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
pheatmap(stab_mat2, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, 
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/heatmap_selection_bladder.2.pdf")
dev.off()
