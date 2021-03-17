
### TDS Project -- sPLS stability selection on lung cancer (some plots + extract loading)
## Programme created by Vivian on 8 March

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
X_1 <- cbind(X_1[,5:46], X_1[,1:4], X_1[, 47:98])
Y_1 <-  lung.1[, 1]

t0=Sys.time()
out_1=CalibrateRegression(xdata = X_1, ydata = Y_1, Lambda=1:ncol(X_1), family="binomial", 
                        implementation="SparsePLS", K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(out_1, "sPLS_stability_selection_lung.1.rds")
out_1 <- readRDS("../../Results/PLS/sPLS_stability_selection_lung.1.rds")

##### Parameters calibrated by stability--
##### number of variables and threshold in selection proportion

#print(GetArgmax(out_1)) 
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
pdf("../../PLS_plots/sPLS_CalibrationPlot_lung.1.pdf", width=10, height=7)
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
         filename = "../../PLS_plots/sPLS_heatmap_selection_lung.1.pdf")
dev.off()


##### Checking consistency in sign of the loading coefficients for the variables with high selprop
hat_lambda_id_1 <- GetArgmaxId(out_1)[1]
selprop_1 <- CalibratedSelectionProportionsRegression(out_1, plot = F)
a_1 <- apply(out_1$Beta[hat_lambda_id_1,,],1,FUN=function(x){sum(x>0)})
b_1 <- apply(out_1$Beta[hat_lambda_id_1,,],1,FUN=function(x){sum(x<0)})

a_1 <- data.frame(Var = names(a_1), a_1)
b_1 <- data.frame(Var = names(b_1), b_1)
selprop_1 <- data.frame(Var = names(selprop_1), selprop_1)
check_1 <- merge(a_1, b_1, by = "Var") %>%
  merge(selprop_1, by = "Var") %>%
  mutate(pos_b = a_1/(a_1+b_1))

pdf("../../PLS_plots/sPLS_check_selprop_lung.1.pdf", width=10, height=7)
ggplot(check_1, aes(pos_b, selprop_1,
                      label = ifelse((selprop_1 >= 0.9 & pos_b>0.4 & pos_b<0.6),
                                     paste0(gsub(":", ":\n", Var),"\n",signif(abs(myloadings_1),3)),""))) +
  geom_point(color = ifelse((check_1$selprop_1>=0.9 & check_1$pos_b>0.4 & check_1$pos_b<0.6),"tomato","navy"),
             position = "jitter") +
  geom_text_repel(box.padding = 0.5, max.overlaps = Inf) +
  labs(title = "Base model")+
  xlab("Proportion of positive loading coefficient among loading coefficients")+
  ylab("Selection Proportion") +
  theme_bw()
dev.off()


##### Extract loadings--
hat_pi_1=GetArgmax(out_1)[2]
myselprop_1 = out_1$selprop[which.max(out_1$S),]
myloadings_1 = out_1$Beta[which.max(out_1$S),,]
myloadings_1 = apply(myloadings_1,1,FUN=function(x){mean(x[x!=0])})
myloadings_1[is.na(myloadings_1)] <- 0
myloadings_1[which(!(names(myloadings_1) %in% names(myselprop_1)[myselprop_1>=hat_pi_1]))] <- 0
myloadings_1 <- myloadings_1*(-1)

saveRDS(myloadings_1, "loadings_lung_1.rds")



##### Sparse PLS-DA--

X_2 <- lung.2[, -1]
Y_2 <- lung.2[, 1]

t0=Sys.time()
out_2 = CalibrateRegression(xdata = X_2, ydata = Y_2, Lambda=1:ncol(X_2), family="binomial", 
                         implementation="SparsePLS", K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(out_2, "sPLS_stability_selection_lung.2.rds")
out_2 <- readRDS("../../Results/PLS/sPLS_stability_selection_lung.2.rds")

##### Parameters calibrated by stability--
##### number of variables and threshold in selection proportion

#print(GetArgmax(out_2)) 
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
pdf("../../PLS_plots/sPLS_CalibrationPlot_lung.2.pdf", width=10, height=7)
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
         filename = "../../PLS_plots/sPLS_heatmap_selection_lung.2.pdf")
dev.off()


##### Checking consistency in sign of the loading coefficients for the variables with high selprop
hat_lambda_id_2 <- GetArgmaxId(out_2)[1]
selprop_2 <- CalibratedSelectionProportionsRegression(out_2)
a_2 <- apply(out_2$Beta[hat_lambda_id_2,,],1,FUN=function(x){sum(x>0)})
b_2 <- apply(out_2$Beta[hat_lambda_id_2,,],1,FUN=function(x){sum(x<0)})

a_2 <- data.frame(Var = names(a_2), a_2)
b_2 <- data.frame(Var = names(b_2), b_2)
selprop_2 <- data.frame(Var = names(selprop_2), selprop_2)
check_2 <- merge(a_2, b_2, by = "Var") %>%
  merge(selprop_2, by = "Var") %>%
  mutate(pos_b = a_2/(a_2+b_2))

pdf("../../PLS_plots/sPLS_check_selprop_lung.2.pdf", width=10, height=7)
ggplot(check_2, aes(pos_b, selprop_2,
                    label = ifelse((selprop_2 >= 0.9 & pos_b>0.4 & pos_b<0.6),
                                   paste0(gsub(":", ":\n", Var),"\n",signif(abs(myloadings_2),3)),""))) +
  geom_point(color = ifelse((check_2$selprop_2>=0.9 & check_2$pos_b>0.4 & check_2$pos_b<0.6),"tomato","navy"),
             position = "jitter") +
  geom_text_repel(box.padding = 0.5, max.overlaps = Inf) +
  labs(title = "Model adjusted on smoking status")+
  xlab("Proportion of positive loading coefficient among loading coefficients")+
  ylab("Selection Proportion") +
  theme_bw()
dev.off()


##### Extract loadings
hat_pi_2=GetArgmax(out_2)[2]
myselprop_2 = out_2$selprop[which.max(out_2$S),]
myloadings_2 = out_2$Beta[which.max(out_2$S),,]
myloadings_2 = apply(myloadings_2,1,FUN=function(x){mean(x[x!=0])})
myloadings_2[is.na(myloadings_2)] <- 0
myloadings_2[which(!(names(myloadings_2) %in% names(myselprop_2)[myselprop_2>=hat_pi_2]))] <- 0
myloadings_2 <- myloadings_2*(-1)

saveRDS(myloadings_2, "../PLS/loadings_lung_2.rds")

