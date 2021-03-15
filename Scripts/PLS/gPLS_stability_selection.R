
### TDS Project -- gPLS stability selection (some plots + extract loading)
## Programme created by Vivian on 11 March

rm(list=ls())

project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("template/penalisation_functions.R")
source("template/functions.R")

suppressPackageStartupMessages(library(mixOmics))
suppressPackageStartupMessages(library(sgPLS))
suppressPackageStartupMessages(library(tidyverse))
install.packages("ggrepel")
suppressPackageStartupMessages(library(ggrepel))

##### load data--

g_out_1 <- readRDS("../../Results/PLS/gPLS_stability_selection_lung.1.rds")
g_out_2 <- readRDS("../../Results/PLS/gPLS_stability_selection_lung.2.rds")
g_out1 <- readRDS("../../Results/PLS/gPLS_stability_selection_bladder.1.rds")
g_out2 <- readRDS("../../Results/PLS/gPLS_stability_selection_bladder.2.rds")

models=c("Base model", "Model adjusted on smoking status")

##### lung.1
## Parameters calibrated by stability (number of variables and threshold in selection proportion)
print(GetArgmax(g_out_1)) 
hat_params_g_1 <- GetArgmax(g_out_1)

## Calibrated selection proportions
#print(CalibratedSelectionProportionsRegression(g_out_1))
selprop_g_1 <- CalibratedSelectionProportionsRegression(g_out_1, plot=FALSE)

## Stably selected variables
A_g_1 <- CalibratedStableRegression(g_out_1)

## Selection proportions (not run)
pdf("../../PLS_plots/g_selection_proportions_bladder.1.pdf", width=10, height=7)
par(mar=c(14,5,1,1))
mycolours=ifelse(selprop_g_1 >= (1-hat_params_g_1[2]), yes=1, no=0) + ifelse(selprop_g_1 >= hat_params_g_1[2], yes=1, no=0)
plot(selprop_g_1, type="h", lwd=2, xaxt="n", las=1, ylim=c(0,1),
     xlab="", ylab="Selection Proportion", cex.lab=1.5,
     col=c("darkgrey", "skyblue2", "royalblue4")[mycolours+1])
abline(h=hat_params_g_1[2], col="red", lty=2)
abline(h=1-hat_params_g_1[2], col="red", lty=2)
for (i in 1:length(selprop_g_1)){
  axis(side=1, at=i, labels=names(selprop_g_1)[i], las=2, font = 2,
       col=ifelse(selprop_g_1[i]>= hat_params_g_1[2], yes="royalblue4", ifelse(selprop_g_1[i]  >= (1-hat_params_g_1[2]), yes="skyblue2", no="grey")), 
       col.axis=ifelse(selprop_g_1[i]>= hat_params_g_1[2], yes="royalblue4", ifelse(selprop_g_1[i]  >= (1-hat_params_g_1[2]), yes="skyblue2", no="grey")),
       cex.axis = 0.6)
}
dev.off()

## Calibration plot
pdf("../../PLS_plots/gPLS_CalibrationPlot_lung.1.pdf", width=10, height=7)
CalibrationPlot(g_out_1) # 2-d version with lambda on the X-axis and threshold on the Y-axis
dev.off()

pdf("../../PLS_plots/g_Cali_plot_bladder.1.pdf", width=10, height=7)
CalibrationPlot(g_out1, bi_dim=FALSE) # stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
dev.off()

## Visualising selection proportion paths
stab_mat_g_1=g_out_1$selprop[,sort.list(apply(g_out_1$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat_g_1)=1:nrow(stab_mat_g_1)
pheatmap(stab_mat_g_1, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, 
         height = 30, width = 60, fontsize = 25)
pheatmap(stab_mat_g_1, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, 
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/gPLS_heatmap_selection_lung.1.pdf")


## Checking consistency in sign of the beta coefficients for the variables with high selprop
hat_lambda_id_g_1 <- GetArgmaxId(g_out_1)[1]
selprop_g_1 <- CalibratedSelectionProportionsRegression(g_out_1)
a_g_1 <- apply(g_out_1$Beta[hat_lambda_id_g_1,,],1,FUN=function(x){sum(x>0)})
b_g_1 <- apply(g_out_1$Beta[hat_lambda_id_g_1,,],1,FUN=function(x){sum(x<0)})

a_g_1 <- data.frame(Var = names(a_g_1), a_g_1)
b_g_1 <- data.frame(Var = names(b_g_1), b_g_1)
selprop_g_1 <- data.frame(Var = names(selprop_g_1), selprop_g_1)
check_g_1 <- merge(a_g_1, b_g_1, by = "Var") %>%
  merge(selprop_g_1, by = "Var") %>%
  mutate(pos_b = a_g_1/(a_g_1+b_g_1))

pdf("../../PLS_plots/gPLS_check_selprop_lung.1.pdf", width=10, height=7)
ggplot(check_g_1, aes(pos_b, selprop_g_1,
                         label = ifelse((selprop_g_1 >= 0.9 & pos_b>0.4 & pos_b<0.6),
                                   paste0(gsub(":", ":\n", Var),"\n",signif(abs(myloadings_g_1),3)),""))) +
  geom_point(color = ifelse((check_g_1$selprop_g_1>=0.9 & check_g_1$pos_b>0.4 & check_g_1$pos_b<0.6),"tomato","navy"),
             position = "jitter") +
  ylim(0, 1.2) +
  geom_text_repel(box.padding = 0.5, max.overlaps = Inf) +
  labs(title = models[1])+
  xlab("Proportion of positive loading coefficient among loading coefficients")+
  ylab("Selection Proportion") +
  theme_bw()
dev.off()


## Extract loadings
hat_pi_g_1=GetArgmax(g_out_1)[2]
myselprop_g_1 = g_out_1$selprop[which.max(g_out_1$S),]
myloadings_g_1 = g_out_1$Beta[which.max(g_out_1$S),,]
myloadings_g_1 = apply(myloadings_g_1,1,FUN=function(x){mean(x[x!=0])})
myloadings_g_1[is.na(myloadings_g_1)] <- 0
myloadings_g_1[which(!(names(myloadings_g_1) %in% names(myselprop_g_1)[myselprop_g_1>=hat_pi_g_1]))] <- 0
myloadings_g_1 <- myloadings_g_1*(-1)

saveRDS(myloadings_g_1, "../../Results/PLS/gPLS_loadings_lung_1.rds")



##### lung.2
## Parameters calibrated by stability (number of variables and threshold in selection proportion)
print(GetArgmax(g_out_2)) 
hat_params_g_2 <- GetArgmax(g_out_2)

## Calibrated selection proportions
#print(CalibratedSelectionProportionsRegression(g_out_1))
selprop_g_2 <- CalibratedSelectionProportionsRegression(g_out_2, plot=FALSE)

## Stably selected variables
A_g_2 <- CalibratedStableRegression(g_out_2)

## Selection proportions (not run)
pdf("../../PLS_plots/g_selection_proportions_lung.2.pdf", width=10, height=7)
par(mar=c(14,5,1,1))
mycolours=ifelse(selprop_g_2 >= (1-hat_params_g_2[2]), yes=1, no=0) + ifelse(selprop_g_2 >= hat_params_g_2[2], yes=1, no=0)
plot(selprop_g_2, type="h", lwd=2, xaxt="n", las=1, ylim=c(0,1),
     xlab="", ylab="Selection Proportion", cex.lab=1.5,
     col=c("darkgrey", "skyblue2", "royalblue4")[mycolours+1])
abline(h=hat_params_g_2[2], col="red", lty=2)
abline(h=1-hat_params_g_2[2], col="red", lty=2)
for (i in 1:length(selprop_g_2)){
  axis(side=1, at=i, labels=names(selprop_g_2)[i], las=2, font = 2,
       col=ifelse(selprop_g_2[i]>= hat_params_g_2[2], yes="royalblue4", ifelse(selprop_g_2[i]  >= (1-hat_params_g_2[2]), yes="skyblue2", no="grey")), 
       col.axis=ifelse(selprop_g_2[i]>= hat_params_g_2[2], yes="royalblue4", ifelse(selprop_g_2[i]  >= (1-hat_params_g_2[2]), yes="skyblue2", no="grey")),
       cex.axis = 0.6)
}
dev.off()

## Calibration plot
pdf("../../PLS_plots/gPLS_CalibrationPlot_lung.2.pdf", width=10, height=7)
CalibrationPlot(g_out_2) # 2-d version with lambda on the X-axis and threshold on the Y-axis
dev.off()

pdf("../../PLS_plots/gPLS_Cali_plot_bladder.1.pdf", width=10, height=7)
CalibrationPlot(g_out2, bi_dim=FALSE) # stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
dev.off()

## Visualising selection proportion paths
stab_mat_g_2=g_out_2$selprop[,sort.list(apply(g_out_2$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat_g_2)=1:nrow(stab_mat_g_2)
pheatmap(stab_mat_g_2, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, 
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/gPLS_heatmap_selection_lung.2.pdf")

## Checking consistency in sign of the beta coefficients for the variables with high selprop
hat_lambda_id_g_2 <- GetArgmaxId(g_out_2)[1]
selprop_g_2 <- CalibratedSelectionProportionsRegression(g_out_2)
a_g_2 <- apply(g_out_2$Beta[hat_lambda_id_g_2,,],1,FUN=function(x){sum(x>0)})
b_g_2 <- apply(g_out_2$Beta[hat_lambda_id_g_2,,],1,FUN=function(x){sum(x<0)})

a_g_2 <- data.frame(Var = names(a_g_2), a_g_2)
b_g_2 <- data.frame(Var = names(b_g_2), b_g_2)
selprop_g_2 <- data.frame(Var = names(selprop_g_2), selprop_g_2)
check_g_2 <- merge(a_g_2, b_g_2, by = "Var") %>%
  merge(selprop_g_2, by = "Var") %>%
  mutate(pos_b = a_g_2/(a_g_2+b_g_2))

pdf("../../PLS_plots/gPLS_check_selprop_lung.2.pdf", width=10, height=7)
ggplot(check_g_2, aes(pos_b, selprop_g_2,
                      label = ifelse((selprop_g_2 >= 0.9 & pos_b>0.4 & pos_b<0.6),
                                     paste0(gsub(":", ":\n", Var),"\n",signif(abs(myloadings_g_2),3)),""))) +
  geom_point(color = ifelse((check_g_2$selprop_g_2>=0.9 & check_g_2$pos_b>0.4 & check_g_2$pos_b<0.6),"tomato","navy"),
             position = "jitter") +
  ylim(0, 1.2) +
  geom_text_repel(box.padding = 0.5, max.overlaps = Inf) +
  labs(title = models[2])+
  xlab("Proportion of positive loading coefficient among loading coefficients")+
  ylab("Selection Proportion") +
  theme_bw()
dev.off()



## Extract loadings
hat_pi_g_2=GetArgmax(g_out_2)[2]
myselprop_g_2 = g_out_2$selprop[which.max(g_out_2$S),]
myloadings_g_2 = g_out_2$Beta[which.max(g_out_2$S),,]
myloadings_g_2 = apply(myloadings_g_2,1,FUN=function(x){mean(x[x!=0])})
myloadings_g_2[is.na(myloadings_g_2)] <- 0
myloadings_g_2[which(!(names(myloadings_g_2) %in% names(myselprop_g_2)[myselprop_g_2>=hat_pi_g_2]))] <- 0
myloadings_g_2 <- myloadings_g_2*(-1)

saveRDS(myloadings_g_2, "../../Results/PLS/gPLS_loadings_lung_2.rds")



##### bladder.1 --
## Parameters calibrated by stability (number of variables and threshold in selection proportion)
print(GetArgmax(g_out1)) 
hat_params_g1 <- GetArgmax(g_out1)

## Calibrated selection proportions
#print(CalibratedSelectionProportionsRegression(g_out_1))
selprop_g1 <- CalibratedSelectionProportionsRegression(g_out1, plot=FALSE)

## Stably selected variables
A_g1 <- CalibratedStableRegression(g_out1)

## Selection proportions (not run)
pdf("../../PLS_plots/g_selection_proportions_bladder.1.pdf", width=10, height=7)
par(mar=c(14,5,1,1))
mycolours=ifelse(selprop_g1 >= (1-hat_params_g1[2]), yes=1, no=0) + ifelse(selprop_g1 >= hat_params_g1[2], yes=1, no=0)
plot(selprop_g1, type="h", lwd=2, xaxt="n", las=1, ylim=c(0,1),
     xlab="", ylab="Selection Proportion", cex.lab=1.5,
     col=c("darkgrey", "skyblue2", "royalblue4")[mycolours+1])
abline(h=hat_params_g1[2], col="red", lty=2)
abline(h=1-hat_params_g1[2], col="red", lty=2)
for (i in 1:length(selprop_g1)){
  axis(side=1, at=i, labels=names(selprop_g1)[i], las=2, font = 2,
       col=ifelse(selprop_g1[i]>= hat_params_g1[2], yes="royalblue4", ifelse(selprop_g1[i]  >= (1-hat_params_g1[2]), yes="skyblue2", no="grey")), 
       col.axis=ifelse(selprop_g1[i]>= hat_params_g1[2], yes="royalblue4", ifelse(selprop_g1[i]  >= (1-hat_params_g1[2]), yes="skyblue2", no="grey")),
       cex.axis = 0.6)
}
dev.off()

## Calibration plot
pdf("../../PLS_plots/gPLS_CalibrationPlot_bladder.1.pdf", width=10, height=7)
CalibrationPlot(g_out1) # 2-d version with lambda on the X-axis and threshold on the Y-axis
dev.off()

pdf("../../PLS_plots/g_Cali_plot_bladder.1.pdf", width=10, height=7)
CalibrationPlot(g_out1, bi_dim=FALSE) # stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
dev.off()

## Visualising selection proportion paths
stab_mat_g1=g_out1$selprop[,sort.list(apply(g_out1$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat_g1)=1:nrow(stab_mat_g1)
pheatmap(stab_mat_g1, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, 
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/gPLS_heatmap_selection_bladder.1.pdf")


## Checking consistency in sign of the beta coefficients for the variables with high selprop
hat_lambda_id_g1 <- GetArgmaxId(g_out1)[1]
selprop_g1 <- CalibratedSelectionProportionsRegression(g_out1)
a_g1 <- apply(g_out1$Beta[hat_lambda_id_g1,,],1,FUN=function(x){sum(x>0)})
b_g1 <- apply(g_out1$Beta[hat_lambda_id_g1,,],1,FUN=function(x){sum(x<0)})

a_g1 <- data.frame(Var = names(a_g1), a_g1)
b_g1 <- data.frame(Var = names(b_g1), b_g1)
selprop_g1 <- data.frame(Var = names(selprop_g1), selprop_g1)
check_g1 <- merge(a_g1, b_g1, by = "Var") %>%
  merge(selprop_g1, by = "Var") %>%
  mutate(pos_b = a_g1/(a_g1+b_g1))

pdf("../../PLS_plots/gPLS_check_selprop_bladder.1.pdf", width=10, height=7)
ggplot(check_g1, aes(pos_b, selprop_g1,
                     label = ifelse((selprop_g1 >= 0.83 & pos_b>0.4 & pos_b<0.6),
                                    paste0(gsub(":", ":\n", Var),"\n",signif(abs(myloadings_g1),3)),""))) +
  geom_point(color = ifelse((check_g1$selprop_g1>=0.83 & check_g1$pos_b>0.4 & check_g1$pos_b<0.6),"tomato","navy"),
             position = "jitter") +
  ylim(0, 1.2) +
  geom_text_repel(box.padding = 0.5, max.overlaps = Inf) +
  labs(title = models[1])+
  xlab("Proportion of positive loading coefficient among loading coefficients")+
  ylab("Selection Proportion") +
  theme_bw()
dev.off()


pdf("../../PLS_plots/gPLS_check_selprop_bladder.1.pdf", width=10, height=7)
par(mar=c(5,5,1,1))
plot(jitter(check_g1$a_g1/(check_g1$a_g1+check_g1$b_g1), 2), check_g1$selprop_g1, 
     las=1, pch=19, col="navy", cex.lab=1.5,
     xlab="Proportion of positive loading coefficients among non-zero loading coefficients", 
     ylab="Selection Proportion") 
dev.off()


## Extract loadings
hat_pi_g1=GetArgmax(g_out1)[2]
myselprop_g1 = g_out1$selprop[which.max(g_out1$S),]
myloadings_g1 = g_out1$Beta[which.max(g_out1$S),,]
myloadings_g1 = apply(myloadings_g1,1,FUN=function(x){mean(x[x!=0])})
myloadings_g1[is.na(myloadings_g1)] <- 0
myloadings_g1[which(!(names(myloadings_g1) %in% names(myselprop_g1)[myselprop_g1>=hat_pi_g1]))] <- 0

saveRDS(myloadings_g1, "../../Results/PLS/gPLS_loadings_bladder_1.rds")


##### bladder.2
## Parameters calibrated by stability (number of variables and threshold in selection proportion)
print(GetArgmax(g_out2)) 
hat_params_g2 <- GetArgmax(g_out2)

## Calibrated selection proportions
#print(CalibratedSelectionProportionsRegression(g_out_1))
selprop_g2 <- CalibratedSelectionProportionsRegression(g_out2, plot=FALSE)

## Stably selected variables
A_g2 <- CalibratedStableRegression(g_out2)

## Selection proportions (not run)
pdf("../../PLS_plots/g_selection_proportions_lung.2.pdf", width=10, height=7)
par(mar=c(14,5,1,1))
mycolours=ifelse(selprop_g2 >= (1-hat_params_g2[2]), yes=1, no=0) + ifelse(selprop_g2 >= hat_params_g2[2], yes=1, no=0)
plot(selprop_g2, type="h", lwd=2, xaxt="n", las=1, ylim=c(0,1),
     xlab="", ylab="Selection Proportion", cex.lab=1.5,
     col=c("darkgrey", "skyblue2", "royalblue4")[mycolours+1])
abline(h=hat_params_g2[2], col="red", lty=2)
abline(h=1-hat_params_g2[2], col="red", lty=2)
for (i in 1:length(selprop_g2)){
  axis(side=1, at=i, labels=names(selprop_g2)[i], las=2, font = 2,
       col=ifelse(selprop_g2[i]>= hat_params_g2[2], yes="royalblue4", ifelse(selprop_g2[i]  >= (1-hat_params_g2[2]), yes="skyblue2", no="grey")), 
       col.axis=ifelse(selprop_g2[i]>= hat_params_g2[2], yes="royalblue4", ifelse(selprop_g2[i]  >= (1-hat_params_g2[2]), yes="skyblue2", no="grey")),
       cex.axis = 0.6)
}
dev.off()

## Calibration plot
pdf("../../PLS_plots/gPLS_CalibrationPlot_bladder.2.pdf", width=10, height=7)
CalibrationPlot(g_out2) # 2-d version with lambda on the X-axis and threshold on the Y-axis
dev.off()

pdf("../../PLS_plots/gPLS_Cali_plot_bladder.2.pdf", width=10, height=7)
CalibrationPlot(g_out2, bi_dim=FALSE) # stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
dev.off()

##### Visualising selection proportion paths--
stab_mat_g2=g_out2$selprop[,sort.list(apply(g_out2$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat_g2)=1:nrow(stab_mat_g2)
pheatmap(stab_mat_g2, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, 
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/gPLS_heatmap_selection_bladder.2.pdf")

## Checking consistency in sign of the beta coefficients for the variables with high selprop
hat_lambda_id_g2 <- GetArgmaxId(g_out2)[1]
selprop_g2 <- CalibratedSelectionProportionsRegression(g_out2)
a_g2 <- apply(g_out2$Beta[hat_lambda_id_g2,,],1,FUN=function(x){sum(x>0)})
b_g2 <- apply(g_out2$Beta[hat_lambda_id_g2,,],1,FUN=function(x){sum(x<0)})

a_g2 <- data.frame(Var = names(a_g2), a_g2)
b_g2 <- data.frame(Var = names(b_g2), b_g2)
selprop_g2 <- data.frame(Var = names(selprop_g2), selprop_g2)
check_g2 <- merge(a_g2, b_g2, by = "Var") %>%
  merge(selprop_g2, by = "Var") %>%
  mutate(pos_b = a_g2/(a_g2+b_g2))

pdf("../../PLS_plots/gPLS_check_selprop_bladder.2.pdf", width=10, height=7)
ggplot(check_g2, aes(pos_b, selprop_g2,
                      label = ifelse((selprop_g2 >= 0.9 & pos_b>0.4 & pos_b<0.6),
                                     paste0(gsub(":", ":\n", Var),"\n",signif(abs(myloadings_g2),3)),""))) +
  geom_point(color = ifelse((check_g2$selprop_g2>=0.9 & check_g2$pos_b>0.4 & check_g2$pos_b<0.6),"tomato","navy"),
             position = "jitter") +
  ylim(0, 1.2) +
  geom_text_repel(box.padding = 0.5, max.overlaps = Inf) +
  labs(title = models[2])+
  xlab("Proportion of positive loading coefficient among loading coefficients")+
  ylab("Selection Proportion") +
  theme_bw()
dev.off()



## Extract loadings
hat_pi_g2=GetArgmax(g_out2)[2]
myselprop_g2 = g_out2$selprop[which.max(g_out2$S),]
myloadings_g2 = g_out2$Beta[which.max(g_out2$S),,]
myloadings_g2 = apply(myloadings_g2,1,FUN=function(x){mean(x[x!=0])})
myloadings_g2[is.na(myloadings_g2)] <- 0
myloadings_g2[which(!(names(myloadings_g2) %in% names(myselprop_g2)[myselprop_g2>=hat_pi_g2]))] <- 0

saveRDS(myloadings_g2, "../../Results/PLS/gPLS_loadings_bladder_2.rds")

