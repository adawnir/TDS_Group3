
### TDS Project -- sgPLS stability selection (some plots + extract loading)
## Programme created by Vivian on 12 March

rm(list=ls())

project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/PLS"
setwd(project_path)

source("template/penalisation_functions.R")
source("template/functions.R")

suppressPackageStartupMessages(library(mixOmics))
suppressPackageStartupMessages(library(sgPLS))
suppressPackageStartupMessages(library(tidyverse))
install.packages("ggrepel")
suppressPackageStartupMessages(library(ggrepel))

##### load data
sg_out1 <- readRDS("../../Results/PLS/sgPLS_stability_selection_bladder.1.rds")
sg_out2 <- readRDS("../../Results/PLS/sgPLS_stability_selection_bladder.2.rds")
sg_out_1 <- readRDS("../../Results/PLS/sgPLS_stability_selection_lung.1.rds")
sg_out_2 <- readRDS("../../Results/PLS/sgPLS_stability_selection_lung.2.rds")

lung.1 <- readRDS("../../Results/denoised/lung.1_denoised.rds")
lung.2 <- readRDS("../../Results/denoised/lung.2_denoised.rds")
bladder.1 <- readRDS("../../Results/denoised/bladder.1_denoised.rds")
bladder.2 <- readRDS("../../Results/denoised/bladder.2_denoised.rds")


##### bladder.1 --
X_sg1 <-  bladder.1[, -1]
X_sg1 <- cbind(X_sg1[,5:46], X_sg1[,1:4], X_sg1[, 47:98])
Y_sg1 <-  bladder.1[, 1]

Xgroups_1 = c(18, 46, 54, 70)


### Calibration plot
CalibrationPlot(sg_out1, height = 7, width = 10,
                filename = "../../PLS_plots/sgPLS_CalibrationPlot_bladder.1.pdf")


### Selection proportions for best alpha over different numbers of groups--
mygroups=NULL
group_size=c(Xgroups_1[1], diff(c(Xgroups_1, ncol(X_sg1))))
for (i in 1:(length(group_size))){
  mygroups=c(mygroups,rep(i, group_size[i]))
}
names(mygroups)=colnames(X_sg1)
sg_new1 <- SeparateMatrix(sg_out1$selprop, groups=mygroups)
rownames(sg_new1)=1:nrow(sg_new1)
pheatmap(sg_new1, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, na_col="white",
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/sgPLS_heatmap_selection_bladder.1.pdf")


### Extracting average loadings from sgPLS-DA calibrated by stability--
hat_pi1=GetArgmax(sg_out1)[2]
myselprop1 <- sg_out1$selprop[which.max(sg_out1$S),]
myloadings1 <- sg_out1$Beta[which.max(sg_out1$S),,]
myloadings1 <- apply(myloadings1,1,FUN=function(x){mean(x[x!=0])})
myloadings1[is.na(myloadings1)] <- 0
myloadings1[which(!(names(myloadings1) %in% names(myselprop1)[myselprop1>=hat_pi1]))] <- 0

saveRDS(myloadings1, "sgPLS_loadings_bladder_1.rds")


### Figure with average loadings from sgPLS-DA calibrated by stability (in red if stably selected)--
par(mar=c(5,5,1,1))

plot(myloadings1, type="h", lwd=5, las=1, col=ifelse(myselprop1>=hat_pi1, yes="darkred", no="grey"),
     xlab="", ylab="Average Loadings Coefficients", cex.lab=1.5, xaxt="n")
abline(v=c(0,which(names(myloadings1)=="")), lty=3, col="grey")
for (k in 1:length(myloadings1)){
  if (names(myloadings1)[k]!=""){
    axis(side=1, at=k, labels=names(myloadings1)[k], las=2, col.axis=ifelse(myselprop1[k]>=hat_pi1, yes="darkred", no="black"))
  }
}


##### bladder.2 --
X_sg2 <-  bladder.2[, -1]
Y_sg2 <-  bladder.2[, 1]

Xgroups_2 = c(18, 42, 50, 66)


### Calibration plot
CalibrationPlot(sg_out2)
CalibrationPlot(sg_out2, height = 7, width = 10,
                filename = "../../PLS_plots/sgPLS_CalibrationPlot_bladder.2.pdf")


### Selection proportions for best alpha over different numbers of groups--
mygroups=NULL
group_size=c(Xgroups_2[1], diff(c(Xgroups_2, ncol(X_sg2))))
for (i in 1:(length(group_size))){
  mygroups=c(mygroups,rep(i, group_size[i]))
}
names(mygroups)=colnames(X_sg2)
sg_new2 <- SeparateMatrix(sg_out2$selprop, groups=mygroups)
rownames(sg_new2)=1:nrow(sg_new2)
pheatmap(sg_new2, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, na_col="white")
pheatmap(sg_new2, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, na_col="white",
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/sgPLS_heatmap_selection_bladder.2.pdf")


### Extracting average loadings from sgPLS-DA calibrated by stability--
hat_pi2=GetArgmax(sg_out2)[2]
myselprop2 <- sg_out2$selprop[which.max(sg_out2$S),]
myloadings2 <- sg_out2$Beta[which.max(sg_out2$S),,]
myloadings2 <- apply(myloadings2,1,FUN=function(x){mean(x[x!=0])})
myloadings2[is.na(myloadings2)] <- 0
myloadings2[which(!(names(myloadings2) %in% names(myselprop2)[myselprop2>=hat_pi2]))] <- 0

saveRDS(myloadings2, "sgPLS_loadings_bladder_2.rds")


### Figure with average loadings from sgPLS-DA calibrated by stability (in red if stably selected)--
par(mar=c(5,5,1,1))

plot(myloadings1, type="h", lwd=5, las=1, col=ifelse(myselprop1>=hat_pi1, yes="darkred", no="grey"),
     xlab="", ylab="Average Loadings Coefficients", cex.lab=1.5, xaxt="n")
abline(v=c(0,which(names(myloadings1)=="")), lty=3, col="grey")
for (k in 1:length(myloadings1)){
  if (names(myloadings1)[k]!=""){
    axis(side=1, at=k, labels=names(myloadings1)[k], las=2, col.axis=ifelse(myselprop1[k]>=hat_pi1, yes="darkred", no="black"))
  }
}



##### lung.1 --
X_sg_1 <-  lung.1[, -1]
X_sg_1 <- cbind(X_sg_1[,5:46], X_sg_1[,1:4], X_sg_1[, 47:98])
Y_sg_1 <-  lung.1[, 1]

Xgroups_1 = c(18, 46, 54, 70)


### Calibration plot
CalibrationPlot(sg_out_1, height = 7, width = 10,
                filename = "sgPLS_CalibrationPlot_lung.1.pdf")


### Selection proportions for best alpha over different numbers of groups--
mygroups=NULL
group_size=c(Xgroups_1[1], diff(c(Xgroups_1, ncol(X_sg_1))))
for (i in 1:(length(group_size))){
  mygroups=c(mygroups,rep(i, group_size[i]))
}
names(mygroups)=colnames(X_sg_1)
sg_new_1 <- SeparateMatrix(sg_out_1$selprop, groups=mygroups)
rownames(sg_new_1)=1:nrow(sg_new_1)
pheatmap(sg_new_1, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, na_col="white")
pheatmap(sg_new_1, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, na_col="white",
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/sgPLS_heatmap_selection_lung.1.pdf")


### Extracting average loadings from sgPLS-DA calibrated by stability--
hat_pi_1=GetArgmax(sg_out_1)[2]
myselprop_1 <- sg_out_1$selprop[which.max(sg_out_1$S),]
myloadings_1 <- sg_out_1$Beta[which.max(sg_out_1$S),,]
myloadings_1 <- apply(myloadings_1,1,FUN=function(x){mean(x[x!=0])})
myloadings_1[is.na(myloadings_1)] <- 0
myloadings_1[which(!(names(myloadings_1) %in% names(myselprop_1)[myselprop_1>=hat_pi_1]))] <- 0
myloadings_1 <- myloadings_1*(-1)

saveRDS(myloadings_1, "sgPLS_loadings_lung_1.rds")


### Figure with average loadings from sgPLS-DA calibrated by stability (in red if stably selected)--
par(mar=c(5,5,1,1))

plot(myloadings_1, type="h", lwd=5, las=1, col=ifelse(myselprop_1>=hat_pi_1, yes="darkred", no="grey"),
     xlab="", ylab="Average Loadings Coefficients", cex.lab=1.5, xaxt="n")
abline(v=c(0,which(names(myloadings_1)=="")), lty=3, col="grey")
for (k in 1:length(myloadings_1)){
  if (names(myloadings_1)[k]!=""){
    axis(side=1, at=k, labels=names(myloadings_1)[k], las=2, col.axis=ifelse(myselprop_1[k]>=hat_pi_1, yes="darkred", no="black"))
  }
}



##### lung.2 --
X_sg_2 <-  lung.2[, -1]
Y_sg_2 <-  lung.2[, 1]

Xgroups_2 = c(18, 42, 50, 66)


### Calibration plot
CalibrationPlot(sg_out_2, height = 7, width = 10)
CalibrationPlot(sg_out_2, height = 7, width = 10,
                filename = "../../PLS_plots/sgPLS_CalibrationPlot_lung.2.pdf")


### Selection proportions for best alpha over different numbers of groups--
mygroups=NULL
group_size=c(Xgroups_2[1], diff(c(Xgroups_2, ncol(X_sg_2))))
for (i in 1:(length(group_size))){
  mygroups=c(mygroups,rep(i, group_size[i]))
}
names(mygroups)=colnames(X_sg_2)
sg_new_2 <- SeparateMatrix(sg_out_2$selprop, groups=mygroups)
rownames(sg_new_2)=1:nrow(sg_new_2)
pheatmap(sg_new_2, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, na_col="white")
pheatmap(sg_new_2, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, na_col="white",
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/sgPLS_heatmap_selection_lung.2.pdf")


### Extracting average loadings from sgPLS-DA calibrated by stability--
hat_pi_2=GetArgmax(sg_out_2)[2]
myselprop_2 <- sg_out_2$selprop[which.max(sg_out_2$S),]
myloadings_2 <- sg_out_2$Beta[which.max(sg_out_2$S),,]
myloadings_2 <- apply(myloadings_2,1,FUN=function(x){mean(x[x!=0])})
myloadings_2[is.na(myloadings_2)] <- 0
myloadings_2[which(!(names(myloadings_2) %in% names(myselprop_2)[myselprop_2>=hat_pi_2]))] <- 0
myloadings_2 <- myloadings_2*(-1)

saveRDS(myloadings_2, "sgPLS_loadings_lung_2.rds")


### Figure with average loadings from sgPLS-DA calibrated by stability (in red if stably selected)--
par(mar=c(5,5,1,1))

plot(myloadings_2, type="h", lwd=5, las=1, col=ifelse(myselprop_2>=hat_pi_2, yes="darkred", no="grey"),
     xlab="", ylab="Average Loadings Coefficients", cex.lab=1.5, xaxt="n")
abline(v=c(0,which(names(myloadings_1)=="")), lty=3, col="grey")
for (k in 1:length(myloadings_1)){
  if (names(myloadings_1)[k]!=""){
    axis(side=1, at=k, labels=names(myloadings_1)[k], las=2, col.axis=ifelse(myselprop_1[k]>=hat_pi_1, yes="darkred", no="black"))
  }
}

