
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
CalibrationPlot(sg_out1)


### Selection proportions for best alpha over different numbers of groups--
mygroups=NULL
group_size=c(Xgroups_1[1], diff(c(Xgroups_1, ncol(X_sg1))))
for (i in 1:(length(group_size))){
  mygroups=c(mygroups,rep(i, group_size[i]))
}
names(mygroups)=colnames(X_sg1)
new=SeparateMatrix(sg_out1$selprop, groups=mygroups)
rownames(new)=1:nrow(new)
pheatmap(new, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, na_col="white")


### Extracting average loadings from sgPLS-DA calibrated by stability--
myselprop=out$selprop[which.max(out$S),]
myloadings=out$Beta[which.max(out$S),,]
myloadings=apply(myloadings,1,FUN=function(x){mean(x[x!=0])})
myloadings[is.na(myloadings)]=0
myloadings=myloadings[colnames(new)]
names(myloadings)[is.na(names(myloadings))]=""
myselprop=myselprop[names(myloadings)]


### Figure with average loadings from sgPLS-DA calibrated by stability (in red if stably selected)--
par(mar=c(5,5,1,1))
hat_pi=GetArgmax(out)[2]
plot(myloadings, type="h", lwd=5, las=1, col=ifelse(myselprop>=hat_pi, yes="darkred", no="grey"),
     xlab="", ylab="Average Loadings Coefficients", cex.lab=1.5, xaxt="n")
abline(v=c(0,which(names(myloadings)=="")), lty=3, col="grey")
for (k in 1:length(myloadings)){
  if (names(myloadings)[k]!=""){
    axis(side=1, at=k, labels=names(myloadings)[k], las=2, col.axis=ifelse(myselprop[k]>=hat_pi, yes="darkred", no="black"))
  }
}

