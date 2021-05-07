### TDS Project -- Multivariate analyses Prediction Performance
## Programme created by Rin Wada on 25 March

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

### Loading packages----
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(pROC)

source("penalisation_functions.R")

### Plot labels----
plot_annot=read_csv("../Dictionaries/plot_annot.csv")

### Load outputs ----
lasso_forced_hat_params=NULL
for (m in 1:2){
  arr=paste0(c("lung","bladder"),".0")[m]
  ### Lasso
  lasso_forced_out=readRDS(paste0("../Results/lasso_forced/out_",arr,".rds")) # Load output
  assign(paste0("lasso_forced_out_",arr),lasso_forced_out) # Assign name
  lasso_forced_hat_params=rbind(lasso_forced_hat_params, GetArgmax(lasso_forced_out)) # Extract calibrated pi
  lasso_forced_calib=sum(CalibratedStableRegression(lasso_forced_out)) # Number of selected variables
  assign(paste0("lasso_forced_calib_",arr),lasso_forced_calib) # Assign name
  # lasso_forced_auc_avg=readRDS(paste0("../Results/lasso_forced/auc_average_beta_",arr,".rds")) # Load auc (average beta)
  # assign(paste0("lasso_forced_auc_avg_",arr),lasso_forced_auc_avg) # Assign name
  lasso_forced_roc=readRDS(paste0("../Results/lasso_forced/roc_recalib_beta_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("lasso_forced_roc_",arr),lasso_forced_roc) # Assign name
  lasso_forced_auc=readRDS(paste0("../Results/lasso_forced/auc_recalib_beta_",arr,".rds")) # Load auc (recalibrated)
  assign(paste0("lasso_forced_auc_",arr),lasso_forced_auc) # Assign name
}

for (m in 1:4){
  arr=paste0(rep(c("lung","bladder"),each=2),".",1:2)[m]
  ### Lasso
  lasso_out=readRDS(paste0("../Results/lasso/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_calib=sum(CalibratedStableRegression(lasso_out)) # Number of selected variables
  assign(paste0("lasso_calib_",arr),lasso_calib) # Assign name
  # lasso_auc_avg=readRDS(paste0("../Results/lasso/auc_average_beta_",arr,".rds")) # Load auc (average beta)
  # assign(paste0("lasso_auc_avg_",arr),lasso_auc_avg) # Assign name
  lasso_roc=readRDS(paste0("../Results/lasso/roc_recalib_beta_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("lasso_roc_",arr),lasso_roc) # Assign name
  lasso_auc=readRDS(paste0("../Results/lasso/auc_recalib_beta_",arr,".rds")) # Load auc (recalibrated)
  assign(paste0("lasso_auc_",arr),lasso_auc) # Assign name
  
  arrTT=paste0(rep(c("lung_TT","bladder_TT"),each=2),".",1:2)[m]
  ### sPLS
  spls_out=readRDS(paste0("../Results/PLS_319/train_test_spls/sPLS_out_",arrTT,".rds")) # Load output
  assign(paste0("spls_out_",arr),spls_out) # Assign name
  spls_calib=sum(CalibratedStableRegression(spls_out)) # Number of selected variables
  assign(paste0("spls_calib_",arr),spls_calib) # Assign name
  spls_roc=readRDS(paste0("../Results/PLS_319/train_test_spls/sPLS_roc_recali_",arrTT,".rds")) # Load roc (recalibrated)
  assign(paste0("spls_roc_",arr),spls_roc) # Assign name
}

### Sensitivity analysis LASSO----

## Check parameters
rownames(lasso_forced_hat_params)=arr=paste0(c("lung","bladder"),".0")
print(lasso_forced_hat_params)

## Global settings
mycolours=rep(c("grey50","tomato","forestgreen","royalblue","gold"),
              times=c(20,25,8,16,28))
mycolours_lab=darken(mycolours, amount=0.5)
colnames=names(lasso_forced_out_lung.0$Beta[1,,1])
myorder=c(colnames[1:2],colnames[12:29],colnames[3:7],colnames[30:45],colnames[8:11],
          colnames[46:length(colnames)])
names(mycolours)=myorder
names(mycolours_lab)=myorder
point.lab=plot_annot$label.point
names(point.lab)=c(myorder)

## Recalibrated beta: Lung
auc=lasso_forced_auc_lung.0
calib=lasso_forced_calib_lung.0
file_path="lasso_forced_auc_recalib_lung"

# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Final/Supplementary/",file_path,".pdf"), width=10, height=5)
  par(mar=c(11,5,1,1))
  plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
         li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
         xaxt="n", xlab="", pch=19, col=mycolours[rownames(auc)], cex=0.5,
         ylab="AUC", ylim=c(0.4,1))
  for (k in 1:length(auc[,2])){
    mytext=point.lab[rownames(auc)[k]]
    labcol=mycolours_lab[rownames(auc)[k]]
    axcol=mycolours[rownames(auc)[k]]
    labfont=ifelse(k==calib,2,1)
    axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
         col.axis=labcol, col=axcol, font=labfont)
  }
  abline(v=calib, col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  abline(v=11, col="black", lty=3, lwd=1)
  abline(h=as.numeric(auc[11,2]), col="black", lty=3, lwd=1)
  abline(h=as.numeric(auc[calib,2]), col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  legend("bottomright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = c(paste0("Calibrated model; AUC = ",auc[calib,2]),
                    paste0("Emplty model; AUC = ",auc[11,2])))
  dev.off()
}

## Recalibrated beta: Bladder
auc=lasso_forced_auc_bladder.0
calib=lasso_forced_calib_bladder.0
file_path="lasso_forced_auc_recalib_bladder"
{pdf(paste0("../Figures/Final/Supplementary/",file_path,".pdf"), width=10, height=5)
  par(mar=c(11,5,1,1))
  plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
         li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
         xaxt="n", xlab="", pch=19, col=mycolours[rownames(auc)], cex=0.5,
         ylab="AUC", ylim=c(0.4,1))
  for (k in 1:length(auc[,2])){
    mytext=point.lab[rownames(auc)[k]]
    labcol=mycolours_lab[rownames(auc)[k]]
    axcol=mycolours[rownames(auc)[k]]
    labfont=ifelse(k==calib,2,1)
    axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
         col.axis=labcol, col=axcol, font=labfont)
  }
  abline(v=calib, col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  abline(v=11, col="black", lty=3, lwd=1)
  abline(h=as.numeric(auc[11,2]), col="black", lty=3, lwd=1)
  abline(h=as.numeric(auc[calib,2]), col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  legend("bottomright", lty=c(4,3), lwd=1, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = c(paste0("Calibrated model; AUC = ",auc[calib,2]),
                    paste0("Emplty model; AUC = ",auc[11,2])))
  dev.off()
}

### Main analysis LASSO ----
plot_annot=plot_annot[-c(1,2,21:25),] # Remove age, sex and BMI

## Global settings
mycolours=rep(c("grey50","tomato","forestgreen","royalblue","gold"),
              times=c(18,20,8,16,28))
mycolours_lab=darken(mycolours, amount=0.5)
colnames=names(lasso_out_lung.1$Beta[1,,1])
myorder=c(colnames[5:38],colnames[1:4],colnames[39:length(colnames)])
names(mycolours)=c(myorder)
names(mycolours_lab)=c(myorder)
point.lab=plot_annot$label.point
names(point.lab)=c(myorder)

## Recalibrate beta: Lung
auc=lasso_auc_lung.1
calib=lasso_calib_lung.1
file_path="lasso_auc_recalib_lung.1"

# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Final/Supplementary/",file_path,".pdf"), width=10, height=5)
  par(mar=c(11,5,1,1))
  plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
         li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
         xaxt="n", xlab="", pch=19, col=mycolours[rownames(auc)], cex=0.5,
         ylab="AUC", ylim=c(0.4,1))
  for (k in 1:length(auc[,2])){
    mytext=point.lab[rownames(auc)[k]]
    labcol=mycolours_lab[rownames(auc)[k]]
    axcol=mycolours[rownames(auc)[k]]
    labfont=ifelse(k==calib,2,1)
    axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
         col.axis=labcol, col=axcol, font=labfont)
  }
  abline(v=calib, col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  abline(h=as.numeric(auc[calib,2]), col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  legend("bottomright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
  dev.off()
}

auc=lasso_auc_lung.2
calib=lasso_calib_lung.2
file_path="lasso_auc_recalib_lung.2"

# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Final/Supplementary/",file_path,".pdf"), width=10, height=5)
  par(mar=c(11,5,1,1))
  plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
         li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
         xaxt="n", xlab="", pch=19, col=mycolours[rownames(auc)], cex=0.5,
         ylab="AUC", ylim=c(0.4,1))
  for (k in 1:length(auc[,2])){
    mytext=point.lab[rownames(auc)[k]]
    labcol=mycolours_lab[rownames(auc)[k]]
    axcol=mycolours[rownames(auc)[k]]
    labfont=ifelse(k==calib,2,1)
    axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
         col.axis=labcol, col=axcol, font=labfont)
  }
  abline(v=calib, col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  abline(h=as.numeric(auc[calib,2]), col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  legend("bottomright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
  dev.off()
}

## Recalibrated beta: Bladder
auc=lasso_auc_bladder.1
calib=lasso_calib_bladder.1
file_path="lasso_auc_recalib_bladder.1"

# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Final/Supplementary/",file_path,".pdf"), width=10, height=5)
  par(mar=c(11,5,1,1))
  plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
         li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
         xaxt="n", xlab="", pch=19, col=mycolours[rownames(auc)], cex=0.5,
         ylab="AUC", ylim=c(0.4,1))
  for (k in 1:length(auc[,2])){
    mytext=point.lab[rownames(auc)[k]]
    labcol=mycolours_lab[rownames(auc)[k]]
    axcol=mycolours[rownames(auc)[k]]
    labfont=ifelse(k==calib,2,1)
    axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
         col.axis=labcol, col=axcol, font=labfont)
  }
  abline(v=calib, col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  abline(h=as.numeric(auc[calib,2]), col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  legend("bottomright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
  dev.off()
}

auc=lasso_auc_bladder.2
calib=lasso_calib_bladder.2
file_path="lasso_auc_recalib_bladder.2"

# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Final/Supplementary/",file_path,".pdf"), width=10, height=5)
  par(mar=c(11,5,1,1))
  plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
         li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
         xaxt="n", xlab="", pch=19, col=mycolours[rownames(auc)], cex=0.5,
         ylab="AUC", ylim=c(0.4,1))
  for (k in 1:length(auc[,2])){
    mytext=point.lab[rownames(auc)[k]]
    labcol=mycolours_lab[rownames(auc)[k]]
    axcol=mycolours[rownames(auc)[k]]
    labfont=ifelse(k==calib,2,1)
    axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
         col.axis=labcol, col=axcol, font=labfont)
  }
  abline(v=calib, col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  abline(h=as.numeric(auc[calib,2]), col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  legend("bottomright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
  dev.off()
}

### Main analyses: LASSO vs sPLS ----

# Calculate CI for ROC coordinates
foo = function(l0, l1, l2, s1, s2){
  l0.coord = data.frame(fpr=1-l0$specificities,tpr=l0$sensitivities)
  l1.coord = data.frame(fpr=1-l1$specificities,tpr=l1$sensitivities)
  l2.coord = data.frame(fpr=1-l2$specificities,tpr=l2$sensitivities)
  s1.coord = data.frame(fpr=1-s1$specificities,tpr=s1$sensitivities)
  s2.coord = data.frame(fpr=1-s2$specificities,tpr=s2$sensitivities)
  x=list(l0.coord, l1.coord, l2.coord, s1.coord, s2.coord)
  names(x) = c("l0","l1","l2","s1","s2")
  return(x)
}
# foo.ci = function(l0, l1, l2, s1, s2){
#   l0.ci = cbind(seq(1, 0, -0.01),ci.se(l0, seq(1, 0, -0.01), boot.n=1000)[,-2])
#   l1.ci = cbind(seq(1, 0, -0.01),ci.se(l1, seq(1, 0, -0.01), boot.n=1000)[,-2])
#   l2.ci = cbind(seq(1, 0, -0.01),ci.se(l2,seq(1, 0, -0.01), boot.n=1000)[,-2])
#   s1.ci = cbind(seq(1, 0, -0.01),ci.se(s1, seq(1, 0, -0.01), boot.n=1000)[,-2])
#   s2.ci = cbind(seq(1, 0, -0.01),ci.se(s2, seq(1, 0, -0.01),boot.n=1000)[,-2])
#   x=list(l0.ci, l1.ci, l2.ci, s1.ci, s2.ci)
#   names(x) = c("l0","l1","l2","s1","s2")
#   return(x)
# }

## Lung
roc_lung=foo(lasso_forced_roc_lung.0, lasso_roc_lung.1, lasso_roc_lung.2,
             spls_roc_lung.1, spls_roc_lung.2)
# ci_lung=foo.ci(lasso_forced_roc_lung.0,lasso_roc_lung.1, lasso_roc_lung.2,
#                spls_roc_lung.1, spls_roc_lung.2)

## Bladder
roc_bladder=foo(lasso_forced_roc_bladder.0, lasso_roc_bladder.1, lasso_roc_bladder.2,
             spls_roc_bladder.1, spls_roc_bladder.2)
# ci_lung=foo.ci(lasso_forced_roc_bladder.0, lasso_roc_bladder.1, lasso_roc_bladder.2,
#                spls_roc_bladder.1, spls_roc_bladder.2)

# Calculate AUC and confidence intervals
foo2 = function(l0, l1, l2, s1, s2, l0_calib, l1_calib, l2_calib){
  l0 = round(as.numeric(l0[l0_calib,]),2)
  l1 = round(as.numeric(l1[l1_calib,]),2)
  l2 = round(as.numeric(l2[l2_calib,]),2)
  s1 = round(as.numeric(ci.auc(s1)),2)
  s2 = round(as.numeric(ci.auc(s2)),2)
  x=c(paste0(l0[2]," [",l0[1],"-",l0[3],"]"),
      paste0(l1[2]," [",l1[1],"-",l1[3],"]"),
      paste0(l2[2]," [",l2[1],"-",l2[3],"]"),
      paste0(s1[2]," [",s1[1],"-",s1[3],"]"),
      paste0(s2[2]," [",s2[1],"-",s2[3],"]"))
  
}

## Lung
auc_lung=foo2(lasso_forced_auc_lung.0, lasso_auc_lung.1, lasso_auc_lung.2,
              spls_roc_lung.1, spls_roc_lung.2,
              lasso_forced_calib_lung.0,
              lasso_calib_lung.1, lasso_calib_lung.2)

## Bladder
auc_bladder=foo2(lasso_forced_auc_bladder.0, lasso_auc_bladder.1, lasso_auc_bladder.2,
                 spls_roc_bladder.1, spls_roc_bladder.2,
                 lasso_forced_calib_bladder.0,
                 lasso_calib_bladder.1, lasso_calib_bladder.2)
              
mycolours = c("grey","navy","red","forestgreen","orange")

## LASSO
# Lung
{pdf("../Figures/Final/Main/roc_lung.pdf", width = 7, height = 7)
  par(mar=c(5,5,1,1))
  plot(roc_lung[[1]], ylim = c(0,1),
       col = mycolours[1], type="n", lwd = 2,
       xlab = "False Positive Rate", ylab = "True Positive Rate")
  abline(0,1, lty = 3, col = "grey")
  # polygon(x=c(rev(ci_lung[[1]][,1]),ci_lung[[1]][,1]),
  #         y=c(ci_lung[[1]][,2],rev(ci_lung[[1]][,3])),
  #         col = alpha(mycolours[1],0.1), border = mycolours[1])
  # polygon(x=c(rev(ci_lung[[2]][,1]),ci_lung[[2]][,1]),
  #         y=c(ci_lung[[2]][,2],rev(ci_lung[[2]][,3])),
  #         col = alpha(mycolours[2],0.1), border = mycolours[2])
  # polygon(x=c(rev(ci_lung[[3]][,1]),ci_lung[[3]][,1]),
  #         y=c(ci_lung[[3]][,2],rev(ci_lung[[3]][,3])),
  #         col = alpha(mycolours[3],0.1), border = mycolours[3])
  # polygon(x=c(rev(ci_lung[[4]][,1]),ci_lung[[4]][,1]),
  #         y=c(ci_lung[[4]][,2],rev(ci_lung[[4]][,3])),
  #         col = alpha(mycolours[4],0.1), border = mycolours[4])
  # polygon(x=c(rev(ci_lung[[5]][,1]),ci_lung[[5]][,1]),
  #         y=c(ci_lung[[5]][,2],rev(ci_lung[[5]][,3])),
  #         col = alpha(mycolours[4],0.1), border = mycolours[5])
  lines(roc_lung[[1]], col = mycolours[1], type="l", lwd = 2)
  lines(roc_lung[[2]], col = mycolours[2], type="l", lwd = 2)
  lines(roc_lung[[3]], col = mycolours[3], type="l", lwd = 2)
  lines(roc_lung[[4]], col = mycolours[4], type="l", lwd = 2)
  lines(roc_lung[[5]], col = mycolours[5], type="l", lwd = 2)
  legend("bottomright",
         legend = c(paste0("LASSO Model adjusted on smoking (forced); AUC = ",
                           auc_lung[1]),
                    paste0("LASSO Base model; AUC = ",
                           auc_lung[2]),
                    paste0("LASSO Model removed effect of smoking; AUC = ",
                           auc_lung[3]),
                    paste0("sPLS-DA Base model; AUC = ",
                           auc_lung[4]),
                    paste0("sPLS-DA Model removed effect of smoking; AUC = ",
                           auc_lung[5])),
         lty = 1, lwd = 2, col = mycolours, cex = 0.7)
  dev.off()
  }

# Bladder
{pdf("../Figures/Final/Main/roc_bladder.pdf", width = 7, height = 7)
  par(mar=c(5,5,1,1))
  plot(roc_bladder[[1]], ylim = c(0,1),
       col = mycolours[1], type="n", lwd = 2,
       xlab = "False Positive Rate", ylab = "True Positive Rate")
  abline(0,1, lty = 3, col = "grey")
  # polygon(x=c(rev(ci_bladder[[1]][,1]),ci_bladder[[1]][,1]),
  #         y=c(ci_bladder[[1]][,2],rev(ci_bladder[[1]][,3])),
  #         col = alpha(mycolours[1],0.1), border = mycolours[1])
  # polygon(x=c(rev(ci_bladder[[2]][,1]),ci_bladder[[2]][,1]),
  #         y=c(ci_bladder[[2]][,2],rev(ci_bladder[[2]][,3])),
  #         col = alpha(mycolours[2],0.1), border = mycolours[2])
  # polygon(x=c(rev(ci_bladder[[3]][,1]),ci_bladder[[3]][,1]),
  #         y=c(ci_bladder[[3]][,2],rev(ci_bladder[[3]][,3])),
  #         col = alpha(mycolours[3],0.1), border = mycolours[3])
  # polygon(x=c(rev(ci_bladder[[4]][,1]),ci_bladder[[4]][,1]),
  #         y=c(ci_bladder[[4]][,2],rev(ci_bladder[[4]][,3])),
  #         col = alpha(mycolours[4],0.1), border = mycolours[4])
  # polygon(x=c(rev(ci_bladder[[5]][,1]),ci_bladder[[5]][,1]),
  #         y=c(ci_bladder[[5]][,2],rev(ci_bladder[[5]][,3])),
  #         col = alpha(mycolours[4],0.1), border = mycolours[5])
  lines(roc_bladder[[1]], col = mycolours[1], type="l", lwd = 2)
  lines(roc_bladder[[2]], col = mycolours[2], type="l", lwd = 2)
  lines(roc_bladder[[3]], col = mycolours[3], type="l", lwd = 2)
  lines(roc_bladder[[4]], col = mycolours[4], type="l", lwd = 2)
  lines(roc_bladder[[5]], col = mycolours[5], type="l", lwd = 2)
  legend("bottomright",
         legend = c(paste0("LASSO Model adjusted on smoking (forced); AUC = ",
                           auc_bladder[1]),
                    paste0("LASSO Base model; AUC = ",
                           auc_bladder[2]),
                    paste0("LASSO Model removed effect of smoking; AUC = ",
                           auc_bladder[3]),
                    paste0("sPLS-DA Base model; AUC = ",
                           auc_bladder[4]),
                    paste0("sPLS-DA Model removed effect of smoking; AUC = ",
                           auc_bladder[5])),
         lty = 1, lwd = 2, col = mycolours, cex = 0.7)
  dev.off()
  }

# base=c(lasso_auc_avg_lung.1[lasso_calib_lung.1,2],
#        lasso_auc_recalib_lung.1[lasso_calib_lung.1,2],
#        spls_auc_avg_lung.1,
#        spls_auc_recalib_lung.1,
#        lasso_auc_avg_bladder.1[lasso_calib_bladder.1,2],
#        lasso_auc_recalib_bladder.1[lasso_calib_bladder.1,2],
#        spls_auc_avg_bladder.1,
#        spls_auc_recalib_bladder.1)
# 
# smoking=c(lasso_auc_avg_lung.2[lasso_calib_lung.2,2],
#           lasso_auc_recalib_lung.2[lasso_calib_lung.2,2],
#           spls_auc_avg_lung.2,
#           spls_auc_recalib_lung.2,
#           lasso_auc_avg_bladder.2[lasso_calib_bladder.2,2],
#           lasso_auc_recalib_bladder.2[lasso_calib_bladder.2,2],
#           spls_auc_avg_bladder.2,
#           spls_auc_recalib_bladder.2)
#                
# AUC = cbind(base, smoking, rep(NA, 8), rep(NA, 8))
# AUC=as.vector(t(AUC))
# AUC = AUC[-c(length(AUC) - 1, length(AUC))]
# 
# models=c("Base model", "Model adjusted on smoking status")
# mycolours=c("navy","tomato")
# 
# {pdf("../Figures/Presentation/auc_lasso_spls.pdf", width=10, height=5)
#   par(mar=c(10,5,1,1))
#   plot(AUC, type = "h", lwd = 3, xaxt = "n", xlab = "",
#        ylab = "AUC", col = mycolours, xlim = c(0, length(AUC)), ylim = c(0,1))
#   axis(1, at = seq(1.5, 8 * 4, by = 4), labels = rep(c("Mean beta","Recalibrated"),4))
#   tmpseq=seq(1.5, 8 * 4, by = 4)
#   for (k in seq(1, length(tmpseq),by=2)){
#     lab=rep(c("LASSO","sPLS"),each=2, times=2)[k]
#     axis(side=1, at=tmpseq[c(k,k+1)]+c(-0.7,+0.7), line=3, labels=NA, cex=0.5)
#     axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=2.7, tick=FALSE,labels=lab)
#   }
#   for (k in seq(1, length(tmpseq),by=4)){
#     lab=rep(c("Lung cancer","Bladder cancer"),each=4)[k]
#     axis(side=1, at=tmpseq[c(k,k+3)]+c(-1,+1), line=6, labels=NA, cex=0.5)
#     axis(side=1, at=mean(tmpseq[c(k,k+3)]), line=5.7, tick=FALSE,labels=lab)
#   }
#   legend("bottomright", lty=1, lwd=2, col=mycolours, legend = models, bg="white")
#   dev.off()
# }
# 
# base=c(lasso_auc_recalib_lung.1[lasso_calib_lung.1,2],
#        spls_auc_recalib_lung.1,
#        lasso_auc_recalib_bladder.1[lasso_calib_bladder.1,2],
#        spls_auc_recalib_bladder.1)
# 
# smoking=c(lasso_auc_recalib_lung.2[lasso_calib_lung.2,2],
#           spls_auc_recalib_lung.2,
#           lasso_auc_recalib_bladder.2[lasso_calib_bladder.2,2],
#           spls_auc_recalib_bladder.2)
# 
# AUC = cbind(base, smoking, rep(NA, 4), rep(NA, 4))
# AUC=as.vector(t(AUC))
# AUC = AUC[-c(length(AUC) - 1, length(AUC))]
# 
# models=c("Base model", "Model adjusted on smoking status")
# mycolours=c("navy","tomato")
# {pdf("../Figures/Report/auc_lasso_spls.pdf", width=8, height=5)
#   par(mar=c(6,5,1,1))
#   plot(AUC, type = "h", lwd = 3, xaxt = "n", xlab = "",
#        ylab = "AUC", col = mycolours, xlim = c(0, length(AUC)), ylim = c(0,1))
#   axis(1, at = seq(1.5, 4 * 4, by = 4), labels = rep(c("LASSO","sPLS"),2))
#   tmpseq=seq(1.5, 4 * 4, by = 4)
#   for (k in seq(1, length(tmpseq),by=2)){
#     lab=rep(c("Lung cancer","Bladder cancer"),each=2, times=2)[k]
#     axis(side=1, at=tmpseq[c(k,k+1)]+c(-1,+1), line=3, labels=NA, cex=0.5)
#     axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=2.7, tick=FALSE,labels=lab)
#   }
#   legend("bottomright", lty=1, lwd=2, col=mycolours, legend = models, bg="white")
#   dev.off()
# }

