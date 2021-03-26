### TDS Project -- Multivariate analyses Prediction Performance
## Programme created by Rin Wada on 25 March

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/"
setwd(project_path)

### Loading packages----
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)

source("penalisation_functions.R")

### Plot labels----
plot_annot=read_csv("../Dictionaries/plot_annot.csv")
plot_annot=plot_annot[-c(1,2,21:25),] # Remove age, sex and BMI


### Load outputs ----
for (m in 1:2){
  arr=paste0(c("lung","bladder"),".1")[m]
  ### Lasso
  lasso_forced_out=readRDS(paste0("../Results/lasso_forced/out_",arr,".rds")) # Load output
  assign(paste0("lasso_forced_out_",arr),lasso_forced_out) # Assign name
  lasso_forced_calib=sum(CalibratedStableRegression(lasso_forced_out)) # Number of selected variables
  assign(paste0("lasso_forced_calib_",arr),lasso_forced_calib) # Assign name
  lasso_forced_auc_avg=readRDS(paste0("../Results/lasso_forced/auc_average_beta_",arr,".rds")) # Load auc (average beta)
  assign(paste0("lasso_forced_auc_avg_",arr),lasso_forced_auc_avg) # Assign name
  lasso_forced_auc_recalib=readRDS(paste0("../Results/lasso_forced/auc_recalib_beta_",arr,".rds")) # Load auc (recalibrated)
  assign(paste0("lasso_forced_auc_recalib_",arr),lasso_forced_auc_recalib) # Assign name
}

for (m in 1:4){
  arr=paste0(rep(c("lung","bladder"),each=2),".",1:2)[m]
  ### Lasso
  lasso_out=readRDS(paste0("../Results/lasso/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_calib=sum(CalibratedStableRegression(lasso_out)) # Number of selected variables
  assign(paste0("lasso_calib_",arr),lasso_calib) # Assign name
  lasso_auc_avg=readRDS(paste0("../Results/lasso/auc_average_beta_",arr,".rds")) # Load auc (average beta)
  assign(paste0("lasso_auc_avg_",arr),lasso_auc_avg) # Assign name
  lasso_auc_recalib=readRDS(paste0("../Results/lasso/auc_recalib_beta_",arr,".rds")) # Load auc (recalibrated)
  assign(paste0("lasso_auc_recalib_",arr),lasso_auc_recalib) # Assign name
  
  arrTT=paste0(rep(c("lung_TT","bladder_TT"),each=2),".",1:2)[m]
  ### sPLS
  spls_out=readRDS(paste0("../Results/PLS_319/train_test_spls/sPLS_TT_out_",arrTT,".rds")) # Load output
  assign(paste0("spls_out_",arr),spls_out) # Assign name
  spls_calib=sum(CalibratedStableRegression(spls_out)) # Number of selected variables
  assign(paste0("spls_calib_",arr),spls_calib) # Assign name
  spls_auc_avg=readRDS(paste0("../Results/PLS_319/train_test_spls/sPLS_auc_",arrTT,".rds")) # Load auc (average beta)
  assign(paste0("spls_auc_avg_",arr),spls_auc_avg) # Assign name
  spls_auc_recalib=readRDS(paste0("../Results/PLS_319/train_test_spls/sPLS_auc_recali_",arrTT,".rds")) # Load auc (recalibrated)
  assign(paste0("spls_auc_recalib_",arr),spls_auc_recalib) # Assign name
}

### Sensitivity analysis LASSO----

## Global settings
mycolours=rep(c("grey50","tomato","forestgreen","royalblue","gold"),times=c(18,28,8,16,28))
mycolours_lab=darken(mycolours, amount=0.5)
colnames=names(lasso_forced_out_lung.1$Beta[1,,1])
myorder=c(colnames[5:46],colnames[1:4],colnames[47:length(colnames)])
names(mycolours)=c(myorder)
names(mycolours_lab)=c(myorder)
point.lab=plot_annot$label.point
names(point.lab)=c(myorder)

## Average beta: Lung
auc=lasso_forced_auc_avg_lung.1
calib=lasso_forced_calib_lung.1
file_path="lasso_forced_auc_avg_lung"

# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Report/",file_path,".pdf"), width=10, height=5)
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
  abline(v=4, col="black", lty=3, lwd=1)
  abline(h=as.numeric(auc[4,2]), col="black", lty=3, lwd=1)
  abline(h=as.numeric(auc[calib,2]), col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  legend("topright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = c(paste0("Calibrated model; AUC = ",auc[calib,2]),
                    paste0("Emplty model; AUC = ",auc[4,2])))
  dev.off()
}

## Average beta: Bladder
auc=lasso_forced_auc_avg_bladder.1
calib=lasso_forced_calib_bladder.1
file_path="lasso_forced_auc_avg_bladder"
{pdf(paste0("../Figures/Report/",file_path,".pdf"), width=10, height=5)
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
  abline(v=4, col="black", lty=3, lwd=1)
  abline(h=as.numeric(auc[4,2]), col="black", lty=3, lwd=1)
  abline(h=as.numeric(auc[calib,2]), col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  legend("topright", lty=c(4,3), lwd=1, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = c(paste0("Calibrated model; AUC = ",auc[calib,2]),
                    paste0("Emplty model; AUC = ",auc[4,2])))
  dev.off()
}

## Recalibrated: Lung
auc=lasso_forced_auc_recalib_lung.1
calib=lasso_forced_calib_lung.1
file_path="lasso_forced_auc_recalib_lung"

## Recalibrated beta: Bladder
auc=lasso_forced_auc_recalib_bladder.1
calib=lasso_forced_calib_bladder.1
file_path="lasso_forced_auc_recalib_bladder"
{pdf(paste0("../Figures/Report/",file_path,".pdf"), width=10, height=5)
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
  abline(v=4, col="black", lty=3, lwd=1)
  abline(h=as.numeric(auc[4,2]), col="black", lty=3, lwd=1)
  abline(h=as.numeric(auc[calib,2]), col=mycolours[rownames(auc)[calib]], lty=4, lwd=1)
  legend("topright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = c(paste0("Calibrated model; AUC = ",auc[calib,2]),
                    paste0("Emplty model; AUC = ",auc[4,2])))
  dev.off()
}

### Main analysis LASSO ----

## Global settings
mycolours=rep(c("grey50","tomato","forestgreen","royalblue","gold"),times=c(18,28,8,16,28))
mycolours_lab=darken(mycolours, amount=0.5)
colnames=names(lasso_out_lung.1$Beta[1,,1])
myorder=c(colnames[5:46],colnames[1:4],colnames[47:length(colnames)])
names(mycolours)=c(myorder)
names(mycolours_lab)=c(myorder)
point.lab=plot_annot$label.point
names(point.lab)=c(myorder)

## Average beta: Lung
auc=lasso_auc_avg_lung.1
calib=lasso_calib_lung.1
file_path="lasso_auc_avg_lung.1"

# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Report/",file_path,".pdf"), width=10, height=5)
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
  legend("topright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
  dev.off()
}

auc=lasso_auc_avg_lung.2
calib=lasso_calib_lung.2
file_path="lasso_auc_avg_lung.2"

# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Report/",file_path,".pdf"), width=10, height=5)
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
  legend("topright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
  dev.off()
}

## Average beta: Bladder
auc=lasso_auc_avg_bladder.1
calib=lasso_calib_bladder.1
file_path="lasso_auc_avg_bladder.1"

# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Report/",file_path,".pdf"), width=10, height=5)
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
  legend("topright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
  dev.off()
}

auc=lasso_auc_avg_bladder.2
calib=lasso_calib_bladder.2
file_path="lasso_auc_avg_bladder.2"

# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Report/",file_path,".pdf"), width=10, height=5)
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
  legend("topright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
  dev.off()
}

## Recalibrated: Lung
auc=lasso_auc_recalib_lung.1
calib=lasso_calib_lung.1
file_path="lasso_auc_recalib_lung.1"

# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Report/",file_path,".pdf"), width=10, height=5)
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
  legend("topright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
  dev.off()
}

auc=lasso_auc_recalib_lung.2
calib=lasso_calib_lung.2
file_path="lasso_auc_recalib_lung.2"

# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Report/",file_path,".pdf"), width=10, height=5)
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
  legend("topright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
  dev.off()
}

## Recalibrated beta: Bladder
auc=lasso_auc_recalib_bladder.1
calib=lasso_calib_bladder.1
file_path="lasso_auc_recalib_bladder.1"
# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Report/",file_path,".pdf"), width=10, height=5)
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
  legend("topright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
  dev.off()
}

auc=lasso_auc_recalib_bladder.2
calib=lasso_calib_bladder.2
file_path="lasso_auc_recalib_bladder.2"
# AUC as a function of the number of predictors
{pdf(paste0("../Figures/Report/",file_path,".pdf"), width=10, height=5)
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
  legend("topright", lty=c(4,3), lwd=2, col=c(mycolours[rownames(auc)[calib]],"black"),
         legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
  dev.off()
}

### Main analyses: LASSO vs sPLS ----

base=c(lasso_auc_avg_lung.1[lasso_calib_lung.1,2],
       lasso_auc_recalib_lung.1[lasso_calib_lung.1,2],
       spls_auc_avg_lung.1,
       spls_auc_recalib_lung.1,
       lasso_auc_avg_bladder.1[lasso_calib_bladder.1,2],
       lasso_auc_recalib_bladder.1[lasso_calib_bladder.1,2],
       spls_auc_avg_bladder.1,
       spls_auc_recalib_bladder.1)

smoking=c(lasso_auc_avg_lung.2[lasso_calib_lung.2,2],
          lasso_auc_recalib_lung.2[lasso_calib_lung.2,2],
          spls_auc_avg_lung.2,
          spls_auc_recalib_lung.2,
          lasso_auc_avg_bladder.2[lasso_calib_bladder.2,2],
          lasso_auc_recalib_bladder.2[lasso_calib_bladder.2,2],
          spls_auc_avg_bladder.2,
          spls_auc_recalib_bladder.2)
               
AUC = cbind(base, smoking, rep(NA, 8), rep(NA, 8))
AUC=as.vector(t(AUC))
AUC = AUC[-c(length(AUC) - 1, length(AUC))]

models=c("Base model", "Model adjusted on smoking status")
mycolours=c("navy","tomato")

{pdf("../Figures/Presentation/auc_lasso_spls.pdf", width=10, height=5)
  par(mar=c(10,5,1,1))
  plot(AUC, type = "h", lwd = 3, xaxt = "n", xlab = "",
       ylab = "AUC", col = mycolours, xlim = c(0, length(AUC)), ylim = c(0,1))
  axis(1, at = seq(1.5, 8 * 4, by = 4), labels = rep(c("Mean beta","Recalibrated"),4))
  tmpseq=seq(1.5, 8 * 4, by = 4)
  for (k in seq(1, length(tmpseq),by=2)){
    lab=rep(c("LASSO","sPLS"),each=2, times=2)[k]
    axis(side=1, at=tmpseq[c(k,k+1)]+c(-0.7,+0.7), line=3, labels=NA, cex=0.5)
    axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=2.7, tick=FALSE,labels=lab)
  }
  for (k in seq(1, length(tmpseq),by=4)){
    lab=rep(c("Lung cancer","Bladder cancer"),each=4)[k]
    axis(side=1, at=tmpseq[c(k,k+3)]+c(-1,+1), line=6, labels=NA, cex=0.5)
    axis(side=1, at=mean(tmpseq[c(k,k+3)]), line=5.7, tick=FALSE,labels=lab)
  }
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, bg="white")
  dev.off()
}

base=c(lasso_auc_recalib_lung.1[lasso_calib_lung.1,2],
       spls_auc_recalib_lung.1,
       lasso_auc_recalib_bladder.1[lasso_calib_bladder.1,2],
       spls_auc_recalib_bladder.1)

smoking=c(lasso_auc_recalib_lung.2[lasso_calib_lung.2,2],
          spls_auc_recalib_lung.2,
          lasso_auc_recalib_bladder.2[lasso_calib_bladder.2,2],
          spls_auc_recalib_bladder.2)

AUC = cbind(base, smoking, rep(NA, 4), rep(NA, 4))
AUC=as.vector(t(AUC))
AUC = AUC[-c(length(AUC) - 1, length(AUC))]

models=c("Base model", "Model adjusted on smoking status")
mycolours=c("navy","tomato")
{pdf("../Figures/Report/auc_lasso_spls.pdf", width=8, height=5)
  par(mar=c(6,5,1,1))
  plot(AUC, type = "h", lwd = 3, xaxt = "n", xlab = "",
       ylab = "AUC", col = mycolours, xlim = c(0, length(AUC)), ylim = c(0,1))
  axis(1, at = seq(1.5, 4 * 4, by = 4), labels = rep(c("LASSO","sPLS"),2))
  tmpseq=seq(1.5, 4 * 4, by = 4)
  for (k in seq(1, length(tmpseq),by=2)){
    lab=rep(c("Lung cancer","Bladder cancer"),each=2, times=2)[k]
    axis(side=1, at=tmpseq[c(k,k+1)]+c(-1,+1), line=3, labels=NA, cex=0.5)
    axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=2.7, tick=FALSE,labels=lab)
  }
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, bg="white")
  dev.off()
}

