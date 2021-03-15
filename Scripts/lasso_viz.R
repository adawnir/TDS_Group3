### TDS Project -- Stability selection LASSO Logistic Regression Visualisation
## Programme created by Rin Wada on 11 March reviewed on 14 March

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

# Loading packages
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)

source("penalisation_functions.R")

# plot labels
setwd("../Dictionaries")
plot_annot=read_csv("plot_annot.csv")

### Load data ----
setwd("../Results/lasso")
# Load output
out_lung.0.1=readRDS("out_lung.0.1.rds")
out_lung.0.2=readRDS("out_lung.0.2.rds")
out_lung.1=readRDS("out_lung.1.rds")
out_lung.2=readRDS("out_lung.2.rds")

out_bladder.0.1=readRDS("out_bladder.0.1.rds")
out_bladder.0.2=readRDS("out_bladder.0.2.rds")
out_bladder.1=readRDS("out_bladder.1.rds")
out_bladder.2=readRDS("out_bladder.2.rds")

# Load selection proportion
selprop_lung.0.1=readRDS("selprop_lung.0.1.rds")
selprop_lung.0.2=readRDS("selprop_lung.0.2.rds")
selprop_lung.1=readRDS("selprop_lung.1.rds")
selprop_lung.2=readRDS("selprop_lung.2.rds")

selprop_bladder.0.1=readRDS("selprop_bladder.0.1.rds")
selprop_bladder.0.2=readRDS("selprop_bladder.0.2.rds")
selprop_bladder.1=readRDS("selprop_bladder.1.rds")
selprop_bladder.2=readRDS("selprop_bladder.2.rds")

# Extract calibrated pi
hat_params_lung.0.1=GetArgmax(out_lung.0.1)
hat_params_lung.0.2=GetArgmax(out_lung.0.2)
hat_params_lung.1=GetArgmax(out_lung.1)
hat_params_lung.2=GetArgmax(out_lung.2)

hat_params_bladder.0.1=GetArgmax(out_bladder.0.1)
hat_params_bladder.0.2=GetArgmax(out_bladder.0.2)
hat_params_bladder.1=GetArgmax(out_bladder.1)
hat_params_bladder.2=GetArgmax(out_bladder.2)

# Lung
# non-denoised data with forces variables
selprop_lung_force=data.frame(selprop_lung.0.1)
selprop_lung_force$selprop_lung.0.2=selprop_lung.0.2[match(rownames(selprop_lung_force),
                                                           names(selprop_lung.0.2))]
# denoised
selprop_lung_denoise=data.frame(selprop_lung.1)
selprop_lung_denoise$selprop_lung.2=selprop_lung.2[match(rownames(selprop_lung_denoise),
                                                         names(selprop_lung.2))]

# Bladder
# non-denoised data with forces variables
selprop_bladder_force=data.frame(selprop_bladder.0.1)
selprop_bladder_force$selprop_bladder.0.2=selprop_bladder.0.2[match(rownames(selprop_bladder_force),
                                                                    names(selprop_bladder.0.2))]

# denoised
selprop_bladder_denoise=data.frame(selprop_bladder.1)
selprop_bladder_denoise$selprop_bladder.2=selprop_bladder.2[match(rownames(selprop_bladder_denoise),
                                                                  names(selprop_bladder.2))]
# Reorder rows
myorder=plot_annot$col.name
selprop_lung_force = selprop_lung_force %>%
  slice(match(myorder,rownames(selprop_lung_force)))
selprop_bladder_force = selprop_bladder_force %>%
  slice(match(myorder,rownames(selprop_bladder_force)))
myorder2=myorder[-c(1,2,21:25)]
selprop_lung_denoise = selprop_lung_denoise %>%
  slice(match(myorder,rownames(selprop_lung_denoise)))
selprop_bladder_denoise = selprop_bladder_denoise %>%
  slice(match(myorder,rownames(selprop_bladder_denoise)))

# Convert data.frame to vector with two empty entries between variables
selprop_lung_force=cbind(rep(NA,nrow(selprop_lung_force)),
                         selprop_lung_force[,1],
                         rep(NA, nrow(selprop_lung_force)),
                         selprop_lung_force[,2],
                         rep(NA,nrow(selprop_lung_force)))
selprop_lung_force=as.vector(t(selprop_lung_force))

selprop_lung_denoise=cbind(rep(NA,nrow(selprop_lung_denoise)),
                           selprop_lung_denoise[,1],
                           rep(NA, nrow(selprop_lung_denoise)),
                           selprop_lung_denoise[,2],
                           rep(NA,nrow(selprop_lung_denoise)))
selprop_lung_denoise=as.vector(t(selprop_lung_denoise))

selprop_bladder_force=cbind(rep(NA,nrow(selprop_bladder_force)),
                            selprop_bladder_force[,1],
                            rep(NA, nrow(selprop_bladder_force)),
                            selprop_bladder_force[,2],
                            rep(NA,nrow(selprop_bladder_force)))
selprop_bladder_force=as.vector(t(selprop_bladder_force))

selprop_bladder_denoise=cbind(rep(NA,nrow(selprop_bladder_denoise)),
                              selprop_bladder_denoise[,1],
                              rep(NA, nrow(selprop_bladder_denoise)),
                              selprop_bladder_denoise[,2],
                              rep(NA,nrow(selprop_bladder_denoise)))
selprop_bladder_denoise=as.vector(t(selprop_bladder_denoise))

# Load beta
beta_lung.0.1=readRDS("average_beta_lung.0.1.rds")
beta_lung.0.2=readRDS("average_beta_lung.0.2.rds")
beta_lung.1=readRDS("average_beta_lung.1.rds")
beta_lung.2=readRDS("average_beta_lung.2.rds")

beta_bladder.0.1=readRDS("average_beta_bladder.0.1.rds")
beta_bladder.0.2=readRDS("average_beta_bladder.0.2.rds")
beta_bladder.1=readRDS("average_beta_bladder.1.rds")
beta_bladder.2=readRDS("average_beta_bladder.2.rds")

# Shrink non-selected beta to zero
beta_lung.0.1=ifelse(CalibratedStableRegression(out_lung.0.1) == 1, beta_lung.0.1, 0)
beta_lung.0.2=ifelse(CalibratedStableRegression(out_lung.0.2) == 1, beta_lung.0.2, 0)
beta_lung.1=ifelse(CalibratedStableRegression(out_lung.1) == 1, beta_lung.1, 0)
beta_lung.2=ifelse(CalibratedStableRegression(out_lung.2) == 1, beta_lung.2, 0)

beta_bladder.0.1=ifelse(CalibratedStableRegression(out_bladder.0.1) == 1, beta_bladder.0.1, 0)
beta_bladder.0.2=ifelse(CalibratedStableRegression(out_bladder.0.2) == 1, beta_bladder.0.2, 0)
beta_bladder.1=ifelse(CalibratedStableRegression(out_bladder.1) == 1, beta_bladder.1, 0)
beta_bladder.2=ifelse(CalibratedStableRegression(out_bladder.2) == 1, beta_bladder.2, 0)

# Extract beta coefficients
# Lung
# non-denoised data with forces variables
beta_lung_force=data.frame(beta_lung.0.1)
beta_lung_force$beta_lung.0.2=beta_lung.0.2[match(rownames(beta_lung_force),
                                                  names(beta_lung.0.2))]

# Reorder rows
myorder=plot_annot$col.name
beta_lung_force = beta_lung_force %>%
  slice(match(myorder,rownames(beta_lung_force)))

beta_lung_denoise=data.frame(beta_lung.1)
beta_lung_denoise$beta_lung.2=beta_lung.2[match(rownames(beta_lung_denoise),
                                                names(beta_lung.2))]
# Reorder rows
myorder2=myorder[-c(1,2,21:25)]
beta_lung_denoise = beta_lung_denoise %>%
  slice(match(myorder2,rownames(beta_lung_denoise)))

# Bladder
# non-denoised data with forces variables
beta_bladder_force=data.frame(beta_bladder.0.1)
beta_bladder_force$beta_bladder.0.2=beta_bladder.0.2[match(rownames(beta_bladder_force),
                                                           names(beta_bladder.0.2))]
# Reorder rows
beta_bladder_force = beta_bladder_force %>%
  slice(match(myorder,rownames(beta_bladder_force)))

# denoised
beta_bladder_denoise=data.frame(beta_bladder.1)
beta_bladder_denoise$beta_bladder.2=beta_bladder.2[match(rownames(beta_bladder_denoise),
                                                         names(beta_bladder.2))]
beta_bladder_denoise = beta_bladder_denoise %>%
  slice(match(myorder2,rownames(beta_bladder_denoise)))

### Selection proportion plot----
## Forced
mylabels=plot_annot$label
myref=plot_annot$ref
variable_cat=c(rep("Sociodemographic",20),
               rep("Health risk", 33),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
models=c("Base model", "Model adjusted on smoking status")
mycolours=darken(c("navy","tomato","gold3"), amount=0.2)
pi=c(hat_params_lung.0.1[2],hat_params_lung.0.2[2])

myspacing=5
xseq=seq(3,length(myorder)*myspacing, by=myspacing)

setwd("../../Figures/lasso")
{pdf("selprop_lung_force.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(selprop_lung_force,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), cex.axis=0.7)
  mtext(side=4, text="Selection Proportion", line=2, cex.lab=0.7)
  abline(h=pi[1], lty=2, col=mycolours[1])
  abline(h=pi[2], lty=4, col=mycolours[2])
  for (k in 1:length(xseq)){
    mytext=mylabels[k]
    if (grepl("m\\^", mytext)){
      mytext=gsub("m\\^","'~m^", mytext)
      mytext=sub(")","~')", mytext)
    }
    if (grepl("\\[", mytext)){
      mytext=gsub("\\[","'[", mytext)
      mytext=sub("\\(ug","~'(ug", mytext)
    }
    mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
    a=selprop_lung.0.1[myorder]
    a[which(is.na(a))]=0
    b=selprop_lung.0.2[myorder]
    b[which(is.na(b))]=0
    labcol=ifelse((a[k]>=pi[1] & b[k]>=pi[2]), mycolours[3],
                  ifelse(a[k]>=pi[1],mycolours[1],
                         ifelse(b[k]>=pi[2], mycolours[2],"black")))
    labfont=ifelse(labcol=="black",1,2)
    axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.5,
         col.axis=labcol, col = labcol, font=labfont)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.5)
    }
  }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      if (grepl("m\\^", mytext)){
        mytext=gsub("m\\^","'~m^", mytext)
        mytext=sub(")","~')", mytext)
      }
      mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
      axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
           labels=mytmp, las=2)
    }
  }
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black")
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6, bg="white")
  dev.off()}

pi=c(hat_params_bladder.0.1[2],hat_params_bladder.0.2[2])
{pdf("selprop_bladder_force.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(selprop_bladder_force,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), cex.axis=0.7)
  mtext(side=4, text="Selection Proportion", line=2, cex.lab=0.7)
  abline(h=pi[1], lty=2, col=mycolours[1])
  abline(h=pi[2], lty=4, col=mycolours[2])
  for (k in 1:length(xseq)){
    mytext=mylabels[k]
    if (grepl("m\\^", mytext)){
      mytext=gsub("m\\^","'~m^", mytext)
      mytext=sub(")","~')", mytext)
    }
    if (grepl("\\[", mytext)){
      mytext=gsub("\\[","'[", mytext)
      mytext=sub("\\(ug","~'(ug", mytext)
    }
    mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
    a=selprop_bladder.0.1[myorder]
    a[which(is.na(a))]=0
    b=selprop_bladder.0.2[myorder]
    b[which(is.na(b))]=0
    labcol=ifelse((a[k]>=pi[1] & b[k]>=pi[2]), mycolours[3],
                  ifelse(a[k]>=pi[1],mycolours[1],
                         ifelse(b[k]>=pi[2], mycolours[2],"black")))
    labfont=ifelse(labcol=="black",1,2)
    axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.5,
         col.axis=labcol, col = labcol, font=labfont)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.5)
    }
  }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      if (grepl("m\\^", mytext)){
        mytext=gsub("m\\^","'~m^", mytext)
        mytext=sub(")","~')", mytext)
      }
      mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
      axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
           labels=mytmp, las=2)
    }
  }
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black")
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6, bg="white")
  dev.off()}


## Denoised
mylabels=plot_annot$label[-c(1,2,21:25)]
myref=plot_annot$ref[-c(1,2,21:25)]
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 28),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))


pi=c(hat_params_lung.1[2],hat_params_lung.2[2])
myspacing=5
xseq=seq(3,length(myorder2)*myspacing, by=myspacing)

{pdf("selprop_lung_denoise.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(selprop_lung_denoise,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), cex.axis=0.7)
  mtext(side=4, text="Selection Proportion", line=2, cex.lab=0.7)
  abline(h=pi[1], lty=2, col=mycolours[1])
  abline(h=pi[2], lty=4, col=mycolours[2])
  for (k in 1:length(xseq)){
    mytext=mylabels[k]
    if (grepl("m\\^", mytext)){
      mytext=gsub("m\\^","'~m^", mytext)
      mytext=sub(")","~')", mytext)
    }
    if (grepl("\\[", mytext)){
      mytext=gsub("\\[","'[", mytext)
      mytext=sub("\\(ug","~'(ug", mytext)
    }
    mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
    a=selprop_lung.1[myorder2]
    a[which(is.na(a))]=0
    b=selprop_lung.2[myorder2]
    b[which(is.na(b))]=0
    labcol=ifelse((a[k]>=pi[1] & b[k]>=pi[2]), mycolours[3],
                  ifelse(a[k]>=pi[1],mycolours[1],
                         ifelse(b[k]>=pi[2], mycolours[2],"black")))
    labfont=ifelse(labcol=="black",1,2)
    axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.5,
         col.axis=labcol, col = labcol, font=labfont)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.5)
    }
  }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      if (grepl("m\\^", mytext)){
        mytext=gsub("m\\^","'~m^", mytext)
        mytext=sub(")","~')", mytext)
      }
      mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
      axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
           labels=mytmp, las=2)
    }
  }
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black")
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6, bg="white")
  dev.off()}

pi=c(hat_params_bladder.1[2],hat_params_bladder.2[2])
{pdf("selprop_bladder_denoise.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(selprop_bladder_denoise,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), cex.axis=0.7)
  mtext(side=4, text="Selection Proportion", line=2, cex.lab=0.7)
  abline(h=pi[1], lty=2, col=mycolours[1])
  abline(h=pi[2], lty=4, col=mycolours[2])
  for (k in 1:length(xseq)){
    mytext=mylabels[k]
    if (grepl("m\\^", mytext)){
      mytext=gsub("m\\^","'~m^", mytext)
      mytext=sub(")","~')", mytext)
    }
    if (grepl("\\[", mytext)){
      mytext=gsub("\\[","'[", mytext)
      mytext=sub("\\(ug","~'(ug", mytext)
    }
    mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
    a=selprop_bladder.1[myorder2]
    a[which(is.na(a))]=0
    b=selprop_bladder.2[myorder2]
    b[which(is.na(b))]=0
    labcol=ifelse((a[k]>=pi[1] & b[k]>=pi[2]), mycolours[3],
                  ifelse(a[k]>=pi[1],mycolours[1],
                         ifelse(b[k]>=pi[2], mycolours[2],"black")))
    labfont=ifelse(labcol=="black",1,2)
    axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.5,
         col.axis=labcol, col = labcol, font=labfont)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.5)
    }
  }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      if (grepl("m\\^", mytext)){
        mytext=gsub("m\\^","'~m^", mytext)
        mytext=sub(")","~')", mytext)
      }
      mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
      axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
           labels=mytmp, las=2)
    }
  }
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black")
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6, bg="white")
  dev.off()}

### OR plots ----
# Convert data.frame to vector with two empty entries between variables
OR_lung_force=cbind(rep(NA,nrow(beta_lung_force)),
                    exp(beta_lung_force[,1])-1,
                    rep(NA, nrow(beta_lung_force)),
                    exp(beta_lung_force[,2])-1,
                    rep(NA,nrow(beta_lung_force)))
OR_lung_force=as.vector(t(OR_lung_force))

OR_lung_denoise=cbind(rep(NA,nrow(beta_lung_denoise)),
                      exp(beta_lung_denoise[,1])-1,
                      rep(NA, nrow(beta_lung_denoise)),
                      exp(beta_lung_denoise[,2])-1,
                      rep(NA,nrow(beta_lung_denoise)))
OR_lung_denoise=as.vector(t(OR_lung_denoise))

OR_bladder_force=cbind(rep(NA,nrow(beta_bladder_force)),
                       exp(beta_bladder_force[,1])-1,
                       rep(NA, nrow(beta_bladder_force)),
                       exp(beta_bladder_force[,2])-1,
                       rep(NA,nrow(beta_bladder_force)))
OR_bladder_force=as.vector(t(OR_bladder_force))

OR_bladder_denoise=cbind(rep(NA,nrow(beta_bladder_denoise)),
                         exp(beta_bladder_denoise[,1])-1,
                         rep(NA, nrow(beta_bladder_denoise)),
                         exp(beta_bladder_denoise[,2])-1,
                         rep(NA,nrow(beta_bladder_denoise)))
OR_bladder_denoise=as.vector(t(OR_bladder_denoise))

## Forced
mylabels=plot_annot$label
myref=plot_annot$ref
variable_cat=c(rep("Sociodemographic",20),
               rep("Health risk", 33),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
models=c("Base model", "Model adjusted on smoking status")
mycolours=darken(c("navy","tomato"), amount=0.2)

myspacing=5
xseq=seq(3,length(myorder)*myspacing, by=myspacing)

setwd("../../Figures/lasso")
{pdf("OR_lung_force.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(OR_lung_force,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), labels=axTicks(2)+1, cex.axis=0.7)
  mtext(side=4, text="Mean Odds Ratio", line=2, cex.lab=0.7)
  abline(h=0, lty=2)
  for (k in 1:length(xseq)){
    mytext=mylabels[k]
    if (grepl("m\\^", mytext)){
      mytext=gsub("m\\^","'~m^", mytext)
      mytext=sub(")","~')", mytext)
    }
    if (grepl("\\[", mytext)){
      mytext=gsub("\\[","'[", mytext)
      mytext=sub("\\(ug","~'(ug", mytext)
    }
    mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
    axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.5)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.5)
    }
  }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      if (grepl("m\\^", mytext)){
        mytext=gsub("m\\^","'~m^", mytext)
        mytext=sub(")","~')", mytext)
      }
      mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
      axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
           labels=mytmp, las=2)
    }
  }
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black")
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.7, bg="white")
  dev.off()}

{pdf("OR_bladder_force.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(OR_bladder_force,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), labels=axTicks(2)+1, cex.axis=0.7)
  mtext(side=4, text="Mean Odds Ratio", line=2, cex.lab=0.7)
  abline(h=0, lty=2)
  for (k in 1:length(xseq)){
    mytext=mylabels[k]
    if (grepl("m\\^", mytext)){
      mytext=gsub("m\\^","'~m^", mytext)
      mytext=sub(")","~')", mytext)
    }
    if (grepl("\\[", mytext)){
      mytext=gsub("\\[","'[", mytext)
      mytext=sub("\\(ug","~'(ug", mytext)
    }
    mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
    axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.5)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.5)
    }
  }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      if (grepl("m\\^", mytext)){
        mytext=gsub("m\\^","'~m^", mytext)
        mytext=sub(")","~')", mytext)
      }
      mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
      axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
           labels=mytmp, las=2)
    }
  }
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black")
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.7, bg="white")
  dev.off()}

## Denoised
mylabels=plot_annot$label[-c(1,2,21:25)]
myref=plot_annot$ref[-c(1,2,21:25)]
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 28),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
models=c("Base model", "Model adjusted on smoking status")
mycolours=darken(c("navy","tomato"), amount=0.2)
myspacing=5
xseq=seq(3,length(myorder2)*myspacing, by=myspacing)

{pdf("OR_lung_denoise.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(OR_lung_denoise,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), labels=axTicks(2)+1, cex.axis=0.7)
  mtext(side=4, text="Mean Odds Ratio", line=2, cex.lab=0.7)
  abline(h=0, lty=2)
  for (k in 1:length(xseq)){
    mytext=mylabels[k]
    if (grepl("m\\^", mytext)){
      mytext=gsub("m\\^","'~m^", mytext)
      mytext=sub(")","~')", mytext)
    }
    if (grepl("\\[", mytext)){
      mytext=gsub("\\[","'[", mytext)
      mytext=sub("\\(ug","~'(ug", mytext)
    }
    mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
    axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.5)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[!duplicated(myref)|is.na(myref)])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.5)
    }
  }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      if (grepl("m\\^", mytext)){
        mytext=gsub("m\\^","'~m^", mytext)
        mytext=sub(")","~')", mytext)
      }
      mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
      axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
           labels=mytmp, las=2)
    }
  }
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black")
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.7, bg="white")
  dev.off()}

{pdf("OR_bladder_denoise.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(OR_bladder_denoise,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), labels=axTicks(2)+1, cex.axis=0.7)
  mtext(side=4, text="Mean Odds Ratio", line=2, cex.lab=0.7)
  abline(h=0, lty=2)
  for (k in 1:length(xseq)){
    mytext=mylabels[k]
    if (grepl("m\\^", mytext)){
      mytext=gsub("m\\^","'~m^", mytext)
      mytext=sub(")","~')", mytext)
    }
    if (grepl("\\[", mytext)){
      mytext=gsub("\\[","'[", mytext)
      mytext=sub("\\(ug","~'(ug", mytext)
    }
    mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
    axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.5)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[!duplicated(myref)|is.na(myref)])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.5)
    }
  }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      if (grepl("m\\^", mytext)){
        mytext=gsub("m\\^","'~m^", mytext)
        mytext=sub(")","~')", mytext)
      }
      mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
      axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
           labels=mytmp, las=2)
    }
  }
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black")
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.7, bg="white")
  dev.off()}

### Check ----
library(ggrepel)

## Forced
# Extracting pi
pi=c(hat_params_lung.0.1[2],hat_params_lung.0.2[2])
point.lab=plot_annot$label.point
names(point.lab)=plot_annot$col.name

# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_lung.0.1)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_lung.0.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_lung.0.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
dat1=data.frame(pos_b=a/(a+b), sel=selprop_lung.0.1,
               row.names=point.lab[names(selprop_lung.0.1)])
# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_lung.0.2)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_lung.0.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_lung.0.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
dat2=data.frame(pos_b=a/(a+b), sel=selprop_lung.0.2,
                row.names=point.lab[names(selprop_lung.0.2)])
p1=ggplot(dat1, aes(pos_b, sel,
                   label = ifelse((sel>=pi[1]&pos_b>0.4&pos_b<0.6),
                                  paste0(gsub(":", ":\n", rownames(dat1)),"\n",signif(abs(beta_lung.0.1),3)),""))) +
  geom_point(color = ifelse((dat1$sel>=pi[1]&dat1$pos_b>0.4&dat1$pos_b<0.6),"tomato","navy")) +
  geom_text_repel() +
  labs(title = models[1])+
  xlab("Proportion of positive beta among non-zero betas")+
  ylab("Selection Proportion") +
  theme_bw()
p2=ggplot(dat2, aes(pos_b, sel,
                    label = ifelse((sel>=pi[1]&pos_b>0.4&pos_b<0.6),
                                   paste0(gsub(":", ":\n", rownames(dat2)),"\n",signif(abs(beta_lung.0.2),3)),""))) +
  geom_point(color = ifelse((dat2$sel>=pi[1]&dat2$pos_b>0.4&dat2$pos_b<0.6),"tomato","navy")) +
  geom_text_repel() +
  labs(title = models[2])+
  xlab("Proportion of positive beta among non-zero betas")+
  ylab("Selection Proportion") +
  theme_bw()

pdf("selprop_check_lung_force.pdf", width=10, height=5)
gridExtra::grid.arrange(p1, p2, ncol = 2)
dev.off()

# Extracting pi
pi=c(hat_params_bladder.0.1[2],hat_params_bladder.0.2[2])
point.lab=plot_annot$label.point
names(point.lab)=plot_annot$col.name

# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_bladder.0.1)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_bladder.0.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_bladder.0.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
dat1=data.frame(pos_b=a/(a+b), sel=selprop_bladder.0.1,
                row.names=point.lab[names(selprop_bladder.0.1)])
# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_bladder.0.2)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_bladder.0.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_bladder.0.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
dat2=data.frame(pos_b=a/(a+b), sel=selprop_bladder.0.2,
                row.names=point.lab[names(selprop_bladder.0.2)])
p1=ggplot(dat1, aes(pos_b, sel,
                    label = ifelse((sel>=pi[1]&pos_b>0.4&pos_b<0.6),
                                   paste0(gsub(":", ":\n", rownames(dat1)),"\n",signif(abs(beta_bladder.0.1),3)),""))) +
  geom_point(color = ifelse((dat1$sel>=pi[1]&dat1$pos_b>0.4&dat1$pos_b<0.6),"tomato","navy")) +
  geom_text_repel() +
  labs(title = models[1])+
  xlab("Proportion of positive beta among non-zero betas")+
  ylab("Selection Proportion") +
  theme_bw()
p2=ggplot(dat2, aes(pos_b, sel,
                    label = ifelse((sel>=pi[1]&pos_b>0.4&pos_b<0.6),
                                   paste0(gsub(":", ":\n", rownames(dat2)),"\n",signif(abs(beta_bladder.0.2),3)),""))) +
  geom_point(color = ifelse((dat2$sel>=pi[1]&dat2$pos_b>0.4&dat2$pos_b<0.6),"tomato","navy")) +
  geom_text_repel() +
  labs(title = models[2])+
  xlab("Proportion of positive beta among non-zero betas")+
  ylab("Selection Proportion") +
  theme_bw()

pdf("selprop_check_bladder_force.pdf", width=10, height=5)
gridExtra::grid.arrange(p1, p2, ncol = 2)
dev.off()

## Denoise
# Extracting pi
pi=c(hat_params_lung.1[2],hat_params_lung.2[2])
point.lab=plot_annot$label.point
names(point.lab)=plot_annot$col.name

# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_lung.1)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_lung.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_lung.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
dat1=data.frame(pos_b=a/(a+b), sel=selprop_lung.1,
                row.names=point.lab[names(selprop_lung.1)])
# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_lung.2)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_lung.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_lung.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
dat2=data.frame(pos_b=a/(a+b), sel=selprop_lung.2,
                row.names=point.lab[names(selprop_lung.2)])
p1=ggplot(dat1, aes(pos_b, sel,
                    label = ifelse((sel>=pi[1]&pos_b>0.4&pos_b<0.6),
                                   paste0(gsub(":", ":\n", rownames(dat1)),"\n",signif(abs(beta_lung.1),3)),""))) +
  geom_point(color = ifelse((dat1$sel>=pi[1]&dat1$pos_b>0.4&dat1$pos_b<0.6),"tomato","navy")) +
  geom_text_repel() +
  labs(title = models[1])+
  xlab("Proportion of positive beta among non-zero betas")+
  ylab("Selection Proportion") +
  theme_bw()
p2=ggplot(dat2, aes(pos_b, sel,
                    label = ifelse((sel>=pi[1]&pos_b>0.4&pos_b<0.6),
                                   paste0(gsub(":", ":\n", rownames(dat2)),"\n",signif(abs(beta_lung.2),3)),""))) +
  geom_point(color = ifelse((dat2$sel>=pi[1]&dat2$pos_b>0.4&dat2$pos_b<0.6),"tomato","navy")) +
  geom_text_repel() +
  labs(title = models[2])+
  xlab("Proportion of positive beta among non-zero betas")+
  ylab("Selection Proportion") +
  theme_bw()

pdf("selprop_check_lung_denoise.pdf", width=10, height=5)
gridExtra::grid.arrange(p1, p2, ncol = 2)
dev.off()

# Extracting pi
pi=c(hat_params_bladder.1[2],hat_params_bladder.2[2])
point.lab=plot_annot$label.point
names(point.lab)=plot_annot$col.name

# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_bladder.1)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_bladder.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_bladder.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
dat1=data.frame(pos_b=a/(a+b), sel=selprop_bladder.1,
                row.names=point.lab[names(selprop_bladder.1)])
# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_bladder.2)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_bladder.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_bladder.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
dat2=data.frame(pos_b=a/(a+b), sel=selprop_bladder.2,
                row.names=point.lab[names(selprop_bladder.2)])
p1=ggplot(dat1, aes(pos_b, sel,
                    label = ifelse((sel>=pi[1]&pos_b>0.4&pos_b<0.6),
                                   paste0(gsub(":", ":\n", rownames(dat1)),"\n",signif(abs(beta_bladder.1),3)),""))) +
  geom_point(color = ifelse((dat1$sel>=pi[1]&dat1$pos_b>0.4&dat1$pos_b<0.6),"tomato","navy")) +
  geom_text_repel() +
  labs(title = models[1])+
  xlab("Proportion of positive beta among non-zero betas")+
  ylab("Selection Proportion") +
  theme_bw()
p2=ggplot(dat2, aes(pos_b, sel,
                    label = ifelse((sel>=pi[1]&pos_b>0.4&pos_b<0.6),
                                   paste0(gsub(":", ":\n", rownames(dat2)),"\n",signif(abs(beta_bladder.2),3)),""))) +
  geom_point(color = ifelse((dat2$sel>=pi[1]&dat2$pos_b>0.4&dat2$pos_b<0.6),"tomato","navy")) +
  geom_text_repel() +
  labs(title = models[2])+
  xlab("Proportion of positive beta among non-zero betas")+
  ylab("Selection Proportion") +
  theme_bw()

pdf("selprop_check_bladder_denoise.pdf", width=10, height=5)
gridExtra::grid.arrange(p1, p2, ncol = 2)
dev.off()

### AUC plots ----
# Load results
setwd("../../Results/lasso")
auc_lung.0.1=readRDS("auc_lung.0.1.rds")
auc_lung.0.2=readRDS("auc_lung.0.2.rds")
auc_lung.1=readRDS("auc_lung.1.rds")
auc_lung.2=readRDS("auc_lung.2.rds")

auc_bladder.0.1=readRDS("auc_bladder.0.1.rds")
auc_bladder.0.2=readRDS("auc_bladder.0.2.rds")
auc_bladder.1=readRDS("auc_bladder.1.rds")
auc_bladder.2=readRDS("auc_bladder.2.rds")

# AUC as a function of the number of predictors (red line = calibrated model)
## Lung.0.1
calib=sum(CalibratedStableRegression(out_lung.0.1))
point.col=darken(c(rep("grey95",20),
               rep("tomato", 33),
               rep("forestgreen", 8),
               rep("royalblue", 16), 
               rep("gold", 28)),
               amount=0.5)
names(point.col)=plot_annot$col.name
setwd("../../Figures/lasso")
pdf("auc_lung.0.1.pdf", width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc_lung.0.1[,2]), y=as.numeric(auc_lung.0.1[,2]),
       li=as.numeric(auc_lung.0.1[,1]), ui=as.numeric(auc_lung.0.1[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, col=point.col[rownames(auc_lung.0.1)], cex=0.5,
       ylab="AUC", ylim=c(0.4,1))
for (k in 1:length(auc_lung.0.1[,2])){
  mytext=point.lab[rownames(auc_lung.0.1)][k]
  labcol=point.col[rownames(auc_lung.0.1)][k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
       col.axis=labcol, col=labcol, font=labfont)
}
abline(v=calib, col=point.col[rownames(auc_lung.0.1)][calib], lty=4, lwd=2)
abline(v=7, col="black", lty=3, lwd=2)
abline(h=as.numeric(auc_lung.0.1[7,2]), col="black", lty=3, lwd=2)
abline(h=as.numeric(auc_lung.0.1[calib,2]), col=point.col[rownames(auc_lung.0.1)][calib], lty=4, lwd=2)
legend("topright", lty=c(4,3), lwd=2, col=c(point.col[rownames(auc_lung.0.1)][calib],"black"),
       legend = c(paste0("Calibrated model; AUC = ",auc_lung.0.1[calib,2]),
                  paste0("Emplty model; AUC = ",auc_lung.0.1[7,2])))
dev.off()

## Lung.0.2
calib=sum(CalibratedStableRegression(out_lung.0.2))
pdf("auc_lung.0.2.pdf", width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc_lung.0.2[,2]), y=as.numeric(auc_lung.0.2[,2]),
       li=as.numeric(auc_lung.0.2[,1]), ui=as.numeric(auc_lung.0.2[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, col=point.col[rownames(auc_lung.0.2)], cex=0.5,
       ylab="AUC", ylim=c(0.4,1))
for (k in 1:length(auc_lung.0.2[,2])){
  mytext=point.lab[rownames(auc_lung.0.2)][k]
  labcol=point.col[rownames(auc_lung.0.2)][k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
       col.axis=labcol, col=labcol, font=labfont)
}
abline(v=calib, col=point.col[rownames(auc_lung.0.2)][calib], lty=4, lwd=2)
abline(v=11, col="black", lty=3, lwd=2)
abline(h=as.numeric(auc_lung.0.2[calib,2]), col=point.col[rownames(auc_lung.0.2)][calib], lty=4, lwd=2)
abline(h=as.numeric(auc_lung.0.2[11,2]), col="black", lty=3, lwd=2)
legend("topright", lty=c(4,3), lwd=2, col=c(point.col[rownames(auc_lung.0.2)][calib],"black"),
       legend = c(paste0("Calibrated model; AUC = ",auc_lung.0.2[calib,2]),
                  paste0("Emplty model; AUC = ",auc_lung.0.2[11,2])))
dev.off()

## bladder.0.1
calib=sum(CalibratedStableRegression(out_bladder.0.1))
pdf("auc_bladder.0.1.pdf", width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc_bladder.0.1[,2]), y=as.numeric(auc_bladder.0.1[,2]),
       li=as.numeric(auc_bladder.0.1[,1]), ui=as.numeric(auc_bladder.0.1[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, col=point.col[rownames(auc_bladder.0.1)], cex=0.5,
       ylab="AUC", ylim=c(0.4,1))
for (k in 1:length(auc_bladder.0.1[,2])){
  mytext=point.lab[rownames(auc_bladder.0.1)][k]
  labcol=point.col[rownames(auc_bladder.0.1)][k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
       col.axis=labcol, col=labcol, font=labfont)
}
abline(v=calib, col=point.col[rownames(auc_bladder.0.1)][calib], lty=4, lwd=2)
abline(v=7, col="black", lty=3, lwd=2)
abline(h=as.numeric(auc_bladder.0.1[calib,2]), col=point.col[rownames(auc_bladder.0.1)][calib], lty=4, lwd=2)
abline(h=as.numeric(auc_bladder.0.1[7,2]), col="black", lty=3, lwd=2)
legend("topright", lty=c(4,3), lwd=2, col=c(point.col[rownames(auc_bladder.0.1)][calib],"black"),
       legend = c(paste0("Calibrated model; AUC = ",auc_bladder.0.1[calib,2]),
                  paste0("Emplty model; AUC = ",auc_bladder.0.1[7,2])))
dev.off()

## bladder.0.2
calib=sum(CalibratedStableRegression(out_bladder.0.2))
pdf("auc_bladder.0.2.pdf", width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc_bladder.0.2[,2]), y=as.numeric(auc_bladder.0.2[,2]),
       li=as.numeric(auc_bladder.0.2[,1]), ui=as.numeric(auc_bladder.0.2[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, col=point.col[rownames(auc_bladder.0.2)], cex=0.5,
       ylab="AUC", ylim=c(0.4,1))
for (k in 1:length(auc_bladder.0.2[,2])){
  mytext=point.lab[rownames(auc_bladder.0.2)][k]
  labcol=point.col[rownames(auc_bladder.0.2)][k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
       col.axis=labcol, col=labcol, font=labfont)
}
abline(v=11, col="black", lty=3, lwd=2)
abline(h=as.numeric(auc_bladder.0.2[11,2]), col="black", lty=3, lwd=2)
legend("topright", lty=3, lwd=2, col="black",
       legend = paste0("Emplty model; AUC = ",auc_bladder.0.2[11,2]))
dev.off()

## Lung.1
calib=sum(CalibratedStableRegression(out_lung.1))
setwd("../../Figures/lasso")
pdf("auc_lung.1.pdf", width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc_lung.1[,2]), y=as.numeric(auc_lung.1[,2]),
       li=as.numeric(auc_lung.1[,1]), ui=as.numeric(auc_lung.1[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, col=point.col[rownames(auc_lung.1)], cex=0.5,
       ylab="AUC", ylim=c(0.4,1))
for (k in 1:length(auc_lung.1[,2])){
  mytext=point.lab[rownames(auc_lung.1)][k]
  labcol=point.col[rownames(auc_lung.1)][k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
       col.axis=labcol, col=labcol, font=labfont)
}
abline(v=calib, col=point.col[rownames(auc_lung.1)][calib], lty=4, lwd=2)
abline(h=as.numeric(auc_lung.1[calib,2]), col=point.col[rownames(auc_lung.1)][calib], lty=4, lwd=2)
legend("topright", lty=4, lwd=2, col=point.col[rownames(auc_lung.1)][calib],
       legend = paste0("Calibrated model; AUC = ",auc_lung.1[calib,2]))
dev.off()

## Lung.2
calib=sum(CalibratedStableRegression(out_lung.2))
pdf("auc_lung.2.pdf", width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc_lung.2[,2]), y=as.numeric(auc_lung.2[,2]),
       li=as.numeric(auc_lung.2[,1]), ui=as.numeric(auc_lung.2[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, col=point.col[rownames(auc_lung.2)], cex=0.5,
       ylab="AUC", ylim=c(0.4,1))
for (k in 1:length(auc_lung.2[,2])){
  mytext=point.lab[rownames(auc_lung.2)][k]
  labcol=point.col[rownames(auc_lung.2)][k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
       col.axis=labcol, col=labcol, font=labfont)
}
abline(v=calib, col=point.col[rownames(auc_lung.2)][calib], lty=4, lwd=2)
abline(h=as.numeric(auc_lung.2[calib,2]), col=point.col[rownames(auc_lung.2)][calib], lty=4, lwd=2)
legend("topright", lty=4, lwd=2, col=point.col[rownames(auc_lung.2)][calib],
       legend = paste0("Calibrated model; AUC = ",auc_lung.2[calib,2]))
dev.off()

## bladder.1
calib=sum(CalibratedStableRegression(out_bladder.1))
setwd("../../Figures/lasso")
pdf("auc_bladder.1.pdf", width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc_bladder.1[,2]), y=as.numeric(auc_bladder.1[,2]),
       li=as.numeric(auc_bladder.1[,1]), ui=as.numeric(auc_bladder.1[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, col=point.col[rownames(auc_bladder.1)], cex=0.5,
       ylab="AUC", ylim=c(0.4,1))
for (k in 1:length(auc_bladder.1[,2])){
  mytext=point.lab[rownames(auc_bladder.1)][k]
  labcol=point.col[rownames(auc_bladder.1)][k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
       col.axis=labcol, col=labcol, font=labfont)
}
abline(v=calib, col=point.col[rownames(auc_bladder.1)][calib], lty=4, lwd=2)
abline(h=as.numeric(auc_bladder.1[calib,2]), col=point.col[rownames(auc_bladder.1)][calib], lty=4, lwd=2)
legend("topright", lty=4, lwd=2, col=point.col[rownames(auc_bladder.1)][calib],
       legend = paste0("Calibrated model; AUC = ",auc_bladder.1[calib,2]))
dev.off()

## bladder.2
calib=sum(CalibratedStableRegression(out_bladder.2))
pdf("auc_bladder.2.pdf", width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc_bladder.2[,2]), y=as.numeric(auc_bladder.2[,2]),
       li=as.numeric(auc_bladder.2[,1]), ui=as.numeric(auc_bladder.2[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, col=point.col[rownames(auc_bladder.2)], cex=0.5,
       ylab="AUC", ylim=c(0.4,1))
for (k in 1:length(auc_bladder.2[,2])){
  mytext=point.lab[rownames(auc_bladder.2)][k]
  labcol=point.col[rownames(auc_bladder.2)][k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,
       col.axis=labcol, col=labcol, font=labfont)
}
abline(v=calib, col=point.col[rownames(auc_bladder.2)][calib], lty=4, lwd=2)
abline(h=as.numeric(auc_bladder.2[calib,2]), col=point.col[rownames(auc_bladder.2)][calib], lty=4, lwd=2)
legend("topright", lty=4, lwd=2, col=point.col[rownames(auc_bladder.2)][calib],
       legend = paste0("Calibrated model; AUC = ",auc_bladder.2[calib,2]))
dev.off()

