### TDS Project -- Stratified analysis by sex: Stability selection LASSO Logistic Regression Visualisation
## Programme created by Rin Wada on 15 March

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
plot_annot=plot_annot[-c(1,2,21:25),] # Remove age, sex and BMI

### Load data ----
setwd("../Results/strat_sex_lasso")
# Load output
out_lung.f.1=readRDS("out_lung.f.1.rds")
out_lung.f.2=readRDS("out_lung.f.2.rds")
out_lung.m.1=readRDS("out_lung.m.1.rds")
out_lung.m.2=readRDS("out_lung.m.2.rds")

out_bladder.f.1=readRDS("out_bladder.f.1.rds")
out_bladder.f.2=readRDS("out_bladder.f.2.rds")
out_bladder.m.1=readRDS("out_bladder.m.1.rds")
out_bladder.m.2=readRDS("out_bladder.m.2.rds")

# Load selection proportion
selprop_lung.f.1=readRDS("selprop_lung.f.1.rds")
selprop_lung.f.2=readRDS("selprop_lung.f.2.rds")
selprop_lung.m.1=readRDS("selprop_lung.m.1.rds")
selprop_lung.m.2=readRDS("selprop_lung.m.2.rds")

selprop_bladder.f.1=readRDS("selprop_bladder.f.1.rds")
selprop_bladder.f.2=readRDS("selprop_bladder.f.2.rds")
selprop_bladder.m.1=readRDS("selprop_bladder.m.1.rds")
selprop_bladder.m.2=readRDS("selprop_bladder.m.2.rds")

# Extract calibrated pi
hat_params_lung.f.1=GetArgmax(out_lung.f.1)
hat_params_lung.f.2=GetArgmax(out_lung.f.2)
hat_params_lung.m.1=GetArgmax(out_lung.m.1)
hat_params_lung.m.2=GetArgmax(out_lung.m.2)

hat_params_bladder.f.1=GetArgmax(out_bladder.f.1)
hat_params_bladder.f.2=GetArgmax(out_bladder.f.2)
hat_params_bladder.m.1=GetArgmax(out_bladder.m.1)
hat_params_bladder.m.2=GetArgmax(out_bladder.m.2)

# Lung
selprop_lung=data.frame(selprop_lung.f.1)
selprop_lung$selprop_lung.f.2=selprop_lung.f.2[match(rownames(selprop_lung),
                                                     names(selprop_lung.f.2))]
selprop_lung$selprop_lung.m.1=selprop_lung.m.1[match(rownames(selprop_lung),
                                                     names(selprop_lung.m.1))]
selprop_lung$selprop_lung.m.2=selprop_lung.m.2[match(rownames(selprop_lung),
                                                     names(selprop_lung.m.2))]

# Bladder
selprop_bladder=data.frame(selprop_bladder.f.1)
selprop_bladder$selprop_bladder.f.2=selprop_bladder.f.2[match(rownames(selprop_bladder),
                                                     names(selprop_bladder.f.2))]
selprop_bladder$selprop_bladder.m.1=selprop_bladder.m.1[match(rownames(selprop_bladder),
                                                     names(selprop_bladder.m.1))]
selprop_bladder$selprop_bladder.m.2=selprop_bladder.m.2[match(rownames(selprop_bladder),
                                                     names(selprop_bladder.m.2))]
# Reorder rows
myorder=plot_annot$col.name
selprop_lung = selprop_lung %>%
  slice(match(myorder,rownames(selprop_lung)))
selprop_bladder = selprop_bladder %>%
  slice(match(myorder,rownames(selprop_bladder)))

# Convert data.frame to vector with two empty entries between variables
selprop_lung=cbind(rep(NA,nrow(selprop_lung)),
                   selprop_lung[,1],
                   rep(NA, nrow(selprop_lung)),
                   selprop_lung[,2],
                   rep(NA,nrow(selprop_lung)),
                   selprop_lung[,3],
                   rep(NA,nrow(selprop_lung)),
                   selprop_lung[,4],
                   rep(NA,nrow(selprop_lung)))
selprop_lung=as.vector(t(selprop_lung))

selprop_bladder=cbind(rep(NA,nrow(selprop_bladder)),
                   selprop_bladder[,1],
                   rep(NA, nrow(selprop_bladder)),
                   selprop_bladder[,2],
                   rep(NA,nrow(selprop_bladder)),
                   selprop_bladder[,3],
                   rep(NA,nrow(selprop_bladder)),
                   selprop_bladder[,4],
                   rep(NA,nrow(selprop_bladder)))
selprop_bladder=as.vector(t(selprop_bladder))

# Load beta
beta_lung.f.1=readRDS("average_beta_lung.f.1.rds")
beta_lung.f.2=readRDS("average_beta_lung.f.2.rds")
beta_lung.m.1=readRDS("average_beta_lung.m.1.rds")
beta_lung.m.2=readRDS("average_beta_lung.m.2.rds")

beta_bladder.f.1=readRDS("average_beta_bladder.f.1.rds")
beta_bladder.f.2=readRDS("average_beta_bladder.f.2.rds")
beta_bladder.m.1=readRDS("average_beta_bladder.m.1.rds")
beta_bladder.m.2=readRDS("average_beta_bladder.m.2.rds")

# Shrink non-selected beta to zero
beta_lung.f.1=ifelse(CalibratedStableRegression(out_lung.f.1) == 1, beta_lung.f.1, 0)
beta_lung.f.2=ifelse(CalibratedStableRegression(out_lung.f.2) == 1, beta_lung.f.2, 0)
beta_lung.m.1=ifelse(CalibratedStableRegression(out_lung.m.1) == 1, beta_lung.m.1, 0)
beta_lung.m.2=ifelse(CalibratedStableRegression(out_lung.m.2) == 1, beta_lung.m.2, 0)

beta_bladder.f.1=ifelse(CalibratedStableRegression(out_bladder.f.1) == 1, beta_bladder.f.1, 0)
beta_bladder.f.2=ifelse(CalibratedStableRegression(out_bladder.f.2) == 1, beta_bladder.f.2, 0)
beta_bladder.m.1=ifelse(CalibratedStableRegression(out_bladder.m.1) == 1, beta_bladder.m.1, 0)
beta_bladder.m.2=ifelse(CalibratedStableRegression(out_bladder.m.2) == 1, beta_bladder.m.2, 0)

# Extract beta coefficients
# Lung
beta_lung=data.frame(beta_lung.f.1)
beta_lung$beta_lung.f.2=beta_lung.f.2[match(rownames(beta_lung),
                                                     names(beta_lung.f.2))]
beta_lung$beta_lung.m.1=beta_lung.m.1[match(rownames(beta_lung),
                                                     names(beta_lung.m.1))]
beta_lung$beta_lung.m.2=beta_lung.m.2[match(rownames(beta_lung),
                                                     names(beta_lung.m.2))]

# Bladder
beta_bladder=data.frame(beta_bladder.f.1)
beta_bladder$beta_bladder.f.2=beta_bladder.f.2[match(rownames(beta_bladder),
                                                              names(beta_bladder.f.2))]
beta_bladder$beta_bladder.m.1=beta_bladder.m.1[match(rownames(beta_bladder),
                                                              names(beta_bladder.m.1))]
beta_bladder$beta_bladder.m.2=beta_bladder.m.2[match(rownames(beta_bladder),
                                                              names(beta_bladder.m.2))]
# Reorder rows
myorder=plot_annot$col.name
beta_lung = beta_lung %>%
  slice(match(myorder,rownames(beta_lung)))
beta_bladder = beta_bladder %>%
  slice(match(myorder,rownames(beta_bladder)))

### Selection proportion plot----

mylabels=plot_annot$label
myref=plot_annot$ref
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 28),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
models=c("Female: Base model", "Female: Model adjusted on smoking status",
         "Male: Base model", "Male: Model adjusted on smoking status")
mycolours=c("lightcoral", darken("lightcoral",amount=0.5),"steelblue",darken("steelblue", amount=0.5))

pi=c(hat_params_lung.f.1[2],hat_params_lung.f.2[2],hat_params_lung.m.1[2],hat_params_lung.m.2[2])
myspacing=9
xseq=seq(5,length(myorder)*myspacing, by=myspacing)
setwd("../../Figures/strat_sex_lasso")
{pdf("selprop_lung.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(selprop_lung,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA, mycolours[3], NA, mycolours[4], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), cex.axis=0.7)
  mtext(side=4, text="Selection Proportion", line=2, cex.lab=0.7)
  abline(h=pi, lty=c(2,4,2,4), col=mycolours[1:4])
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
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6, bg="white")
  dev.off()}

pi=c(hat_params_bladder.f.1[2],hat_params_bladder.f.2[2],hat_params_bladder.m.1[2],hat_params_bladder.m.2[2])
myspacing=9
xseq=seq(5,length(myorder)*myspacing, by=myspacing)
{pdf("selprop_bladder.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(selprop_bladder,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA, mycolours[3], NA, mycolours[4], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), cex.axis=0.7)
  mtext(side=4, text="Selection Proportion", line=2, cex.lab=0.7)
  abline(h=pi, lty=c(2,4,2,4), col=mycolours[1:4])
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
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6, bg="white")
  dev.off()}

### OR plots ----
# Convert data.frame to vector with two empty entries between variables
OR_lung=cbind(rep(NA,nrow(beta_lung)),
              exp(beta_lung[,1])-1,
              rep(NA, nrow(beta_lung)),
              exp(beta_lung[,2])-1,
              rep(NA,nrow(beta_lung)),
              exp(beta_lung[,3])-1,
              rep(NA,nrow(beta_lung)),
              exp(beta_lung[,4])-1,
              rep(NA,nrow(beta_lung)))
OR_lung=as.vector(t(OR_lung))

OR_bladder=cbind(rep(NA,nrow(beta_bladder)),
              exp(beta_bladder[,1])-1,
              rep(NA, nrow(beta_bladder)),
              exp(beta_bladder[,2])-1,
              rep(NA,nrow(beta_bladder)),
              exp(beta_bladder[,3])-1,
              rep(NA,nrow(beta_bladder)),
              exp(beta_bladder[,4])-1,
              rep(NA,nrow(beta_bladder)))
OR_bladder=as.vector(t(OR_bladder))

mylabels=plot_annot$label
myref=plot_annot$ref
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 28),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
models=c("Female: Base model", "Female: Model adjusted on smoking status",
         "Male: Base model", "Male: Model adjusted on smoking status")
mycolours=c("lightcoral", darken("lightcoral",amount=0.5),"steelblue",darken("steelblue", amount=0.5))
myspacing=9
xseq=seq(5,length(myorder)*myspacing, by=myspacing)

{pdf("OR_lung.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(OR_lung,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA, mycolours[3], NA, mycolours[4], NA),
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

{pdf("OR_bladder.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(OR_bladder,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA, mycolours[3], NA, mycolours[4], NA),
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

