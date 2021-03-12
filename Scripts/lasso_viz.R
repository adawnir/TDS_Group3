### TDS Project -- Stability selection LASSO Logistic Regression Visualisation
## Programme created by Rin Wada on 11 March

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

### OR plots ----
# Load results
setwd("../Results/lasso")
beta_lung.0.1=readRDS("average_beta_lung.0.1.rds")
beta_lung.0.2=readRDS("average_beta_lung.0.2.rds")
beta_lung.1=readRDS("average_beta_lung.1.rds")
beta_lung.2=readRDS("average_beta_lung.2.rds")

beta_bladder.0.1=readRDS("average_beta_bladder.0.1.rds")
beta_bladder.0.2=readRDS("average_beta_bladder.0.2.rds")
beta_bladder.1=readRDS("average_beta_bladder.1.rds")
beta_bladder.2=readRDS("average_beta_bladder.2.rds")

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

background=TRUE
MyPal = brewer.pal("Paired", n = 12)
mycolours=c(MyPal[1], MyPal[2])

myspacing=5
xseq=seq(3,length(myorder)*myspacing, by=myspacing)

dir.create("../../Figures/lasso")
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
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6)
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
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6)
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

background=TRUE
MyPal = brewer.pal("Paired", n = 12)
mycolours=c(MyPal[3], MyPal[4])

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
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6)
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
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6)
  dev.off()}


### Selection proportions ----
# Load lasso output
setwd("../../Results/lasso")
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

## Forced
mylabels=plot_annot$label
myref=plot_annot$ref
variable_cat=c(rep("Sociodemographic",20),
               rep("Health risk", 33),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
models=c("Base model", "Model adjusted on smoking status")
pi=max(c(hat_params_lung.0.1[2],hat_params_lung.0.2[2]))
background=TRUE
MyPal = brewer.pal("Paired", n = 12)
mycolours=c(MyPal[1], MyPal[2])

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
  abline(h=pi, lty=2, col="darkred")
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
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6)
  dev.off()}

pi=max(c(hat_params_bladder.0.1[2],hat_params_bladder.0.2[2]))
{pdf("selprop_bladder_force.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(selprop_bladder_force,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), cex.axis=0.7)
  mtext(side=4, text="Selection Proportion", line=2, cex.lab=0.7)
  abline(h=pi, lty=2, col="darkred")
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
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6)
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
pi=max(c(hat_params_lung.1[2],hat_params_lung.2[2]))
background=TRUE
MyPal = brewer.pal("Paired", n = 12)
mycolours=c(MyPal[3], MyPal[4])

myspacing=5
xseq=seq(3,length(myorder)*myspacing, by=myspacing)

{pdf("selprop_lung_denoise.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(selprop_lung_denoise,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), cex.axis=0.7)
  mtext(side=4, text="Selection Proportion", line=2, cex.lab=0.7)
  abline(h=pi, lty=2, col="darkred")
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
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6)
  dev.off()}

pi=max(c(hat_params_bladder.1[2],hat_params_bladder.2[2]))
{pdf("selprop_bladder_denoise.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(selprop_bladder_denoise,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), cex.axis=0.7)
  mtext(side=4, text="Selection Proportion", line=2, cex.lab=0.7)
  abline(h=pi, lty=2, col="darkred")
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
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6)
  dev.off()}


### Check ----
## Lung
# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_lung.0.1)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_lung.0.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_lung.0.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
pdf("selprop_check_lung.0.1.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
plot(a/(a+b), selprop_lung.0.1, las=1, pch=19, col="navy", cex.lab=1.5,
     xlab="Proportion of positive beta among non-zero betas", 
     ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
dev.off()
# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_lung.0.2)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_lung.0.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_lung.0.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
pdf("selprop_check_lung.0.2.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
plot(a/(a+b), selprop_lung.0.2, las=1, pch=19, col="navy", cex.lab=1.5,
     xlab="Proportion of positive beta among non-zero betas", 
     ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
dev.off()
# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_lung.1)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_lung.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_lung.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
pdf("selprop_check_lung.1.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
plot(a/(a+b), selprop_lung.1, las=1, pch=19, col="navy", cex.lab=1.5,
     xlab="Proportion of positive beta among non-zero betas", 
     ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
dev.off()
# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_lung.2)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_lung.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_lung.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
pdf("selprop_check_lung.2.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
plot(a/(a+b), selprop_lung.2, las=1, pch=19, col="navy", cex.lab=1.5,
     xlab="Proportion of positive beta among non-zero betas", 
     ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
dev.off()

## Bladder
# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_bladder.0.1)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_bladder.0.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_bladder.0.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
pdf("selprop_check_bladder.0.1.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
plot(a/(a+b), selprop_bladder.0.1, las=1, pch=19, col="navy", cex.lab=1.5,
     xlab="Proportion of positive beta among non-zero betas", 
     ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
dev.off()
# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_bladder.0.2)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_bladder.0.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_bladder.0.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
pdf("selprop_check_bladder.0.2.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
plot(a/(a+b), selprop_bladder.0.2, las=1, pch=19, col="navy", cex.lab=1.5,
     xlab="Proportion of positive beta among non-zero betas", 
     ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
dev.off()
# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_bladder.1)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_bladder.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_bladder.1$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
pdf("selprop_check_bladder.1.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
plot(a/(a+b), selprop_bladder.1, las=1, pch=19, col="navy", cex.lab=1.5,
     xlab="Proportion of positive beta among non-zero betas", 
     ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
dev.off()
# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out_bladder.2)[1]
# Checking consistency in sign of the beta coefficients for the variables with high selprop
a=apply(out_bladder.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x>0)})
b=apply(out_bladder.2$Beta[hat_lambda_id,,],1,FUN=function(x){sum(x<0)})
pdf("selprop_check_bladder.2.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
plot(a/(a+b), selprop_bladder.2, las=1, pch=19, col="navy", cex.lab=1.5,
     xlab="Proportion of positive beta among non-zero betas", 
     ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
dev.off()

### AUC plots ----
# Load results
setwd("../../Results/lasso")
auc_lung.0.1=readRDS("auc_lung.0.1.rds")
auc_lung.0.2=readRDS("average_auc_lung.0.2.rds")
auc_lung.1=readRDS("auc_lung.1.rds")
auc_lung.2=readRDS("auc_lung.2.rds")

auc_bladder.0.1=readRDS("auc_bladder.0.1.rds")
auc_bladder.0.2=readRDS("auc_bladder.0.2.rds")
auc_bladder.1=readRDS("auc_bladder.1.rds")
auc_bladder.2=readRDS("auc_bladder.2.rds")

# AUC as a function of the number of predictors (red line = calibrated model)
## Lung
setwd("../../Figures/lasso")
pdf("auc_lung.0.1.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
calib=sum(CalibratedStableRegression(out_lung.0.1))
plotCI(x=seq(1,length(auc_lung.0.1[,2]),1), y=as.numeric(auc_lung.0.1[,2]),
       li=as.numeric(auc_lung.0.1[,1]), ui=as.numeric(auc_lung.0.1[,3]),
       xlab="Number of predictors", pch=19, col="navy", cex=0.5,
       ylab="AUC", cex.lab=1, las=1, ylim=c(0,1))
abline(v=calib, col="red", lty=2)
legend("bottomright", lty=2, lwd=2, col="red",
       legend = paste0("Calibrated model; AUC = ",auc_lung.0.1[calib,2]))
dev.off()
pdf("auc_lung.0.2.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
calib=sum(CalibratedStableRegression(out_lung.0.2))
plotCI(x=seq(1,length(auc_lung.0.2[,2]),1), y=as.numeric(auc_lung.0.2[,2]),
       li=as.numeric(auc_lung.0.2[,1]), ui=as.numeric(auc_lung.0.2[,3]),
       xlab="Number of predictors", pch=19, col="navy", cex=0.5,
       ylab="AUC", cex.lab=1, las=1, ylim=c(0,1))
abline(v=calib, col="red", lty=2)
legend("bottomright", lty=2, lwd=2, col="red",
       legend = paste0("Calibrated model; AUC = ",auc_lung.0.2[calib,2]))
dev.off()

pdf("auc_lung.1.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
calib=sum(CalibratedStableRegression(out_lung.1))
plotCI(x=seq(1,length(auc_lung.1[,2]),1), y=as.numeric(auc_lung.1[,2]),
       li=as.numeric(auc_lung.1[,1]), ui=as.numeric(auc_lung.1[,3]),
       xlab="Number of predictors", pch=19, col="navy", cex=0.5,
       ylab="AUC", cex.lab=1, las=1, ylim=c(0,1))
abline(v=calib, col="red", lty=2)
legend("bottomright", lty=2, lwd=2, col="red",
       legend = paste0("Calibrated model; AUC = ",auc_lung.1[calib,2]))
dev.off()
pdf("auc_lung.2.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
calib=sum(CalibratedStableRegression(out_lung.2))
plotCI(x=seq(1,length(auc_lung.2[,2]),1), y=as.numeric(auc_lung.2[,2]),
       li=as.numeric(auc_lung.2[,1]), ui=as.numeric(auc_lung.2[,3]),
       xlab="Number of predictors", pch=19, col="navy", cex=0.5,
       ylab="AUC", cex.lab=1, las=1, ylim=c(0,1))
abline(v=calib, col="red", lty=2)
legend("bottomright", lty=2, lwd=2, col="red",
       legend = paste0("Calibrated model; AUC = ",auc_lung.2[calib,2]))
dev.off()

## bladder
pdf("auc_bladder.0.1.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
calib=sum(CalibratedStableRegression(out_bladder.0.1))
plotCI(x=seq(1,length(auc_bladder.0.1[,2]),1), y=as.numeric(auc_bladder.0.1[,2]),
       li=as.numeric(auc_bladder.0.1[,1]), ui=as.numeric(auc_bladder.0.1[,3]),
       xlab="Number of predictors", pch=19, col="navy", cex=0.5,
       ylab="AUC", cex.lab=1, las=1, ylim=c(0,1))
abline(v=calib, col="red", lty=2)
legend("bottomright", lty=2, lwd=2, col="red",
       legend = paste0("Calibrated model; AUC = ",auc_bladder.0.1[calib,2]))
dev.off()
pdf("auc_bladder.0.2.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
calib=sum(CalibratedStableRegression(out_bladder.0.2))
plotCI(x=seq(1,length(auc_bladder.0.2[,2]),1), y=as.numeric(auc_bladder.0.2[,2]),
       li=as.numeric(auc_bladder.0.2[,1]), ui=as.numeric(auc_bladder.0.2[,3]),
       xlab="Number of predictors", pch=19, col="navy", cex=0.5,
       ylab="AUC", cex.lab=1, las=1, ylim=c(0,1))
abline(v=calib, col="red", lty=2)
legend("bottomright", lty=2, lwd=2, col="red",
       legend = paste0("Calibrated model; AUC = ",auc_bladder.0.2[calib,2]))
dev.off()

pdf("auc_bladder.1.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
calib=sum(CalibratedStableRegression(out_bladder.1))
plotCI(x=seq(1,length(auc_bladder.1[,2]),1), y=as.numeric(auc_bladder.1[,2]),
       li=as.numeric(auc_bladder.1[,1]), ui=as.numeric(auc_bladder.1[,3]),
       xlab="Number of predictors", pch=19, col="navy", cex=0.5,
       ylab="AUC", cex.lab=1, las=1, ylim=c(0,1))
abline(v=calib, col="red", lty=2)
legend("bottomright", lty=2, lwd=2, col="red",
       legend = paste0("Calibrated model; AUC = ",auc_bladder.1[calib,2]))
dev.off()
pdf("auc_bladder.2.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
calib=sum(CalibratedStableRegression(out_bladder.2))
plotCI(x=seq(1,length(auc_bladder.2[,2]),1), y=as.numeric(auc_bladder.2[,2]),
       li=as.numeric(auc_bladder.2[,1]), ui=as.numeric(auc_bladder.2[,3]),
       xlab="Number of predictors", pch=19, col="navy", cex=0.5,
       ylab="AUC", cex.lab=1, las=1, ylim=c(0,1))
abline(v=calib, col="red", lty=2)
legend("bottomright", lty=2, lwd=2, col="red",
       legend = paste0("Calibrated model; AUC = ",auc_bladder.2[calib,2]))
dev.off()
