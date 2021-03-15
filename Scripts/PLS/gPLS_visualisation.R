
### TDS Project -- Stability selection gPLS loading coefficients & selection proportion Visualisation
## Programme created by Vivian on 13 March

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/PLS"
setwd(project_path)

# Loading packages
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)


##########################         Loading coefficients       ########################## 

##### plot labels--
plot_annot=read_csv("../../Dictionaries/plot_annot.csv")

##### Load data--
load_lung.1 <- readRDS("../../Results/PLS/gPLS_loadings_lung_1.rds")
load_lung.2 <- readRDS("../../Results/PLS/gPLS_loadings_lung_2.rds")
load_bladder.1 <- readRDS("../../Results/PLS/gPLS_loadings_bladder_1.rds")
load_bladder.2 <- readRDS("../../Results/PLS/gPLS_loadings_bladder_2.rds")


##### Extract loading coefficients--
## Lung
load_lung = data.frame(load_lung.1)
load_lung$load_lung.2 <- load_lung.2[match(rownames(load_lung), names(load_lung.2))]

# Reorder rows
myorder <- plot_annot$col.name
myorder2 <- myorder[-c(1,2,21:25)]
load_lung <- load_lung %>%
  slice(match(myorder2,rownames(load_lung)))

## bladder
load_bladder = data.frame(load_bladder.1)
load_bladder$load_bladder.2 <- load_bladder.2[match(rownames(load_bladder), names(load_bladder.2))]

# Reorder rows
load_bladder <- load_bladder %>%
  slice(match(myorder2,rownames(load_bladder)))


##### Convert data.frame to vector with two empty entries between variables--

LC_lung <- cbind(rep(NA,nrow(load_lung)), 
                 load_lung[,1],
                 rep(NA, nrow(load_lung)),
                 load_lung[,2],
                 rep(NA,nrow(load_lung)))
LC_lung <- as.vector(t(LC_lung))

LC_bladder <- cbind(rep(NA,nrow(load_bladder)), 
                    load_bladder[,1],
                    rep(NA, nrow(load_bladder)),
                    load_bladder[,2],
                    rep(NA,nrow(load_bladder)))
LC_bladder <- as.vector(t(LC_bladder))


##### Settings
mylabels <- plot_annot$label[-c(1,2,21:25)]
myref <- plot_annot$ref[-c(1,2,21:25)]
variable_cat <- c(rep("Sociodemographic",18), 
                  rep("Health risk", 28),
                  rep("Environmental", 8),
                  rep("Medical", 16), 
                  rep("Biomarkers", 28))
models=c("Base model", "Model adjusted on smoking status")

background=TRUE
MyPal = brewer.pal("Paired", n = 12)
mycolours=darken(c("navy","tomato"), amount=0.2)

myspacing <- 5
xseq <- seq(3,length(myorder2)*myspacing, by=myspacing)

##### Plot

{pdf("gPLS_LC_lung.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(LC_lung,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), labels=axTicks(2), cex.axis=0.7)
  mtext(side=4, text="Average Loading Coefficients", line=2, cex.lab=0.7)
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
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6, bty = "n")
  dev.off()}


{pdf("gPLS_LC_bladder.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(LC_bladder,
       xaxt="n", yaxt="n",
       type="h", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  axis(side=4, at=axTicks(2), labels=axTicks(2), cex.axis=0.7)
  mtext(side=4, text="Average Loading Coefficients", line=2, cex.lab=0.7)
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
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6, bty = "n")
  dev.off()}



##########################        Selection proportions       ########################## 

##### Load data--
out_lung_1 <- readRDS("../../Results/PLS/gPLS_stability_selection_lung.1.rds")
out_lung_2 <- readRDS("../../Results/PLS/gPLS_stability_selection_lung.2.rds")
out_bladder_1 <- readRDS("../../Results/PLS/gPLS_stability_selection_bladder.1.rds")
out_bladder_2 <- readRDS("../../Results/PLS/gPLS_stability_selection_bladder.2.rds")


##### Extract selection proportion and calibrated parameters--
source("template/penalisation_functions.R")

selprop1 <- CalibratedSelectionProportionsRegression(out_bladder_1 , plot=FALSE)
selprop2 <- CalibratedSelectionProportionsRegression(out_bladder_2 , plot=FALSE)
selprop_1 <- CalibratedSelectionProportionsRegression(out_lung_1, plot=FALSE)
selprop_2 <- CalibratedSelectionProportionsRegression(out_lung_2, plot=FALSE)

hat_params1 <- GetArgmax(out_bladder_1)
hat_params2 <- GetArgmax(out_bladder_2)
hat_params_1 <- GetArgmax(out_lung_1)
hat_params_2 <- GetArgmax(out_lung_2)


##### Combine two models--
selprop_lung <- data.frame(selprop_1)
selprop_lung$selprop_2=selprop_2[match(rownames(selprop_lung), names(selprop_2))]

selprop_bladder <- data.frame(selprop1)
selprop_bladder$selprop2 <- selprop2[match(rownames(selprop_bladder), names(selprop2))]

# Reorder rows
myorder2 <- myorder[-c(1,2,21:25)]
selprop_lung = selprop_lung %>%
  slice(match(myorder2,rownames(selprop_lung)))
selprop_bladder <- selprop_bladder %>%
  slice(match(myorder2,rownames(selprop_bladder)))


##### Convert data.frame to vector with two empty entries between variables
SP_lung <- cbind(rep(NA,nrow(selprop_lung)),
                 selprop_lung[,1],
                 rep(NA, nrow(selprop_lung)),
                 selprop_lung[,2],
                 rep(NA,nrow(selprop_lung)))
SP_lung <- as.vector(t(SP_lung))

SP_bladder <- cbind(rep(NA,nrow(selprop_bladder)),
                    selprop_bladder[,1],
                    rep(NA, nrow(selprop_bladder)),
                    selprop_bladder[,2],
                    rep(NA,nrow(selprop_bladder)))
SP_bladder <- as.vector(t(SP_bladder))


##### Settings
mylabels <- plot_annot$label[-c(1,2,21:25)]
myref <- plot_annot$ref[-c(1,2,21:25)]
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 28),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
models=c("Base model", "Model adjusted on smoking status")

background=TRUE
MyPal = brewer.pal("Paired", n = 12)
mycolours=darken(c("navy","tomato","gold3"), amount=0.2)

myspacing <- 5
xseq <- seq(3,length(myorder2)*myspacing, by=myspacing)


##### Plots
pi= c(hat_params_1[2],hat_params_2[2])

{pdf("gPLS_selprop_lung.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(SP_lung,
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
    a=selprop_1[myorder2]
    a[which(is.na(a))]=0
    b=selprop_2[myorder2]
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
  legend("topright", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6, bty = "n")
  dev.off()}


pi=c(hat_params1[2],hat_params2[2])
{pdf("gPLS_selprop_bladder.pdf", width=10, height=7)
  par(mar=c(17, 1, 3, 5))
  plot(SP_bladder,
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
    a=selprop1[myorder2]
    a[which(is.na(a))]=0
    b=selprop2[myorder2]
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
  legend("topleft", lty=1, lwd=2, col=mycolours, legend = models, cex=0.6, bty = "n")
  dev.off()}





