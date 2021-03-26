
### TDS Project -- Stability selection gPLS/sgPLS loading coefficients & selection proportion Visualisation
## Programme created by Vivian on 25 March

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

# Loading packages
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)

source("penalisation_functions.R")

##########################         Loading coefficients       ########################## 

##### plot labels--
plot_annot=read_csv("../Dictionaries/plot_annot.csv")
plot_annot=plot_annot[-c(1,2,21:25),] # Remove age, sex and BMI


##### Load output--

out_lung.g1=readRDS("../Results/PLS_319/gPLS_out_lung.1.rds")
out_lung.g2=readRDS("../Results/PLS_319/gPLS_out_lung.2.rds")
out_bladder.g1=readRDS("../Results/PLS_319/gPLS_out_bladder.1.rds")
out_bladder.g2=readRDS("../Results/PLS_319/gPLS_out_bladder.2.rds")

out_lung.sg1=readRDS("../Results/PLS_319/sgPLS_out_lung.1.rds")
out_lung.sg2=readRDS("../Results/PLS_319/sgPLS_out_lung.2.rds")
out_bladder.sg1=readRDS("../Results/PLS_319/sgPLS_out_bladder.1.rds")
out_bladder.sg2=readRDS("../Results/PLS_319/sgPLS_out_bladder.2.rds")


##### Load data--

lung.g1 <- readRDS("../Results/PLS_319/gPLS_load_lung.1.rds")
lung.g2 <- readRDS("../Results/PLS_319/gPLS_load_lung.2.rds")
bladder.g1 <- readRDS("../Results/PLS_319/gPLS_load_bladder.1.rds")
bladder.g2 <- readRDS("../Results/PLS_319/gPLS_load_bladder.2.rds")

lung.sg1 <- readRDS("../Results/PLS_319/sgPLS_load_lung.1.rds")
lung.sg2 <- readRDS("../Results/PLS_319/sgPLS_load_lung.2.rds")
bladder.sg1 <- readRDS("../Results/PLS_319/sgPLS_load_bladder.1.rds")
bladder.sg2 <- readRDS("../Results/PLS_319/sgPLS_load_bladder.2.rds")


##### Shrink non-selected loadings to zero--

lung.g1 <- ifelse(CalibratedStableRegression(out_lung.g1) == 1, lung.g1, 0)
lung.g2 <- ifelse(CalibratedStableRegression(out_lung.g2) == 1, lung.g2, 0)
bladder.g1 <- ifelse(CalibratedStableRegression(out_bladder.g1) == 1, bladder.g1, 0)
bladder.g2 <- ifelse(CalibratedStableRegression(out_bladder.g2) == 1, bladder.g2, 0)

lung.sg1 <- ifelse(CalibratedStableRegression(out_lung.sg1) == 1, lung.sg1, 0)
lung.sg2 <- ifelse(CalibratedStableRegression(out_lung.sg2) == 1, lung.sg2, 0)
bladder.sg1 <- ifelse(CalibratedStableRegression(out_bladder.sg1) == 1, bladder.sg1, 0)
bladder.sg2 <- ifelse(CalibratedStableRegression(out_bladder.sg2) == 1, bladder.sg2, 0)

##### Reverse loadings if necessary

lung.g1  <- lung.g1*(-1)
lung.g2  <- lung.g2*(-1)

lung.sg1  <- lung.sg1*(-1)
lung.sg2  <- lung.sg2*(-1)



##### Reorder rows and transform to vector with empty values in between--
foo = function(x1, x2, order){
  x=data.frame(x1)
  x[,2]=x2[match(rownames(x),names(x2))]
  x = slice(x, match(order,rownames(x)))
  x=cbind(rep(NA,nrow(x)),x[,1],
          rep(NA, nrow(x)),x[,2],
          rep(NA,nrow(x)))
  x=as.vector(t(x))
}

foo2 = function(x0, x1, x2, order){
  x=data.frame(x0)
  x[,2]=x1[match(rownames(x),names(x1))]
  x[,3]=x2[match(rownames(x),names(x2))]
  x = slice(x, match(order,rownames(x)))
  x=cbind(rep(NA,nrow(x)),x[,2],
          rep(NA, nrow(x)),x[,3],
          rep(NA,nrow(x)))
  x=as.vector(t(x))
}

colnames=names(lung.sg1)
myorder=c(colnames)

##### Extract loading coefficients--
load_lung_1 = foo(lung.g1, lung.sg1, myorder)
load_bladder_1 = foo(bladder.g1, bladder.sg1, myorder)

load_lung_2 = foo2(lung.g1, lung.g2, lung.sg2, myorder)
load_bladder_2 = foo2(bladder.g1, bladder.g2, bladder.sg2, myorder)




##### Settings
mylabels=plot_annot$label
myref=plot_annot$ref
variable_cat <- c(rep("Sociodemographic",18), 
                  rep("Health risk", 28),
                  rep("Environmental", 8),
                  rep("Medical", 16), 
                  rep("Biomarkers", 28))

models=c("gPLS", "sgPLS")
mycolours=darken(c("navy", "red"), amount=0.2)

n=2 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),length(myorder)*myspacing, by=myspacing)
background=TRUE


##### Plot
myupper <- max(load_lung_1, load_bladder_1, na.rm = T)
mylower <- min(load_lung_1, load_bladder_1, na.rm = T)

{pdf("gPLS_sgPLS_LC_base.pdf", width=11, height=6)
  par(oma=c(1, 5, 3, 1), mfrow=c(2,1),las=0)
  # Lung
  par(mar=c(5.5, 0, 0, 0),xpd=FALSE)
  plot(load_lung_1,
       xaxt="n", yaxt="n", ylim = c(mylower, myupper), xlim=c(1,length(load_lung_1)),
       type="n", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("darkturquoise",0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("darkturquoise",0.99), border=NA)
    }
    box()
  }
  par(new=TRUE)
  plot(load_lung_1, type="h", ylim = c(mylower, myupper),
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Average Loading Coefficients\n(Lung)", line=2, cex.lab=0.7)
  abline(h=0, lty=2)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  par(xpd=TRUE)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  
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
    if (is.na(myref)[k]){
      myadj=0.5
      mycex=0.5
      myline=5.2
    } else{
      myadj=0
      mycex=0.4
      myline=4.9
    }
    par(xpd=TRUE)
    mtext(side=1, mytmp, line=myline, at=xseq[k], adj=myadj, cex=mycex, las=2)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[!duplicated(myref)|is.na(myref)])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=5.3, labels=NA, cex=0.5)
    }
  }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      tmp=sub(" \\(","&(", mytext)
      split=strsplit(tmp, "&", perl=TRUE)
      mytext1=split[[1]][1]
      if (grepl("m\\^", mytext1)){
        mytext1=gsub("m\\^","'~m^", mytext1)
        mytext1=sub(")","~')", mytext1)
      }
      mytmp1=eval(parse(text=paste0("expression(","'", mytext1,"'",")")))
      mytmp2=split[[1]][2]
      mtext(mytmp1, side=1, at=mean(tmpseq[c(k,k+1)])-myspacing/2, line=5.9,  adj=1, cex=0.5,
            las=2)
      mtext(mytmp2, side=1, at=mean(tmpseq[c(k,k+1)])+myspacing/2, line=5.9,  adj=1, cex=0.4,
            las=2)
    }
  }
  legend("bottomright",inset=c(0,-0.35),lty=1, legend=models, col=mycolours, cex=0.6, bg="white")
  
  # bladder
  par(mar=c(0, 0, 5.5, 0),xpd=FALSE)
  plot(load_bladder_1,
       xaxt="n", yaxt="n", ylim = c(mylower, myupper), xlim=c(1,length(load_bladder_1)),
       type="n", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("hotpink",0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("hotpink",0.99), border=NA)
    }
    box()
  }
  
  par(new=TRUE)
  plot(load_bladder_1, type="h",
       xaxt="n", yaxt="n", ylim = c(mylower, myupper),
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Average Loading Coefficients\n(Bladder)", line=2, cex.lab=0.7)
  abline(h=0, lty=2)
  par(xpd=TRUE)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  
  dev.off()}



myupper <- max(load_lung_2, load_bladder_2, na.rm = T)
mylower <- min(load_lung_2, load_bladder_2, na.rm = T)

{pdf("gPLS_sgPLS_LC.pdf", width=11, height=6)
  par(oma=c(1, 5, 3, 1), mfrow=c(2,1),las=0)
  # Lung
  par(mar=c(5.5, 0, 0, 0),xpd=FALSE)
  plot(load_lung_2,
       xaxt="n", yaxt="n", ylim = c(mylower, myupper), xlim=c(1,length(load_lung_2)),
       type="n", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("darkturquoise",0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("darkturquoise",0.99), border=NA)
    }
    box()
  }
  par(new=TRUE)
  plot(load_lung_2, type="h", ylim = c(mylower, myupper),
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Average Loading Coefficients\n(Lung)", line=2, cex.lab=0.7)
  abline(h=0, lty=2)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  par(xpd=TRUE)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  
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
    if (is.na(myref)[k]){
      myadj=0.5
      mycex=0.5
      myline=5.2
    } else{
      myadj=0
      mycex=0.4
      myline=4.9
    }
    par(xpd=TRUE)
    mtext(side=1, mytmp, line=myline, at=xseq[k], adj=myadj, cex=mycex, las=2)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[!duplicated(myref)|is.na(myref)])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=5.3, labels=NA, cex=0.5)
    }
  }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      tmp=sub(" \\(","&(", mytext)
      split=strsplit(tmp, "&", perl=TRUE)
      mytext1=split[[1]][1]
      if (grepl("m\\^", mytext1)){
        mytext1=gsub("m\\^","'~m^", mytext1)
        mytext1=sub(")","~')", mytext1)
      }
      mytmp1=eval(parse(text=paste0("expression(","'", mytext1,"'",")")))
      mytmp2=split[[1]][2]
      mtext(mytmp1, side=1, at=mean(tmpseq[c(k,k+1)])-myspacing/2, line=5.9,  adj=1, cex=0.5,
            las=2)
      mtext(mytmp2, side=1, at=mean(tmpseq[c(k,k+1)])+myspacing/2, line=5.9,  adj=1, cex=0.4,
            las=2)
    }
  }
  legend("bottomright",inset=c(0,-0.35),lty=1, legend=models, col=mycolours, cex=0.6, bg="white")
  
  # bladder
  par(mar=c(0, 0, 5.5, 0),xpd=FALSE)
  plot(load_bladder_2,
       xaxt="n", yaxt="n", ylim = c(mylower, myupper), xlim=c(1,length(load_bladder_2)),
       type="n", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("hotpink",0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("hotpink",0.99), border=NA)
    }
    box()
  }
  
  par(new=TRUE)
  plot(load_bladder_2, type="h",
       xaxt="n", yaxt="n", ylim = c(mylower, myupper),
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Average Loading Coefficients\n(Bladder)", line=2, cex.lab=0.7)
  abline(h=0, lty=2)
  par(xpd=TRUE)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  
  dev.off()}






##########################        Selection proportions       ########################## 


#### Load selection proportion--

selprop_lung.g1=readRDS("../Results/PLS_319/gPLS_selprop_lung.1.rds")
selprop_lung.g2=readRDS("../Results/PLS_319/gPLS_selprop_lung.2.rds")
selprop_bladder.g1=readRDS("../Results/PLS_319/gPLS_selprop_bladder.1.rds")
selprop_bladder.g2=readRDS("../Results/PLS_319/gPLS_selprop_bladder.2.rds")

selprop_lung.sg1=readRDS("../Results/PLS_319/sgPLS_selprop_lung.1.rds")
selprop_lung.sg2=readRDS("../Results/PLS_319/sgPLS_selprop_lung.2.rds")
selprop_bladder.sg1=readRDS("../Results/PLS_319/sgPLS_selprop_bladder.1.rds")
selprop_bladder.sg2=readRDS("../Results/PLS_319/sgPLS_selprop_bladder.2.rds")


hat_params_lung_g1 <- GetArgmax(out_lung.g1)
hat_params_lung_g2 <- GetArgmax(out_lung.g2)
hat_params_lung_sg1 <- GetArgmax(out_lung.sg1)
hat_params_lung_sg2 <- GetArgmax(out_lung.sg2)


hat_params_bladder_g1 <- GetArgmax(out_bladder.g1)
hat_params_bladder_g2 <- GetArgmax(out_bladder.g2)
hat_params_bladder_sg1 <- GetArgmax(out_bladder.sg1)
hat_params_bladder_sg2 <- GetArgmax(out_bladder.sg2)



##### Combine two models--
selprop_lung_1 = foo(selprop_lung.g1, selprop_lung.sg1, myorder)
selprop_bladder_1 = foo(selprop_bladder.g1, selprop_bladder.sg1, myorder)

selprop_lung_2 = foo2(selprop_lung.g1, selprop_lung.g2, selprop_lung.sg2, myorder)
selprop_bladder_2 = foo2(selprop_lung.g1, selprop_bladder.g2, selprop_bladder.sg2, myorder)



##### Settings
mylabels=plot_annot$label
myref=plot_annot$ref
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 28),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
models=c("gPLS", "sgPLS")
mycolours=c("navy","red")
n=2 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),length(myorder)*myspacing, by=myspacing)
background=TRUE
myrange=c(0,1)


##### Plots
pi_lung=c(hat_params_lung_g1[2],hat_params_lung_sg1[2]) 
pi_bladder=c(hat_params_lung_g1[2],hat_params_bladder_sg1[2])

{pdf("gPLS_sgPLS_selprop_base.pdf", width=11, height=6)
  par(oma=c(1, 5, 3, 1), mfrow=c(2,1),las=0)
  # Lung
  par(mar=c(5.5, 0, 0, 0),xpd=FALSE)
  plot(selprop_lung_1,
       xaxt="n", yaxt="n", ylim = myrange, xlim=c(1,length(selprop_lung_1)),
       type="n", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("darkturquoise",0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("darkturquoise",0.99), border=NA)
    }
    box()
  }
  
  par(new=TRUE)
  plot(selprop_lung_1, type="h", ylim = myrange,
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  abline(h=pi_lung, lty=2, lwd=0.5, col=mycolours)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Selection Proportion\n(Lung)", line=2, cex.lab=0.7)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  par(xpd=TRUE)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  
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
    if (is.na(myref)[k]){
      myadj=0.5
      mycex=0.5
      myline=5.2
    } else{
      myadj=0
      mycex=0.4
      myline=4.9
    }
    par(xpd=TRUE)
    mtext(side=1, mytmp, line=myline, at=xseq[k], adj=myadj, cex=mycex, las=2)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[!duplicated(myref)|is.na(myref)])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=5.3, labels=NA, cex=0.5)
    }
  }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      tmp=sub(" \\(","&(", mytext)
      split=strsplit(tmp, "&", perl=TRUE)
      mytext1=split[[1]][1]
      if (grepl("m\\^", mytext1)){
        mytext1=gsub("m\\^","'~m^", mytext1)
        mytext1=sub(")","~')", mytext1)
      }
      mytmp1=eval(parse(text=paste0("expression(","'", mytext1,"'",")")))
      mytmp2=split[[1]][2]
      mtext(mytmp1, side=1, at=mean(tmpseq[c(k,k+1)])-myspacing/2, line=5.9,  adj=1, cex=0.5,
            las=2)
      mtext(mytmp2, side=1, at=mean(tmpseq[c(k,k+1)])+myspacing/2, line=5.9,  adj=1, cex=0.4,
            las=2)
    }
  }
  legend("bottomright",inset=c(0,-0.35),lty=1, legend=models, col=mycolours, cex=0.6, bg="white")
  
  # bladder
  par(mar=c(0, 0, 5.5, 0),xpd=FALSE)
  plot(selprop_bladder_1,
       xaxt="n", yaxt="n", ylim = rev(myrange), xlim=c(1,length(selprop_bladder_1)),
       type="n", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("hotpink",0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("hotpink",0.99), border=NA)
    }
    box()
  }
  
  par(new=TRUE)
  plot(selprop_bladder_1, type="h",
       xaxt="n", yaxt="n", ylim = rev(myrange),
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  abline(h=pi_bladder, lty=2, lwd=0.5, col=mycolours)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Selection Proportion\n(Bladder)", line=2, cex.lab=0.7)
  par(xpd=TRUE)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  
  dev.off()}



pi_lung=c(hat_params_lung_g2[2],hat_params_lung_sg2[2]) 
pi_bladder=c(hat_params_lung_g2[2],hat_params_bladder_sg2[2])

{pdf("gPLS_sgPLS_selprop_adjusted.pdf", width=11, height=6)
  par(oma=c(1, 5, 3, 1), mfrow=c(2,1),las=0)
  # Lung
  par(mar=c(5.5, 0, 0, 0),xpd=FALSE)
  plot(selprop_lung_2,
       xaxt="n", yaxt="n", ylim = myrange, xlim=c(1,length(selprop_lung_2)),
       type="n", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("darkturquoise",0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("darkturquoise",0.99), border=NA)
    }
    box()
  }
  
  par(new=TRUE)
  plot(selprop_lung_2, type="h", ylim = myrange,
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  abline(h=pi_lung, lty=2, lwd=0.5, col=mycolours)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Selection Proportion\n(Lung)", line=2, cex.lab=0.7)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  par(xpd=TRUE)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  
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
    if (is.na(myref)[k]){
      myadj=0.5
      mycex=0.5
      myline=5.2
    } else{
      myadj=0
      mycex=0.4
      myline=4.9
    }
    par(xpd=TRUE)
    mtext(side=1, mytmp, line=myline, at=xseq[k], adj=myadj, cex=mycex, las=2)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[!duplicated(myref)|is.na(myref)])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=5.3, labels=NA, cex=0.5)
    }
  }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      tmp=sub(" \\(","&(", mytext)
      split=strsplit(tmp, "&", perl=TRUE)
      mytext1=split[[1]][1]
      if (grepl("m\\^", mytext1)){
        mytext1=gsub("m\\^","'~m^", mytext1)
        mytext1=sub(")","~')", mytext1)
      }
      mytmp1=eval(parse(text=paste0("expression(","'", mytext1,"'",")")))
      mytmp2=split[[1]][2]
      mtext(mytmp1, side=1, at=mean(tmpseq[c(k,k+1)])-myspacing/2, line=5.9,  adj=1, cex=0.5,
            las=2)
      mtext(mytmp2, side=1, at=mean(tmpseq[c(k,k+1)])+myspacing/2, line=5.9,  adj=1, cex=0.4,
            las=2)
    }
  }
  legend("bottomright",inset=c(0,-0.35),lty=1, legend=models, col=mycolours, cex=0.6, bg="white")
  
  # bladder
  par(mar=c(0, 0, 5.5, 0),xpd=FALSE)
  plot(selprop_bladder_2,
       xaxt="n", yaxt="n", ylim = rev(myrange), xlim=c(1,length(selprop_bladder_2)),
       type="n", lwd=1, col=c(NA, mycolours[1], NA, mycolours[2], NA),
       ylab="", xlab="")
  
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("hotpink",0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten("hotpink",0.99), border=NA)
    }
    box()
  }
  
  par(new=TRUE)
  plot(selprop_bladder_2, type="h",
       xaxt="n", yaxt="n", ylim = rev(myrange),
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  abline(h=pi_bladder, lty=2, lwd=0.5, col=mycolours)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Selection Proportion\n(Bladder)", line=2, cex.lab=0.7)
  par(xpd=TRUE)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  
  dev.off()}
