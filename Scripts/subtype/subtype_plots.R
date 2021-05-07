### TDS Project -- Stability selection LASSO & sPLS Visualisation: By lung cancer site
## Programme created by Rin on 21 March, edited by Ines March 24

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

##### Loading packages--
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(ggrepel)
library(ggpubr)
source("penalisation_functions.R")

### Plot labels----
plot_annot=read_csv("../Dictionaries/plot_annot.csv")
plot_annot=plot_annot[-c(1,2,21:25),] # Remove age, sex and BMI


### Load outputs ----
for (m in 1:4){
  arr=paste0(rep(c("mal.lower","mal.upper"),each=2),".",1:2)[m]
  
  ### Lasso
  lasso_out=readRDS(paste0("../Results/strat_site_lasso/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_hat_params=GetArgmax(lasso_out) # Extract calibrated pi
  assign(paste0("lasso_hat_params_",arr),lasso_hat_params) # Assign name
  lasso_selprop=readRDS(paste0("../Results/strat_site_lasso/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("lasso_selprop_",arr),lasso_selprop) # Assign name
  lasso_beta=readRDS(paste0("../Results/strat_site_lasso/average_beta_",arr,".rds")) # Load beta
  lasso_beta=ifelse(CalibratedStableRegression(lasso_out) == 1, lasso_beta, 0) # Shrink non-selected beta to zero
  lasso_beta=exp(lasso_beta) # Exponentiate to OR
  assign(paste0("lasso_beta_",arr),lasso_beta) # Assign name
  
  ### sPLS
  spls_out=readRDS(paste0("../Results/strat_site_spls/out_",arr,".rds")) # Load output
  assign(paste0("spls_out_",arr),spls_out) # Assign name
  spls_hat_params=GetArgmax(spls_out) # Extract calibrated pi
  assign(paste0("spls_hat_params_",arr),spls_hat_params) # Assign name
  spls_selprop=readRDS(paste0("../Results/strat_site_spls/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("spls_selprop_",arr),spls_selprop) # Assign name
  spls_beta=readRDS(paste0("../Results/strat_site_spls/beta_",arr,".rds")) # Load beta
  spls_beta=ifelse(CalibratedStableRegression(spls_out) == 1, spls_beta, 0) # Shrink non-selected beta to zero
  assign(paste0("spls_beta_",arr),spls_beta) # Assign name
}

# Reorder rows and transform to vector with empty values in between
foo = function(x1, x2, order){
  x=data.frame(x1)
  x[,2]=x2[match(rownames(x),names(x2))]
  x = slice(x, match(order,rownames(x)))
  x=cbind(rep(NA,nrow(x)),x[,1],
          rep(NA, nrow(x)),x[,2],
          rep(NA,nrow(x)))
  x=as.vector(t(x))
}
# Reorder rows and transform to dataframe for ggplot
foo2 = function(l1, l2, s1, s2, order){
  x=data.frame(l1)
  x[,2]=l2[match(rownames(x),names(l2))]
  x[,3]=s1[match(rownames(x),names(s1))]
  x[,4]=s2[match(rownames(x),names(s2))]
  x = slice(x, match(order,rownames(x)))
  df=data.frame(or=unlist(x[,1:2]),load=unlist(x[,3:4]),
                var=rep(rownames(x),2),
                model=rep(c("Base model","Model adjusted for smoking status"),each=nrow(x)))
}

# for each model

# malignant tumor in lower lobe
colnames=names(lasso_selprop_mal.lower.1)
myorder=c(colnames[5:38],colnames[1:4],colnames[39:length(colnames)])
lasso_selprop_mal.lower=foo(lasso_selprop_mal.lower.1,
                         lasso_selprop_mal.lower.2,
                         myorder)
spls_selprop_mal.lower=foo(spls_selprop_mal.lower.1,
                        spls_selprop_mal.lower.2,
                        myorder)

beta_mal.lower=foo2(lasso_beta_mal.lower.1, lasso_beta_mal.lower.2,
                 spls_beta_mal.lower.1,spls_beta_mal.lower.2,myorder)

# malignant tumor in upper lobe
lasso_selprop_mal.upper=foo(lasso_selprop_mal.upper.1,
                         lasso_selprop_mal.upper.2,
                         myorder)
spls_selprop_mal.upper=foo(spls_selprop_mal.upper.1,
                        spls_selprop_mal.upper.2,
                        myorder)
beta_mal.upper=foo2(lasso_beta_mal.upper.1, lasso_beta_mal.upper.2,
                 spls_beta_mal.upper.1,spls_beta_mal.upper.2,myorder)

# malignant tumor in bronchus or lung
lasso_selprop_mal.lung=foo(lasso_selprop_mal.lung.1,
                            lasso_selprop_mal.lung.2,
                            myorder)

spls_selprop_mal.lung=foo(spls_selprop_mal.lung.1,
                           spls_selprop_mal.lung.2,
                           myorder)

beta_mal.lung=foo2(lasso_beta_mal.lung.1, lasso_beta_mal.lung.2,
                    spls_beta_mal.lung.1, spls_beta_mal.lung.2, myorder)
                  
### Selection proportion plots----

colnames=names(lasso_beta_mal.lower.1)
myorder=c(colnames[5:46],colnames[1:4],colnames[47:length(colnames)])

beta_lower.1 = c(lasso_beta_mal.lower.1[5:46],lasso_beta_mal.lower.1[1:4],lasso_beta_mal.lower.1[47:length(lasso_beta_mal.lower.1)])
beta_upper.1 = c(lasso_beta_mal.upper.1[5:46],lasso_beta_mal.upper.1[1:4],lasso_beta_mal.upper.1[47:length(lasso_beta_mal.upper.1)])

cbind
## Glabal settings
mylabels=plot_annot$label
myref=plot_annot$ref
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 20),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
models=c("Base model", "Model adjusted on smoking status")
mycolours=c("navy","red")
n=2 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),length(myorder)*myspacing, by=myspacing)
background=TRUE
myrange=c(0,1)

betas_lower.1 <- as.data.frame(lasso_beta_mal.lower.1)
betas_upper.1<-as.data.frame(lasso_beta_mal.upper.1)
betas.1<-data.frame(betas_lower.1, betas_upper.1, variable_cat)

betas_lower.2 <- as.data.frame(lasso_beta_mal.lower.2)
betas_upper.2<-as.data.frame(lasso_beta_mal.upper.2)
betas.2<-data.frame(betas_lower.2, betas_upper.2, variable_cat_86)


# 
# rownames(forest)
# forest$`Variable group` <- NA
# forest$`Variable group`[1:22] <- 'Sociodemographic'
# forest$`Variable group`[23:39] <- 'Health risk'
# forest$`Variable group`[40:46] <- 'Environmental'
# forest$`Variable group`[47:62] <- 'Medical'
# forest$`Variable group`[63:90] <- 'Biomarkers'

## Tumor in Lower Lobe
# pi_spls=c(spls_hat_params_mal.lower.1[2],spls_hat_params_mal.lower.2[2]) # sPLS Selection proportion threshold
# pi_lasso=c(lasso_hat_params_mal.lower.1[2],lasso_hat_params_mal.lower.2[2]) # LASSO Selection proportion threshold
# file_path="mal.lower"
# spls=spls_selprop_mal.lower
# lasso=lasso_selprop_mal.lower
# background_colour="royalblue" # For lower lobe
# 
# mylabels=plot_annot$label
# myref=plot_annot$ref
variable_cat_86=c(rep("Sociodemographic",18),
               rep("Health risk", 16),
               rep("Environmental", 8),
               rep("Medical", 16),
               rep("Biomarkers", 28))
# models=c("Lung Cancer", "Bladder Cancer")
# mycolours=darken(c("darkturquoise","hotpink"), amount=0.3)
# n=2 # Number of lines per variable
# myspacing=n*2+1
# xseq=seq((n+1),length(myorder)*myspacing, by=myspacing)
# background=TRUE
# myrange=c(0,1)

### OR scatter plots-----
numberlabels1 <- 1:90
numberlabels1 <- as.character(numberlabels1)

numberlabels2 <- 1:86
numberlabels2 <- as.character(numberlabels2)

plot1 <- ggplot(betas.1, aes(x=lasso_beta_mal.lower.1, y=lasso_beta_mal.upper.1, color = variable_cat)) + 
  geom_point(size=2) + geom_text_repel(label=numberlabels1) + 
  geom_abline(linetype = 'dashed', colour = 'grey') +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'grey') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'grey') +
  ylab('Upper lobe ORs') + xlab('Lower lobe ORs') +
  ggtitle('Base model') + theme_bw()
p5 <- plot1 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))


plot2 <- ggplot(betas.2, aes(x=lasso_beta_mal.lower.2, y=lasso_beta_mal.upper.2, color = variable_cat_86)) + 
  geom_point(size=2) + geom_text_repel(label=numberlabels2) + 
  geom_abline(linetype = 'dashed', colour = 'grey') +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'grey') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'grey') +
  ylab('Upper lobe ORs') + xlab('Lower lobe ORs') +
  ggtitle('Adjusted model') + theme_bw()
p6 <- plot2 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

p7 <- ggarrange(p5, p6, common.legend = T, legend = 'right', nrow = 1)

pdf("../Figures/Presentation/or_sites.pdf", height = 5, width = 12)
p7
dev.off()

{pdf("../Figures/selprop_mal.lower.pdf", height = 6, width = 11)
  par(oma=c(1, 5, 5, 1), mfrow=c(2,1),las=0)
  # sPLS
  par(mar=c(5.5, 0, 0, 0),xpd=FALSE)
  plot(spls,ylim=myrange, xlim=c(1,length(spls)), type="n",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="",xlab="")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten(background_colour,0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten(background_colour,0.99), border=NA)
    }
    box()
  }
  par(new=TRUE)
  plot(spls, ylim=myrange, type="h",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  abline(h=pi_spls, lty=4, lwd=0.5, col=mycolours)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Selection Proportion\n(sPLS)", line=2, cex.lab=0.7)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  mtext(side=3, text="Lower Lobe, Bronchus, or Lung: 160 Cases\n\n\n", cex.lab=0.7)
  par(xpd=TRUE)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey42")
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
      myline=5.5
    } else{
      myadj=0
      mycex=0.4
      myline=5.2
    }
    par(xpd=TRUE)
    mtext(side=1, mytmp, line=myline, at=xseq[k], adj=myadj, cex=mycex, las=2)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=5.5, labels=NA, tck=-0.005)
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
      mtext(mytmp1, side=1, at=mean(tmpseq[c(k,k+1)])-myspacing/2, line=5.8,  adj=1, cex=0.5,
            las=2)
      mtext(mytmp2, side=1, at=mean(tmpseq[c(k,k+1)])+myspacing/2, line=5.8,  adj=1, cex=0.4,
            las=2)
    }
  }
  legend("bottomright",inset=c(0,-0.27),lty=1, legend=models, col=mycolours, cex=0.6, bg="white")
  # LASSO
  par(mar=c(0, 0, 5.5, 0),xpd=FALSE)
  plot(lasso, ylim=myrange, xlim=c(1,length(lasso)), type="n",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="",xlab="")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten(background_colour,0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten(background_colour,0.99), border=NA)
    }
    box()
  }
  par(new=TRUE)
  plot(lasso, ylim=rev(myrange), type="h",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  abline(h=pi_spls, lty=4, lwd=0.5, col=mycolours)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Selection Proportion\n(LASSO)", line=2, cex.lab=0.7)
  par(xpd=TRUE)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  dev.off()
} # Run this to make plot

# Malignant tumor in lung or bronchus
pi_spls=c(spls_hat_params_mal.lung.1[2],spls_hat_params_mal.lung.2[2]) # sPLS Selection proportion threshold
pi_lasso=c(lasso_hat_params_mal.lung.1[2],lasso_hat_params_mal.lung.2[2]) # LASSO Selection proportion threshold
file_path="mal.lung"
spls=spls_selprop_mal.lung
lasso=lasso_selprop_mal.lung
background_colour="firebrick" # For Malignant lung
{pdf(paste0("../Figures/selprop_",file_path,".pdf"), height = 6, width = 11)
  par(oma=c(1, 5, 5, 1), mfrow=c(2,1),las=0)
  # sPLS
  par(mar=c(5.5, 0, 0, 0),xpd=FALSE)
  plot(spls,ylim=myrange, xlim=c(1,length(spls)), type="n",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="",xlab="")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten(background_colour,0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten(background_colour,0.99), border=NA)
    }
    box()
  }
  par(new=TRUE)
  plot(spls, ylim=myrange, type="h",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  abline(h=pi_spls, lty=4, lwd=0.5, col=mycolours)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Selection Proportion\n(sPLS)", line=2, cex.lab=0.7)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  mtext(side=3, text="Bronchus or Lung, unspecified: 281 Cases\n\n\n", cex.lab=0.7)
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
      myline=5.5
    } else{
      myadj=0
      mycex=0.4
      myline=5.2
    }
    par(xpd=TRUE)
    mtext(side=1, mytmp, line=myline, at=xseq[k], adj=myadj, cex=mycex, las=2)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=5.5, labels=NA, tck=-0.005)
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
      mtext(mytmp1, side=1, at=mean(tmpseq[c(k,k+1)])-myspacing/2, line=5.8,  adj=1, cex=0.5,
            las=2)
      mtext(mytmp2, side=1, at=mean(tmpseq[c(k,k+1)])+myspacing/2, line=5.8,  adj=1, cex=0.4,
            las=2)
    }
  }
  legend("bottomright",inset=c(0,-0.27),lty=1, legend=models, col=mycolours, cex=0.6, bg="white")
  # LASSO
  par(mar=c(0, 0, 5.5, 0),xpd=FALSE)
  plot(lasso, ylim=myrange, xlim=c(1,length(lasso)), type="n",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="",xlab="")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten(background_colour,0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten(background_colour,0.99), border=NA)
    }
    box()
  }
  par(new=TRUE)
  plot(lasso, ylim=rev(myrange), type="h",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  abline(h=pi_spls, lty=4, lwd=0.5, col=mycolours)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Selection Proportion\n(LASSO)", line=2, cex.lab=0.7)
  par(xpd=TRUE)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  dev.off()
} # Run this to make plot


## Malignant upper
pi_spls=c(spls_hat_params_mal.upper.1[2],spls_hat_params_mal.upper.2[2]) # sPLS Selection proportion threshold
pi_lasso=c(lasso_hat_params_mal.upper.1[2],lasso_hat_params_mal.upper.2[2]) # LASSO Selection proportion threshold
file_path="mal.upper"
spls=spls_selprop_mal.upper
lasso=lasso_selprop_mal.upper
background_colour="darkgreen" # For upper lobe
{pdf(paste0("../Figures/selprop_",file_path,".pdf"), height = 6, width = 11)
  par(oma=c(1, 5, 5, 1), mfrow=c(2,1),las=0)
  # sPLS
  par(mar=c(5.5, 0, 0, 0),xpd=FALSE)
  plot(spls,ylim=myrange, xlim=c(1,length(spls)), type="n",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="",xlab="")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten(background_colour,0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten(background_colour,0.99), border=NA)
    }
    box()
  }
  par(new=TRUE)
  plot(spls, ylim=myrange, type="h",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  abline(h=pi_spls, lty=4, lwd=0.5, col=mycolours)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Selection Proportion\n(sPLS)", line=2, cex.lab=0.7)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  mtext(side=3, text="Upper Lobe, Bronchus, or Lung: 301 Cases\n\n\n", cex.lab=0.7)
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
      myline=5.5
    } else{
      myadj=0
      mycex=0.4
      myline=5.2
    }
    par(xpd=TRUE)
    mtext(side=1, mytmp, line=myline, at=xseq[k], adj=myadj, cex=mycex, las=2)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=5.5, labels=NA, tck=-0.005)
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
      mtext(mytmp1, side=1, at=mean(tmpseq[c(k,k+1)])-myspacing/2, line=5.8,  adj=1, cex=0.5,
            las=2)
      mtext(mytmp2, side=1, at=mean(tmpseq[c(k,k+1)])+myspacing/2, line=5.8,  adj=1, cex=0.4,
            las=2)
    }
  }
  legend("bottomright",inset=c(0,-0.27),lty=1, legend=models, col=mycolours, cex=0.6, bg="white")
  # LASSO
  par(mar=c(0, 0, 5.5, 0),xpd=FALSE)
  plot(lasso, ylim=myrange, xlim=c(1,length(lasso)), type="n",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="",xlab="")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten(background_colour,0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=c(-10,-10,10,10), col=lighten(background_colour,0.99), border=NA)
    }
    box()
  }
  par(new=TRUE)
  plot(lasso, ylim=rev(myrange), type="h",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  abline(h=pi_spls, lty=4, lwd=0.5, col=mycolours)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Selection Proportion\n(LASSO)", line=2, cex.lab=0.7)
  par(xpd=TRUE)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  dev.off()
} # Run this to make plot



### Beta plot----
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 20),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
mycolours=c("grey50","forestgreen" ,"tomato","royalblue","gold")

# lower mal
xlim=c(0.25,1.75)
ylim=c(-0.25,0.75)
beta_mal.lower$var_cat=rep(variable_cat,2)
beta_mal.lower$label=rep(1:(nrow(beta_mal.lower)/2), 2)
beta_mal.lower$mycolour_point=c(lighten(rep(mycolours,times=c(18,20,8,16,28)),0.2),rep(mycolours,times=c(18,20,8,16,28)))
beta_mal.lower$mycolour_lab=darken(beta_mal.lower$mycolour_point, amount=0.5)
p1=ggplot(beta_mal.lower, aes(or, load,
                           label=ifelse(((abs(or-1)<0.1&load==0)|(or==1&abs(load)<0.1)),
                                        "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(aes(shape=model),colour=beta_mal.lower$mycolour_point) +
  scale_shape_manual(values=c(17, 19)) +
  geom_text_repel(color=beta_mal.lower$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) +
  xlab("Odds Ratio (LASSO)")+
  ylab("Loading Coefficient (sPLS)") +
  ggtitle("Lower lobe, bronchus or lung: 160 cases")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

# Lung or Bronchus, unspecified
xlim=c(0.5,1.5)
ylim=c(-0.25,0.55)
beta_mal.lung$var_cat=rep(variable_cat,2)
beta_mal.lung$label=rep(1:(nrow(beta_mal.lung)/2), 2)
beta_mal.lung$mycolour_point=c(lighten(rep(mycolours,times=c(18,20,8,16,28)),0.2),rep(mycolours,times=c(18,20,8,16,28)))
beta_mal.lung$mycolour_lab=darken(beta_mal.lung$mycolour_point, amount=0.5)
p2=ggplot(beta_mal.lung, aes(or, load, label=ifelse(((abs(or-1)<0.1&load==0)|(or==1&abs(load)<0.1)),
                                                     "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(aes(shape=model),colour=beta_mal.lung$mycolour_point) +
  scale_shape_manual(values=c(17, 19)) +
  geom_text_repel(color=beta_mal.lung$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) +
  xlab("Odds Ratio (LASSO)")+
  ylab("Loading Coefficient (sPLS)") +
  ggtitle("Bronchus or lung, unspecified: 281 cases")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

## Male
# Lung
xlim=c(0.25,1.75)
ylim=c(-0.25,0.75)
beta_mal.upper$var_cat=rep(variable_cat,2)
beta_mal.upper$label=rep(1:(nrow(beta_mal.upper)/2), 2)
beta_mal.upper$mycolour_point=c(lighten(rep(mycolours,times=c(18,20,8,16,28)),0.2),rep(mycolours,times=c(18,20,8,16,28)))
beta_mal.upper$mycolour_lab=darken(beta_mal.upper$mycolour_point, amount=0.5)
p3=ggplot(beta_mal.upper, aes(or, load,
                           label=ifelse(((abs(or-1)<0.1&load==0)|(or==1&abs(load)<0.1)),
                                        "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(aes(shape=model),colour=beta_mal.upper$mycolour_point) +
  scale_shape_manual(values=c(17, 19)) +
  geom_text_repel(color=beta_mal.upper$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) +
  xlab("Odds Ratio (LASSO)")+
  ylab("Loading Coefficient (sPLS)") +
  ggtitle("Upper lobe, bronchus or lung: 301 cases")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")


library(cowplot)
pdf("../Figures/or_load_grid_sites_report.pdf", width=10, height=10)
plot_grid(p1, p3, p2, nrow = 2, labels = c('A', 'B','C'))
# extract the legend from one of the plots
dev.off()

pdf("../Figures/or_load_grid_sites_pres.pdf", width=20, height=5)
plot_grid(p1, p3, p2, nrow = 1)
# extract the legend from one of the plots
dev.off()


