### TDS Project -- Stability selection LASSO & sPLS Visualisation
## Programme created by Rin on 21 March, update by Ines for NVSM on March 25

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

### Loading packages----
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(ggrepel)

source("penalisation_functions.R")

### Plot labels----
plot_annot=read_csv("../Dictionaries/nvsm_annot.csv")
plot_annot=plot_annot[-c(1,2,21:25),] # Remove age, sex and BMI & smoking categories
plot_annot=plot_annot

### Load outputs ----
for (m in 1:2){
  arr=c("lung_base_denoised","bladder_base_denoised")[m]
  
  ### Lasso
  lasso_out=readRDS(paste0("../Results/strat_nvsm_lasso/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_hat_params=GetArgmax(lasso_out) # Extract calibrated pi
  assign(paste0("lasso_hat_params_",arr),lasso_hat_params) # Assign name
  lasso_selprop=readRDS(paste0("../Results/strat_nvsm_lasso/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("lasso_selprop_",arr),lasso_selprop) # Assign name
  lasso_beta=readRDS(paste0("../Results/strat_nvsm_lasso/average_beta_",arr,".rds")) # Load beta
  lasso_beta=ifelse(CalibratedStableRegression(lasso_out) == 1, lasso_beta, 0) # Shrink non-selected beta to zero
  lasso_beta=exp(lasso_beta) # Exponentiate to OR
  assign(paste0("lasso_beta_",arr),lasso_beta) # Assign name
  
  ### sPLS
  spls_out=readRDS(paste0("../Results/nvsm_spls/out_",arr,".rds")) # Load output
  assign(paste0("spls_out_",arr),spls_out) # Assign name
  spls_hat_params=GetArgmax(spls_out) # Extract calibrated pi
  assign(paste0("spls_hat_params_",arr),spls_hat_params) # Assign name
  spls_selprop=readRDS(paste0("../Results/nvsm_spls/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("spls_selprop_",arr),spls_selprop) # Assign name
  spls_beta=readRDS(paste0("../Results/nvsm_spls/beta_",arr,".rds")) # Load beta
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
                model=rep(c("Lung Cancer","Bladder Cancer"),each=nrow(x)))
}
colnames=names(lasso_selprop_lung_base_denoised)
#myorder=c(colnames[5:46],colnames[1:4],colnames[47:length(colnames)])
myorder=colnames

# gather data
lasso_selprop_nvsm=foo(lasso_selprop_lung_base_denoised,
                         lasso_selprop_bladder_base_denoised,
                         myorder)
spls_selprop_nvsm=foo(spls_selprop_lung_base_denoised,
                        spls_selprop_bladder_base_denoised,
                        myorder)
beta_nvsm=foo2(lasso_beta_lung_base_denoised, lasso_beta_bladder_base_denoised,
                 spls_beta_lung_base_denoised,spls_beta_bladder_base_denoised,myorder)


### Selection proportion plots----
## Glabal settings
mylabels=plot_annot$label
myref=plot_annot$ref
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 17),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
models=c("Lung Cancer", "Bladder Cancer")
mycolours=darken(c("darkturquoise","hotpink"), amount=0.3)
n=2 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),length(myorder)*myspacing, by=myspacing)
background=TRUE
myrange=c(0,1)

## nvsm
pi_spls=c(spls_hat_params_lung_base_denoised[2],spls_hat_params_bladder_base_denoised[2]) # sPLS Selection proportion threshold
pi_lasso=c(lasso_hat_params_lung_base_denoised[2],lasso_hat_params_bladder_base_denoised[2]) # LASSO Selection proportion threshold
file_path="nvsm"
spls=spls_selprop_nvsm
lasso=lasso_selprop_nvsm
background_colour="darkturquoise" # For lung
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
  mtext(side=3, text="Never Smokers\n\n\n", cex.lab=0.7)
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
    if (is.na(myref[k])){
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
               rep("Health risk", 17),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
mycolours=c("grey50","forestgreen" ,"tomato","royalblue","orange")
## Lung
xlim=c(0.25,1.75)
ylim=c(-.25,.25)
beta_nvsm$var_cat=rep(variable_cat,2)
beta_nvsm$label=rep(plot_annot$label.point, 2)
beta_nvsm$mycolour_point=c(lighten(rep(mycolours,times=c(18,17,8,16,28)),0.2),rep(mycolours,times=c(18,17,8,16,28)))
beta_nvsm$mycolour_lab=darken(beta_nvsm$mycolour_point, amount=0.5)
p <- ggplot(beta_nvsm, aes(or, load,color=var_cat,
                           label=ifelse(((abs(or-1)<0.1&load==0)|(or==1&abs(load)<0.1)),
                                        "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(aes(shape=model)) +
  scale_shape_manual(values=c(17, 19)) +
  geom_text_repel(color=beta_nvsm$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) +
  xlab("Odds Ratio (LASSO)")+
  ylab("Loading Coefficient (sPLS)") +
  ggtitle("Never Smokers")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw()

pdf("../Figures/or_nvsm.pdf", width=10, height=10)
p
dev.off()



