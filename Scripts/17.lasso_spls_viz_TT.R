
### TDS Project -- Stability selection LASSO & sPLS Visualisation 
## Programme created by Vivian on 20 March 
# sPLS with splitting data into train/test

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

##### Loading packages--
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(cowplot)
library(ggrepel)

source("penalisation_functions.R")

##### Plot labels--
plot_annot=read_csv("../Dictionaries/plot_annot.csv")
plot_annot=plot_annot[-c(1,2,21:25),] # Remove age, sex and BMI


##### Load output--
out_lung.s1=readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_out_lung_TT.1.rds")
out_lung.s2=readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_out_lung_TT.2.rds")
out_bladder.s1=readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_out_bladder_TT.1.rds")
out_bladder.s2=readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_out_bladder_TT.2.rds")

out_lung.1=readRDS("../Results/lasso/out_lung.1.rds")
out_lung.2=readRDS("../Results/lasso/out_lung.2.rds")
out_bladder.1=readRDS("../Results/lasso/out_bladder.1.rds")
out_bladder.2=readRDS("../Results/lasso/out_bladder.2.rds")

##### Load loadings--
load_lung.1 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_load_lung_TT.1.rds")
load_lung.2 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_load_lung_TT.2.rds")
load_bladder.1 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_load_bladder_TT.1.rds")
load_bladder.2 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_load_bladder_TT.2.rds")

##### Shrink non-selected loadings to zero--
load_lung.1=ifelse(CalibratedStableRegression(out_lung.s1) == 1, load_lung.1, 0)
load_lung.2=ifelse(CalibratedStableRegression(out_lung.s2) == 1, load_lung.2, 0)
load_bladder.1=ifelse(CalibratedStableRegression(out_bladder.s1) == 1, load_bladder.1, 0)
load_bladder.2=ifelse(CalibratedStableRegression(out_bladder.s2) == 1, load_bladder.2, 0)

##### reverse loadings if necessary
load_lung.1 <- load_lung.1*(-1)
load_lung.2 <- load_lung.2*(-1)


##### Load betas--
beta_lung.1=readRDS("../Results/lasso/average_beta_lung.1.rds")
beta_lung.2=readRDS("../Results/lasso/average_beta_lung.2.rds")
beta_bladder.1=readRDS("../Results/lasso/average_beta_bladder.1.rds")
beta_bladder.2=readRDS("../Results/lasso/average_beta_bladder.2.rds")

##### Shrink non-selected beta to zero--
beta_lung.1=ifelse(CalibratedStableRegression(out_lung.1) == 1, beta_lung.1, 0)
beta_lung.2=ifelse(CalibratedStableRegression(out_lung.2) == 1, beta_lung.2, 0)
beta_bladder.1=ifelse(CalibratedStableRegression(out_bladder.1) == 1, beta_bladder.1, 0)
beta_bladder.2=ifelse(CalibratedStableRegression(out_bladder.2) == 1, beta_bladder.2, 0)

##### conver to OR
or_lung.1 <- exp(beta_lung.1)
or_lung.2 <- exp(beta_lung.2)
or_bladder.1 <- exp(beta_bladder.1)
or_bladder.2 <- exp(beta_bladder.2)

  
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

##### Reorder rows and transform to vector with empty values in between--

re_names.1 <- names(beta_lung.1)
names(load_lung.1) <- re_names.1[c(5:46,1:4,47:98)]
names(load_bladder.1) <- re_names.1[c(5:46,1:4,47:98)]
re_names.2 <- names(beta_lung.2)
names(load_lung.2) <- re_names.2
names(load_bladder.2) <- re_names.2

colnames=names(load_lung.1)
myorder=c(colnames)


foo3 = function(l1, l2, s1, s2, order){
  x=data.frame(s1)
  x[,2] <- s2[match(rownames(x),names(s2))]
  x[,3] <- l1[match(rownames(x),names(l1))]
  x[,4] <- l2[match(rownames(x),names(l2))]
  x = slice(x, match(order,rownames(x)))
  df=data.frame(or=unlist(x[,3:4]),load=unlist(x[,1:2]),
                var=rep(rownames(x),2),
                model=rep(c("Base model","Model adjusted for smoking status"),each=nrow(x)))
}


coef_lung <- foo3(or_lung.1, or_lung.2, load_lung.1 , load_lung.2, myorder)

coef_bladder <- foo3(or_bladder.1, or_bladder.2, load_bladder.1 , load_bladder.2, myorder)


##### settings for Beta plot----
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 28),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
mycolours=c("grey50","tomato","forestgreen","royalblue","gold")

##### Beta plot----
# Lung
xlim=c(0.75, 1.75)
ylim=c(-0.25,0.25)
coef_lung$var_cat=rep(variable_cat)
coef_lung$label=rep(1:(nrow(coef_lung)/2), 2)
coef_lung$mycolour_point=c(lighten(rep(mycolours,times=c(18,28,8,16,28)),0.2),rep(mycolours,times=c(18,28,8,16,28)))
coef_lung$mycolour_lab=darken(coef_lung$mycolour_point, amount=0.5)

p1=ggplot(coef_lung, aes(or, load,
                         label=ifelse(((abs(or-1)<0.1&load==0)|(or==1&abs(load)<0.1)),
                                      "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(aes(shape=model),colour=coef_lung$mycolour_point) +
  scale_shape_manual(values=c(17, 19)) +
  geom_text_repel(color=coef_lung$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Odds Ratio (LASSO)")+
  ylab("Loading Coefficient (sPLS)") +
  ggtitle("Lung cancer") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")


# Bladder
xlim=c(0.75,1.25)
ylim=c(-0.5,0.5)
coef_bladder$var_cat=rep(variable_cat,2)
coef_bladder$label=rep(1:(nrow(coef_bladder)/2), 2)
coef_bladder$mycolour_point=c(lighten(rep(mycolours,times=c(18,28,8,16,28)),0.2),rep(mycolours,times=c(18,28,8,16,28)))
coef_bladder$mycolour_lab=darken(coef_bladder$mycolour_point, amount=0.5)

p2=ggplot(coef_bladder, aes(or, load,
                            label=ifelse(((abs(or-1)<0.1&load==0)|(or==1&abs(load)<0.1)),
                                         "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(aes(shape=model),colour=coef_bladder$mycolour_point) +
  scale_shape_manual(values=c(17, 19)) +
  geom_text_repel(color=coef_bladder$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Odds Ratio (LASSO)")+
  ylab("Loading Coefficient (sPLS)") +
  ggtitle("Bladder cancer") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")


legend <- get_legend(
  # create some space to the left of the legend
  guides(shape = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

pdf("../Figures/or_load_grid.pdf", width=10, height=5.5)
plot_grid(p1, p2, nrow = 1)
# extract the legend from one of the plots
dev.off()

pdf("../Figures/or_load_TT.pdf", width=12, height=4)
plot_grid(p1, p2, nrow = 1)
# extract the legend from one of the plots
dev.off()

# Make legend
models=c("Base model", "Model adjusted on smoking status")

pdf("../Figures/or_load_grid_legend.pdf", width=4, height=11)
par(mar=c(1,1,1,1), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft",pch=c(17,19, rep(NA,length(myorder))),
       legend=c(models,mapply(function(x,n){
         x=paste0(n," - ",x)
         if (grepl("m\\^", x)){
           x=gsub("m\\^","'~m^", x)
           x=sub(")","~')", x)
         }
         if (grepl("\\[", x)){
           x=gsub("\\[","'[", x)
           x=sub("\\(ug","~'(ug", x)
         }
         eval(parse(text=paste0("expression(","'", x,"'",")")))
       }, plot_annot$label_ref, 1:length(myorder))),
       bty="n",text.col = c(lighten("black",amount=0.2),"black",
                            darken(c(rep("grey50",18),
                                     rep("tomato", 28),
                                     rep("forestgreen", 8),
                                     rep("royalblue", 16), 
                                     rep("gold", 28)), 0.5)), cex=0.5, ncol=4, text.width=0.3)
dev.off()




##########################   selection proportion    ##########################

#### Load selection proportion--
selprop_lung.s1=readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_selprop_lung_TT.1.rds")
selprop_lung.s2=readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_selprop_lung_TT.2.rds")
selprop_bladder.s1=readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_selprop_bladder_TT.1.rds")
selprop_bladder.s2=readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_selprop_bladder_TT.2.rds")

selprop_lung.1=readRDS("../Results/lasso_200321/selprop_lung.1.rds")
selprop_lung.2=readRDS("../Results/lasso_200321/selprop_lung.2.rds")
selprop_bladder.1=readRDS("../Results/lasso_200321/selprop_bladder.1.rds")
selprop_bladder.2=readRDS("../Results/lasso_200321/selprop_bladder.2.rds")


##### Extract calibrated pi--
hat_params_lung.s1=GetArgmax(out_lung.s1)
hat_params_lung.s2=GetArgmax(out_lung.s2)
hat_params_bladder.s1=GetArgmax(out_bladder.s1)
hat_params_bladder.s2=GetArgmax(out_bladder.s2)

hat_params_lung.1=GetArgmax(out_lung.1)
hat_params_lung.2=GetArgmax(out_lung.2)
hat_params_bladder.1=GetArgmax(out_bladder.1)
hat_params_bladder.2=GetArgmax(out_bladder.2)

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



##### Reorder rows and transform to vector with empty values in between--

load_lung.1 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_load_lung_TT.1.rds")  # for re-order
re_names.0.1 <- names(load_lung.1)
selprop_lung.s1 <- selprop_lung.s1[match(re_names.0.1, names(selprop_lung.s1))]
selprop_bladder.s1 <- selprop_bladder.s1[match(re_names.0.1, names(selprop_bladder.s1))]

re_names.1 <- names(selprop_lung.1)
names(selprop_lung.s1) <- re_names.1[c(5:46,1:4,47:98)]
names(selprop_bladder.s1) <- re_names.1[c(5:46,1:4,47:98)]

load_lung.2 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_TT_load_lung_TT.2.rds")  # for re-order
re_names.0.2 <- names(load_lung.2)
selprop_lung.s2 <- selprop_lung.s2[match(re_names.0.2, names(selprop_lung.s2))]
selprop_bladder.s2 <- selprop_bladder.s2[match(re_names.0.2, names(selprop_bladder.s2))]

re_names.2 <- names(selprop_lung.2)
names(selprop_lung.s2) <- re_names.2
names(selprop_bladder.s2) <- re_names.2


##### Combine data--
beta_lung.1=readRDS("../Results/lasso_200321/average_beta_lung.1.rds")
colnames=names(beta_lung.1)
myorder=c(colnames[5:46],colnames[1:4],colnames[47:length(colnames)])

selprop_lung_s = foo(selprop_lung.s1, selprop_lung.s2, myorder)
selprop_lung = foo(selprop_lung.1, selprop_lung.2, myorder)

selprop_bladder = foo(selprop_bladder.1, selprop_bladder.2, myorder)
selprop_bladder_s = foo(selprop_bladder.s1, selprop_bladder.s2, myorder)

##### settings for selection proportion plot----
mylabels=plot_annot$label
myref=plot_annot$ref
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 28),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
models=c("Base model", "Model adjusted on smoking status")
mycolours=c("navy","red")

n=2 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),length(myorder)*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,1)


##### settings for slection proportion plot----

### Lung
pi_spls=c(hat_params_lung.s1[2], hat_params_lung.s2[2]) # sPLS Selection proportion threshold
pi_lasso=c(hat_params_lung.1[2],hat_params_lung.2[2]) # LASSO Selection proportion threshold

background_colour="darkturquoise" # For lung
{pdf("lasso_spls_selprop_lung_TT.pdf", height = 6, width = 11)
  par(oma=c(1, 5, 3, 1), mfrow=c(2,1),las=0)
  # sPLS
  par(mar=c(5.5, 0, 0, 0),xpd=FALSE)
  plot(selprop_lung_s, ylim=myrange, xlim=c(1,length(selprop_lung_s)), type="n",
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
  plot(selprop_lung_s, ylim=myrange, type="h",
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
  plot(selprop_lung, ylim=myrange, xlim=c(1,length(selprop_lung)), type="n",
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
  plot(selprop_lung, ylim=rev(myrange), type="h",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  abline(h=pi_lasso, lty=4, lwd=0.5, col=mycolours)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Selection Proportion\n(LASSO)", line=2, cex.lab=0.7)
  par(xpd=TRUE)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  dev.off()
} # Run this to make plot



### Bladder
pi_spls=c(hat_params_bladder.s1[2], hat_params_bladder.s2[2]) # sPLS Selection proportion threshold
pi_lasso=c(hat_params_bladder.1[2],hat_params_bladder.2[2]) # LASSO Selection proportion threshold

background_colour="hotpink" # For bladder
{pdf("lasso_spls_selprop_bladder_TT.pdf", height = 6, width = 11)
  par(oma=c(1, 5, 3, 1), mfrow=c(2,1),las=0)
  # sPLS
  par(mar=c(5.5, 0, 0, 0),xpd=FALSE)
  plot(selprop_bladder_s, ylim=myrange, xlim=c(1,length(selprop_bladder_s)), type="n",
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
  plot(selprop_bladder_s, ylim=myrange, type="h",
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
  plot(selprop_bladder, ylim=myrange, xlim=c(1,length(selprop_bladder)), type="n",
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
  plot(selprop_bladder, ylim=rev(myrange), type="h",
       xaxt="n", yaxt="n",
       col=c(NA, mycolours[1], NA, mycolours[2],NA),
       lwd=1, ylab="", xlab="")
  abline(h=pi_lasso, lty=4, lwd=0.5, col=mycolours)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Selection Proportion\n(LASSO)", line=2, cex.lab=0.7)
  par(xpd=TRUE)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  dev.off()
} # Run this to make plot


