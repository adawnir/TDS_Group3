
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
out_lung.s1=readRDS("../Results/PLS_319/train_test_spls/sPLS_out_lung_TT.1.rds")
out_lung.s2=readRDS("../Results/PLS_319/train_test_spls/sPLS_out_lung_TT.2.rds")
out_bladder.s1=readRDS("../Results/PLS_319/train_test_spls/sPLS_out_bladder_TT.1.rds")
out_bladder.s2=readRDS("../Results/PLS_319/train_test_spls/sPLS_out_bladder_TT.2.rds")

out_lung.1=readRDS("../Results/lasso/out_lung.1.rds")
out_lung.2=readRDS("../Results/lasso/out_lung.2.rds")
out_bladder.1=readRDS("../Results/lasso/out_bladder.1.rds")
out_bladder.2=readRDS("../Results/lasso/out_bladder.2.rds")

##### Load loadings--
load_lung.1 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_load_lung_TT.1.rds")
load_lung.2 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_load_lung_TT.2.rds")
load_bladder.1 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_load_bladder_TT.1.rds")
load_bladder.2 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_load_bladder_TT.2.rds")

##### Shrink non-selected loadings to zero--
load_lung.1=ifelse(CalibratedStableRegression(out_lung.s1) == 1, load_lung.1, 0)
load_lung.2=ifelse(CalibratedStableRegression(out_lung.s2) == 1, load_lung.2, 0)
load_bladder.1=ifelse(CalibratedStableRegression(out_bladder.s1) == 1, load_bladder.1, 0)
load_bladder.2=ifelse(CalibratedStableRegression(out_bladder.s2) == 1, load_bladder.2, 0)

##### reverse loadings if necessary
#load_lung.1 <- load_lung.1*(-1)
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
names(load_lung.1) <- re_names.1[c(5:38,1:4,39:90)]
names(load_bladder.1) <- re_names.1[c(5:38,1:4,39:90)]
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
               rep("Health risk", 20),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
mycolours=c("grey50","tomato","forestgreen","royalblue","gold")

##### Beta plot----
# Lung
xlim=c(0.75, 1.65)
ylim=c(-0.1,0.1)
coef_lung$var_cat=rep(variable_cat)
coef_lung$label=rep(1:(nrow(coef_lung)/2), 2)
coef_lung$mycolour_point=c(lighten(rep(mycolours,times=c(18,20,8,16,28)),0.2),rep(mycolours,times=c(18,20,8,16,28)))
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
  xlab("Mean Odds Ratio (LASSO)")+
  ylab("Mean Loading Coefficient (sPLS)") +
  ggtitle("Lung cancer") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")


# Bladder
xlim=c(0.85,1.2)
ylim=c(-0.5,0.5)
coef_bladder$var_cat=rep(variable_cat,2)
coef_bladder$label=rep(1:(nrow(coef_bladder)/2), 2)
coef_bladder$mycolour_point=c(lighten(rep(mycolours,times=c(18,20,8,16,28)),0.2),rep(mycolours,times=c(18,20,8,16,28)))
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
  xlab("Mean Odds Ratio (LASSO)")+
  ylab("Mean Loading Coefficient (sPLS)") +
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

pdf("../Figures/or_load_main.pdf", width=10, height=5)
plot_grid(p1, p2, nrow = 1)
# extract the legend from one of the plots
dev.off()

pdf("or_load_lung.pdf", width=5, height=5)
p1
dev.off()
pdf("or_load_bladder.pdf", width=5, height=5)
p2
dev.off()


##### for presentation ----

# Reorder rows and transform to dataframe for ggplot
# Reorder rows and transform to dataframe for ggplot
foo2 = function(bb, ll, order){
  x=data.frame(bb)
  x$ll=ll[match(rownames(x),names(ll))]
  x = slice(x, match(order,rownames(x)))
}

foo22 = function(reff, bb, ll, order){
  x=data.frame(reff)
  x$bb=bb[match(rownames(x),names(bb))]
  x$ll=ll[match(rownames(x),names(ll))]
  x = slice(x, match(order,rownames(x)))
}


coef_lasso_1 <- foo2(or_bladder.1, or_lung.1, myorder)
coef_lasso_2 <- foo22(or_bladder.1, or_bladder.2, or_lung.2, myorder)
coef_spls_1 <- foo2(load_bladder.1, load_lung.1, myorder)
coef_spls_2 <- foo22(load_bladder.1, load_bladder.2, load_lung.2, myorder)


foo3 = function(b1, l1, b2, l2, order){
  x=data.frame(b1)
  x$l1=l1[match(rownames(x),names(l1))]
  x$b2=b2[match(rownames(x),names(b2))]
  x$l2=l2[match(rownames(x),names(l2))]
  x = slice(x, match(order,rownames(x)))
  df=data.frame(lung=unlist(x[,c(2,4)]),bladder=unlist(x[,c(1,3)]),
                var=rep(rownames(x),2),
                model=rep(c("Base model","Model adjusted for smoking status"),each=nrow(x)))
}


coef_lasso <- foo2(or_bladder.1, or_lung.1,
                   or_bladder.2, or_lung.2, myorder)

coef_spls <- foo2(load_bladder.1, load_lung.1,
                     load_bladder.2, load_lung.2, myorder)


variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 20),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")
mylabels=plot_annot$label.point
models=c("Base model", "Model adjusted on smoking status")
general_label <- 1:(nrow(coef_lasso_1))

## LASSO - base
xlim=c(0.75,1.75)
ylim=c(0.75,1.75)
coef_lasso_1$var_cat=variable_cat
coef_lasso_1$label=1:(nrow(coef_lasso_1))
coef_lasso_1$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
coef_lasso_1$mycolour_lab=darken(coef_lasso_1$mycolour_point, amount=0.5)

p1=ggplot(coef_lasso_1,
          aes(ll, bb, label=ifelse((ll==1 & bb==1),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(colour=coef_lasso_1$mycolour_point) +
  geom_label_repel(color=coef_lasso_1$mycolour_lab,
                   size=4, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   nudge_y = 0.01, nudge_x = 0.01, box.padding = 0.3,
                   label.size = NA, label.padding=.001, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Odds Ratio (Lung cancer)")+
  ylab("Mean Odds Ratio (Bladder cancer)") +
  ggtitle("LASSO, Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/report_lasso_base_or.pdf", width = 5, height = 5)
p1
dev.off()

## LASSO - adjust
xlim=c(0.75,1.25)
ylim=c(0.75,1.25)
coef_lasso_2$var_cat=variable_cat
coef_lasso_2$label=1:(nrow(coef_lasso_2))
coef_lasso_2$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
coef_lasso_2$mycolour_lab=darken(coef_lasso_2$mycolour_point, amount=0.5)

p2=ggplot(coef_lasso_2,
          aes(ll, bb, label=ifelse((ll==1 & bb==1),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(colour=coef_lasso_2$mycolour_point) +
  geom_label_repel(color=coef_lasso_2$mycolour_lab,
                   size=4, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.001, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Odds Ratio (Lung cancer)")+
  ylab("Mean Odds Ratio (Bladder cancer)") +
  ggtitle("LASSO, Model removed effect of smoking")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/report_lasso_adjust_or.pdf", width = 5, height = 5)
p2
dev.off()

## sPLS 
xlim=c(-0.25,0.5)
ylim=c(-0.25,0.5)
coef_spls_1$var_cat=variable_cat
coef_spls_1$label=1:(nrow(coef_spls_1))
coef_spls_1$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
coef_spls_1$mycolour_lab=darken(coef_spls_1$mycolour_point, amount=0.5)

p3=ggplot(coef_spls_1 ,
          aes(ll, bb, label=ifelse((abs(ll) <= 0.005 & abs(bb) <= 0.005),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=coef_spls_1$mycolour_point) +
  geom_label_repel(color=coef_spls_1$mycolour_lab,
                   size=4, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   nudge_y = 0.01, nudge_x = 0.02, box.padding = 0.3,
                   label.size = NA, label.padding=.001, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Loading Coefficients (Lung cancer)")+
  ylab("Mean Loading Coefficients (Bladder cancer)") +
  ggtitle("sPLS-DA, Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/report_spls_base_load.pdf", width = 5, height = 5)
p3
dev.off()

## sPLS - adjust
xlim=c(-0.25,0.15)
ylim=c(-0.25,0.15)
coef_spls_2$var_cat=variable_cat
coef_spls_2$label=1:(nrow(coef_spls_2))
coef_spls_2$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
coef_spls_2$mycolour_lab=darken(coef_spls_2$mycolour_point, amount=0.5)

p4=ggplot(coef_spls_2,
          aes(ll, bb, label=ifelse((abs(ll) <= 0.005 & abs(bb)<=0.005),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=coef_spls_2$mycolour_point) +
  geom_label_repel(color=coef_spls_2$mycolour_lab,
                   size=4, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   nudge_y = 0.01, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Loading Coefficients (Lung cancer)")+
  ylab("Mean Loading Coefficients (Bladder cancer)") +
  ggtitle("sPLS-DA, Model removed effect of smoking")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/report_spls_adjust_load.pdf", width = 5, height = 5)
p4
dev.off()




##########################   selection proportion    ##########################

#### Load selection proportion--
selprop_lung.s1=readRDS("../Results/PLS_319/train_test_spls/sPLS_selprop_lung_TT.1.rds")
selprop_lung.s2=readRDS("../Results/PLS_319/train_test_spls/sPLS_selprop_lung_TT.2.rds")
selprop_bladder.s1=readRDS("../Results/PLS_319/train_test_spls/sPLS_selprop_bladder_TT.1.rds")
selprop_bladder.s2=readRDS("../Results/PLS_319/train_test_spls/sPLS_selprop_bladder_TT.2.rds")

selprop_lung.1=readRDS("../Results/lasso/selprop_lung.1.rds")
selprop_lung.2=readRDS("../Results/lasso/selprop_lung.2.rds")
selprop_bladder.1=readRDS("../Results/lasso/selprop_bladder.1.rds")
selprop_bladder.2=readRDS("../Results/lasso/selprop_bladder.2.rds")


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

load_lung.1 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_load_lung_TT.1.rds")  # for re-order
re_names.0.1 <- names(load_lung.1)
selprop_lung.s1 <- selprop_lung.s1[match(re_names.0.1, names(selprop_lung.s1))]
selprop_bladder.s1 <- selprop_bladder.s1[match(re_names.0.1, names(selprop_bladder.s1))]

re_names.1 <- names(selprop_lung.1)
names(selprop_lung.s1) <- re_names.1[c(5:38,1:4,39:90)]
names(selprop_bladder.s1) <- re_names.1[c(5:38,1:4,39:90)]

load_lung.2 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_load_lung_TT.2.rds")  # for re-order
re_names.0.2 <- names(load_lung.2)
selprop_lung.s2 <- selprop_lung.s2[match(re_names.0.2, names(selprop_lung.s2))]
selprop_bladder.s2 <- selprop_bladder.s2[match(re_names.0.2, names(selprop_bladder.s2))]

re_names.2 <- names(selprop_lung.2)
names(selprop_lung.s2) <- re_names.2
names(selprop_bladder.s2) <- re_names.2


##### Combine data--
beta_lung.1=readRDS("../Results/lasso/average_beta_lung.1.rds")
colnames=names(beta_lung.1)
myorder=c(colnames[5:38],colnames[1:4],colnames[39:length(colnames)])

selprop_lung_s = foo(selprop_lung.s1, selprop_lung.s2, myorder)
selprop_lung = foo(selprop_lung.1, selprop_lung.2, myorder)

selprop_bladder = foo(selprop_bladder.1, selprop_bladder.2, myorder)
selprop_bladder_s = foo(selprop_bladder.s1, selprop_bladder.s2, myorder)

##### settings for selection proportion plot----
mylabels=plot_annot$label
myref=plot_annot$ref
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 20),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
models=c("Base model", "Model removed effect of smoking")
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
{pdf("../Figures/lasso+spls/lasso_spls_selprop_lung_TT.pdf", height = 6, width = 11)
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
{pdf("../Figures/lasso+spls/lasso_spls_selprop_bladder_TT.pdf", height = 6, width = 11)
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


##### For presentation ---

# Reorder rows and transform to dataframe for ggplot
foo2 = function(bb, ll, order){
  x=data.frame(bb)
  x$ll=ll[match(rownames(x),names(ll))]
  x = slice(x, match(order,rownames(x)))
}


sel_lasso_1 <- foo2(selprop_bladder.1, selprop_lung.1, myorder)
sel_lasso_2 <- foo22(selprop_bladder.1, selprop_bladder.2, selprop_lung.2, myorder)
sel_spls_1 <- foo2(selprop_bladder.s1, selprop_lung.s1, myorder)
sel_spls_2 <- foo22(selprop_bladder.s1, selprop_bladder.s2, selprop_lung.s2, myorder)

## LASSO - base
xlim=c(0,1.05)
ylim=c(0,1.05)
sel_lasso_1$var_cat=variable_cat
sel_lasso_1$label=1:(nrow(sel_lasso_1))
sel_lasso_1$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
sel_lasso_1$mycolour_lab=darken(sel_lasso_1$mycolour_point, amount=0.5)


p5=ggplot(sel_lasso_1,
          aes(ll, bb, label=ifelse((ll < 0.9 & bb < 0.86),"",mylabels))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_hline(aes(yintercept = 0.86),linetype = "dashed",colour = "navy") +
  geom_point(colour=sel_lasso_1$mycolour_point) +
  geom_label_repel(color=sel_lasso_1$mycolour_lab,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (Lung cancer)")+
  ylab("Selection Proportion (Bladder cancer)")+
  ggtitle("LASSO, Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/report_lasso_base_selprop.pdf", width = 5, height = 5)
p5
dev.off()


## LASSO - adjusted
variable_cat_2=c(rep("Sociodemographic",18),
               rep("Health risk", 16),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
mylabels_2 <- mylabels[-c(35:38)]
  
xlim=c(0,1.04)
ylim=c(0,1.04)
sel_lasso_2$var_cat=variable_cat
sel_lasso_2$label=1:(nrow(sel_lasso_2))
sel_lasso_2$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
sel_lasso_2$mycolour_lab=darken(sel_lasso_2$mycolour_point, amount=0.5)


p6=ggplot(sel_lasso_2,
          aes(ll, bb, label=ifelse((ll < 0.9 & bb < 0.72),"",mylabels))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "red") +
  geom_hline(aes(yintercept = 0.72),linetype = "dashed",colour = "red") +
  geom_point(colour=sel_lasso_2$mycolour_point) +
  geom_label_repel(color=sel_lasso_2$mycolour_lab,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, segment.ncp = 3,
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.001, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (Lung cancer)")+
  ylab("Selection Proportion (Bladder cancer)")+
  ggtitle("LASSO, Model removed effect of smoking")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/report_lasso_adjust_selprop.pdf", width = 5, height = 5)
p6
dev.off()


## sPLS - base
xlim=c(0,1.04)
ylim=c(0,1.04)
sel_spls_1$var_cat=variable_cat
sel_spls_1$label=1:(nrow(sel_spls_1))
sel_spls_1$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
sel_spls_1$mycolour_lab=darken(sel_spls_1$mycolour_point, amount=0.5)


p7=ggplot(sel_spls_1,
          aes(ll, bb, label=ifelse((ll < 0.9 & bb < 0.86),"",mylabels))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_hline(aes(yintercept = 0.86),linetype = "dashed",colour = "navy") +
  geom_point(colour=sel_spls_1$mycolour_point) +
  geom_label_repel(color=sel_spls_1$mycolour_lab,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.001, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (Lung cancer)")+
  ylab("Selection Proportion (Bladder cancer)")+
  ggtitle("sPLS-DA, Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/report_spls_base_selprop.pdf", width = 5, height = 5)
p7
dev.off()


## sPLS- adjusted
variable_cat_2=c(rep("Sociodemographic",18),
                 rep("Health risk", 16),
                 rep("Environmental", 8),
                 rep("Medical", 16), 
                 rep("Biomarkers", 28))
mylabels_2 <- mylabels[-c(35:38)]

xlim=c(0,1.1)
ylim=c(0,1.1)
sel_spls_2$var_cat=variable_cat
sel_spls_2$label=1:(nrow(sel_spls_2))
sel_spls_2$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
sel_spls_2$mycolour_lab=darken(sel_spls_2$mycolour_point, amount=0.5)


p8=ggplot(sel_spls_2,
          aes(ll, bb, label=ifelse((ll < 0.9 & bb < 0.88),"",mylabels))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "red") +
  geom_hline(aes(yintercept = 0.88),linetype = "dashed",colour = "red") +
  geom_point(colour=sel_spls_2$mycolour_point) +
  geom_label_repel(color=sel_spls_2$mycolour_lab,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, segment.ncp = 3,
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (Lung cancer)")+
  ylab("Selection Proportion (Bladder cancer)")+
  ggtitle("sPLS-DA, Model removed effect of smoking")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/report_spls_adjust_selprop.pdf", width = 5, height = 5)
p8
dev.off()
