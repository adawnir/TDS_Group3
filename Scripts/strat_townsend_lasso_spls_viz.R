### TDS Project -- Stability selection LASSO & sPLS Visualisation (Run on HPC)
## Programme created by Rin on 21 March
#Edited by fergal to run for townsend stratification

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

### Loading packages----
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(ggrepel)
library(cowplot)
library(pROC)

source("penalisation_functions.R")

### Load labels----
plot_annot=read_csv("../Dictionaries/plot_annot.csv")
plot_annot=plot_annot[-c(1,2,4,21:25),] # Remove age, sex, BMI and townsend
mylabels=plot_annot$label.point


### Load outputs ----
lasso_hat_params=NULL
spls_hat_params=NULL
for (m in 1:8){
  arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("h","l"),each=2),".",c(1,2))[m]
  
  ### Lasso
  lasso_out=readRDS(paste0("../Results/strat_townsend_lasso/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_hat_params=rbind(lasso_hat_params, GetArgmax(lasso_out)) # Extract calibrated pi
  lasso_selprop=readRDS(paste0("../Results/strat_townsend_lasso/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("lasso_selprop_",arr),lasso_selprop) # Assign name
  lasso_beta=readRDS(paste0("../Results/strat_townsend_lasso/average_beta_",arr,".rds")) # Load beta
  lasso_beta=ifelse(CalibratedStableRegression(lasso_out) == 1, lasso_beta, 0) # Shrink non-selected beta to zero
  lasso_beta=exp(lasso_beta) # Exponentiate to OR
  assign(paste0("lasso_beta_",arr),lasso_beta) # Assign name
  lasso_calib=sum(CalibratedStableRegression(lasso_out)) # Number of selected variables
  assign(paste0("lasso_calib_",arr),lasso_calib) # Assign name
  lasso_auc=readRDS(paste0("../Results/strat_townsend_lasso/auc_",arr,".rds")) # Load auc (recalibrated)
  assign(paste0("lasso_auc_",arr),lasso_auc) # Assign name
  lasso_roc=readRDS(paste0("../Results/strat_townsend_lasso/roc_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("lasso_roc_",arr),lasso_roc) # Assign name
  
  ### sPLS
  spls_out=readRDS(paste0("../Results/strat_townsend_spls/out_",arr,".rds")) # Load output
  assign(paste0("spls_out_",arr),spls_out) # Assign name
  spls_hat_params=rbind(spls_hat_params, GetArgmax(spls_out)) # Extract calibrated pi
  spls_selprop=readRDS(paste0("../Results/strat_townsend_spls/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("spls_selprop_",arr),spls_selprop) # Assign name
  spls_beta=readRDS(paste0("../Results/strat_townsend_spls/beta_",arr,".rds")) # Load beta
  spls_beta=ifelse(CalibratedStableRegression(spls_out) == 1, spls_beta, 0) # Shrink non-selected beta to zero
  assign(paste0("spls_beta_",arr),spls_beta) # Assign name
}

rownames(lasso_hat_params)=arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("h","l"),each=2),".",c(1,2))
rownames(spls_hat_params)=arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("h","l"),each=2),".",c(1,2))

## Print model parameters
print(lasso_hat_params)
print(spls_hat_params)

# Reorder rows and transform to dataframe for ggplot
foo = function(h1, l1, h2, l2, order){
  x=data.frame(h1)
  x$l1=l1[match(rownames(x),names(l1))]
  x$h2=h2[match(rownames(x),names(h2))]
  x$l2=l2[match(rownames(x),names(l2))]
  x = slice(x, match(order,rownames(x)))
}



colnames=names(lasso_beta_lung.h.1)
myorder=c(colnames[5],'townsend',colnames[6:37],colnames[1:4],colnames[38:length(colnames)])

lblh1 <- unname(lasso_beta_lung.h.1)
lblh1 <- c(lblh1[5],NA,lblh1[6:37],lblh1[1:4],lblh1[38:89])
names(lblh1) <- myorder
lasso_beta_lung.h.1 <- lblh1
names(lasso_beta_lung.h.1)[2] <- c('townsend')

lblh1 <- unname(lasso_beta_lung.l.1)
lblh1 <- c(lblh1[5],NA,lblh1[6:37],lblh1[1:4],lblh1[38:89])
names(lblh1) <- myorder
lasso_beta_lung.l.1 <- lblh1
names(lasso_beta_lung.l.1)[2] <- c('townsend')

lblh1 <- unname(lasso_beta_lung.h.2)
lblh1 <- c(lblh1[1],NA,lblh1[2:33],NA,NA,NA,NA,lblh1[34:85])
names(lblh1) <- myorder
lasso_beta_lung.h.2 <- lblh1
names(lasso_beta_lung.h.2)[2] <- c('townsend')

lblh1 <- unname(lasso_beta_lung.l.2)
lblh1 <- c(lblh1[1],NA,lblh1[2:33],NA,NA,NA,NA,lblh1[34:85])
names(lblh1) <- myorder
lasso_beta_lung.l.2 <- lblh1
names(lasso_beta_lung.l.2)[2] <- c('townsend')

lblh1 <- unname(spls_beta_lung.h.1)
lblh1 <- c(lblh1[5],NA,lblh1[6:37],lblh1[1:4],lblh1[38:89])
names(lblh1) <- myorder
spls_beta_lung.h.1 <- lblh1
names(spls_beta_lung.h.1)[2] <- c('townsend')

lblh1 <- unname(spls_beta_lung.l.1)
lblh1 <- c(lblh1[5],NA,lblh1[6:37],lblh1[1:4],lblh1[38:89])
names(lblh1) <- myorder
spls_beta_lung.l.1 <- lblh1
names(spls_beta_lung.l.1)[2] <- c('townsend')

lblh1 <- unname(spls_beta_lung.h.2)
lblh1 <- c(lblh1[1],NA,lblh1[2:33],NA,NA,NA,NA,lblh1[34:85])
names(lblh1) <- myorder
spls_beta_lung.h.2 <- lblh1
names(spls_beta_lung.h.2)[2] <- c('townsend')

lblh1 <- unname(spls_beta_lung.l.2)
lblh1 <- c(lblh1[1],NA,lblh1[2:33],NA,NA,NA,NA,lblh1[34:85])
names(lblh1) <- myorder
spls_beta_lung.l.2 <- lblh1
names(spls_beta_lung.l.2)[2] <- c('townsend')



lblh1 <- unname(lasso_beta_bladder.h.1)
lblh1 <- c(lblh1[5],NA,lblh1[6:37],lblh1[1:4],lblh1[38:89])
names(lblh1) <- myorder
lasso_beta_bladder.h.1 <- lblh1
names(lasso_beta_bladder.h.1)[2] <- c('townsend')

lblh1 <- unname(lasso_beta_bladder.l.1)
lblh1 <- c(lblh1[5],NA,lblh1[6:37],lblh1[1:4],lblh1[38:89])
names(lblh1) <- myorder
lasso_beta_bladder.l.1 <- lblh1
names(lasso_beta_bladder.l.1)[2] <- c('townsend')

lblh1 <- unname(lasso_beta_bladder.h.2)
lblh1 <- c(lblh1[1],NA,lblh1[2:33],NA,NA,NA,NA,lblh1[34:85])
names(lblh1) <- myorder
lasso_beta_bladder.h.2 <- lblh1
names(lasso_beta_bladder.h.2)[2] <- c('townsend')

lblh1 <- unname(lasso_beta_bladder.l.2)
lblh1 <- c(lblh1[1],NA,lblh1[2:33],NA,NA,NA,NA,lblh1[34:85])
names(lblh1) <- myorder
lasso_beta_bladder.l.2 <- lblh1
names(lasso_beta_bladder.l.2)[2] <- c('townsend')

lblh1 <- unname(spls_beta_bladder.h.1)
lblh1 <- c(lblh1[5],NA,lblh1[6:37],lblh1[1:4],lblh1[38:89])
names(lblh1) <- myorder
spls_beta_bladder.h.1 <- lblh1
names(spls_beta_bladder.h.1)[2] <- c('townsend')

lblh1 <- unname(spls_beta_bladder.l.1)
lblh1 <- c(lblh1[5],NA,lblh1[6:37],lblh1[1:4],lblh1[38:89])
names(lblh1) <- myorder
spls_beta_bladder.l.1 <- lblh1
names(spls_beta_bladder.l.1)[2] <- c('townsend')

lblh1 <- unname(spls_beta_bladder.h.2)
lblh1 <- c(lblh1[1],NA,lblh1[2:33],NA,NA,NA,NA,lblh1[34:85])
names(lblh1) <- myorder
spls_beta_bladder.h.2 <- lblh1
names(spls_beta_bladder.h.2)[2] <- c('townsend')

lblh1 <- unname(spls_beta_bladder.l.2)
lblh1 <- c(lblh1[1],NA,lblh1[2:33],NA,NA,NA,NA,lblh1[34:85])
names(lblh1) <- myorder
spls_beta_bladder.l.2 <- lblh1
names(spls_beta_bladder.l.2)[2] <- c('townsend')

## Lung
# LASSO
lasso_beta_lung=foo(lasso_beta_lung.h.1, lasso_beta_lung.l.1,
                    lasso_beta_lung.h.2, lasso_beta_lung.l.2, myorder)
# sPLS
spls_beta_lung=foo(spls_beta_lung.h.1, spls_beta_lung.l.1,
                   spls_beta_lung.h.2, spls_beta_lung.l.2, myorder)

## Bladder
# LASSO
lasso_beta_bladder=foo(lasso_beta_bladder.h.1, lasso_beta_bladder.l.1,
                       lasso_beta_bladder.h.2, lasso_beta_bladder.l.2, myorder)
# sPLS
spls_beta_bladder=foo(spls_beta_bladder.h.1, spls_beta_bladder.l.1,
                      spls_beta_bladder.h.2, spls_beta_bladder.l.2, myorder)


### OR plot----
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 20),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")

rownames(lasso_beta_lung)
plot_annot$label.point
mylabels <- c(plot_annot$label.point[1], 'townsend', plot_annot$label.point[2:89])
lasso_beta_lung$namelabel <- mylabels

## Lung
# Base
xlim=c(0.4,1.6)
ylim=c(0.4,1.6)
lasso_beta_lung$var_cat=variable_cat
lasso_beta_lung$label=1:(nrow(lasso_beta_lung))
lasso_beta_lung$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
lasso_beta_lung$mycolour_lab=darken(lasso_beta_lung$mycolour_point, amount=0.5)
p1=ggplot(lasso_beta_lung,
          aes(l1, h1, label=ifelse((abs(h1-1)<0.01&abs(l1-1)<0.01),"",mylabels))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(colour=lasso_beta_lung$mycolour_point) +
  geom_label_repel(size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = -0.001, nudge_x = 0.001,
                   box.padding = 1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Odds Ratio (High SES)")+
  ylab("Mean Odds Ratio (Low SES)") +
  ggtitle("Lung cancer: Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")


xlim=c(0.7,1.3)
ylim=c(0.7,1.3)
p2=ggplot(lasso_beta_lung,
          aes(l2, h2, label=ifelse((abs(h2-1)<0.01&abs(l2-1)<0.01),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(colour=lasso_beta_lung$mycolour_point) +
  geom_label_repel(color=lasso_beta_lung$mycolour_lab,
                   size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.001, nudge_x = 0.001,
                   box.padding = 0.5,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Odds Ratio (High SES)")+
  ylab("Mean Odds Ratio (Low SES)") +
  ggtitle("Lung cancer: Model removed effect of smoking")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/Report/strat_townsend_lasso_lung_adjusted.pdf", width=5, height=5)
p2
dev.off()

## Bladder
# Base
xlim=c(0.8,1.2)
ylim=c(0.8,1.2)
lasso_beta_bladder$var_cat=variable_cat
lasso_beta_bladder$label=1:(nrow(lasso_beta_bladder))
lasso_beta_bladder$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
lasso_beta_bladder$mycolour_lab=darken(lasso_beta_bladder$mycolour_point, amount=0.5)
p3=ggplot(lasso_beta_bladder,
          aes(l1, h1, label=ifelse((abs(h1-1)<0.01&abs(l1-1)<0.01),"",mylabels))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(colour=lasso_beta_bladder$mycolour_point) +
  geom_label_repel(size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = -0.001, nudge_x = 0.001,
                   box.padding = 1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Odds Ratio (High SES)")+
  ylab("Mean Odds Ratio (Low SES)") +
  ggtitle("Bladder cancer: Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(0.8,1.2)
ylim=c(0.8,1.2)
p4=ggplot(lasso_beta_bladder,
          aes(l2, h2, label=ifelse((abs(h2-1)<0.01&abs(l2-1)<0.01),"",mylabels))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(colour=lasso_beta_bladder$mycolour_point) +
  geom_label_repel(color=lasso_beta_lung$mycolour_lab,
                   size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.001, nudge_x = 0.001,
                   box.padding = 0.5,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Odds Ratio (High SES)")+
  ylab("Mean Odds Ratio (Low SES)") +
  ggtitle("Bladder cancer: Model removed effect of smoking")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")


pdf("../Figures/Report/strat_townsend_lasso_bladder_adjusted.pdf", width=5, height=5)
p4
dev.off()

### Loading coefficient plot----
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 20),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")

## Lung
# Base
xlim=c(-0.75,0.75)
ylim=c(-0.75,0.75)
spls_beta_lung$var_cat=variable_cat
spls_beta_lung$label=1:(nrow(spls_beta_lung))
spls_beta_lung$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
spls_beta_lung$mycolour_lab=darken(spls_beta_lung$mycolour_point, amount=0.5)
p5=ggplot(spls_beta_lung,
          aes(l1, h1, label=ifelse((abs(h1)<0.01&abs(l1)<0.01),"",mylabels))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=spls_beta_lung$mycolour_point) +
  geom_label_repel(size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = -0.001, nudge_x = 0.001,
                   box.padding = 1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Loading coefficient (High SES)")+
  ylab("Loading coefficient (Low SES)") +
  ggtitle("Lung cancer: Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(-0.5,0.5)
ylim=c(-0.5,0.5)
p6=ggplot(spls_beta_lung,
          aes(l2, h2, label=ifelse((abs(h2)<0.01&abs(l2)<0.01),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=spls_beta_lung$mycolour_point) +
  geom_label_repel(color=lasso_beta_lung$mycolour_lab,
                   size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = -0.001, nudge_x = 0.001,
                   box.padding = 1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Loading coefficient (High SES)")+
  ylab("Loading coefficient (Low SES)") +
  ggtitle("Lung cancer: Model removed effect of smoking")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

## Bladder
# Base
xlim=c(-0.3,0.3)
ylim=c(-0.3,0.3)
spls_beta_bladder$var_cat=variable_cat
spls_beta_bladder$label=1:(nrow(spls_beta_bladder))
spls_beta_bladder$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
spls_beta_bladder$mycolour_lab=darken(spls_beta_bladder$mycolour_point, amount=0.5)
p7=ggplot(spls_beta_bladder,
          aes(l1, h1, label=ifelse((abs(h1)<0.01&abs(l1)<0.01),"",mylabels))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=spls_beta_bladder$mycolour_point) +
  geom_label_repel(size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = -0.001, nudge_x = 0.001,
                   box.padding = 1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Loading coefficient (High SES)")+
  ylab("Loading coefficient (Low SES)") +
  ggtitle("Bladder cancer: Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(-0.2,0.2)
ylim=c(-0.2,0.2)
p8=ggplot(spls_beta_bladder,
          aes(l2, h2, label=ifelse((abs(h2)<0.01&abs(l2)<0.01),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=spls_beta_bladder$mycolour_point) +
  geom_label_repel(color=lasso_beta_bladder$mycolour_lab,
                   size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = -0.001, nudge_x = 0.001,
                   box.padding = 1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Loading coefficient (High SES)")+
  ylab("Loading coefficient (Low SES)") +
  ggtitle("Bladder cancer: Model removed effect of smoking")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")


pdf("../Figures/Report/strat_townsend_lasso_adjust.pdf", width=10, height=5)
plot_grid(p2,p4, nrow = 1, labels = c('A', 'B'))
dev.off()

pdf("../Figures/Report/strat_townsend_spls_lung_adjust.pdf", width=5, height=5)
plot_grid(p6, nrow = 1)
dev.off()

pdf("../Figures/Report/strat_townsend_spls_bladder_adjust.pdf", width=5, height=5)
plot_grid(p8, nrow = 1)
dev.off()


pdf("../Figures/Final/Supplementary/strat_townsend_or.pdf", width=10, height=10)
plot_grid(p1, p2, p3, p4, nrow = 2, labels = c('A', 'B','C','D'))
dev.off()

pdf("../Figures/Final/Supplementary/strat_townsend_lc.pdf", width=10, height=10)
plot_grid(p5, p6, p7, p8, nrow = 2, labels = c('A', 'B','C','D'))
dev.off()


pdf("../Figures/Final/Supplementary/strat_townsend_or_1row.pdf", width=20, height=5)
plot_grid(p1, p2, p3, p4, nrow = 1)
dev.off()

pdf("../Figures/Final/Supplementary/strat_townsend_lc_1row.pdf", width=20, height=5)
plot_grid(p5, p6, p7, p8, nrow = 1)
dev.off()

