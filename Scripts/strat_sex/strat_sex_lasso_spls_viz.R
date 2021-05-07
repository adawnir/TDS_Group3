### TDS Project -- Stability selection LASSO & sPLS Visualisation (Run on HPC)
## Programme created by Rin on 21 March 

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
plot_annot=plot_annot[-c(1,2,21:25),] # Remove age, sex and BMI


### Load outputs ----
lasso_hat_params=NULL
spls_hat_params=NULL
for (m in 1:8){
  arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("f","m"),each=2),".",c(1,2))[m]
  
  ### Lasso
  lasso_out=readRDS(paste0("../Results/strat_sex_lasso/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_hat_params=rbind(lasso_hat_params, GetArgmax(lasso_out)) # Extract calibrated pi
  lasso_selprop=readRDS(paste0("../Results/strat_sex_lasso/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("lasso_selprop_",arr),lasso_selprop) # Assign name
  lasso_beta=readRDS(paste0("../Results/strat_sex_lasso/average_beta_",arr,".rds")) # Load beta
  lasso_beta=ifelse(CalibratedStableRegression(lasso_out) == 1, lasso_beta, 0) # Shrink non-selected beta to zero
  lasso_beta=exp(lasso_beta) # Exponentiate to OR
  assign(paste0("lasso_beta_",arr),lasso_beta) # Assign name
  lasso_calib=sum(CalibratedStableRegression(lasso_out)) # Number of selected variables
  assign(paste0("lasso_calib_",arr),lasso_calib) # Assign name
  lasso_auc=readRDS(paste0("../Results/strat_sex_lasso/auc_",arr,".rds")) # Load auc (recalibrated)
  assign(paste0("lasso_auc_",arr),lasso_auc) # Assign name
  lasso_roc=readRDS(paste0("../Results/strat_sex_lasso/roc_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("lasso_roc_",arr),lasso_roc) # Assign name

  ### sPLS
  spls_out=readRDS(paste0("../Results/strat_sex_spls/out_",arr,".rds")) # Load output
  assign(paste0("spls_out_",arr),spls_out) # Assign name
  spls_hat_params=rbind(spls_hat_params, GetArgmax(spls_out)) # Extract calibrated pi
  spls_selprop=readRDS(paste0("../Results/strat_sex_spls/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("spls_selprop_",arr),spls_selprop) # Assign name
  spls_beta=readRDS(paste0("../Results/strat_sex_spls/beta_",arr,".rds")) # Load beta
  spls_beta=ifelse(CalibratedStableRegression(spls_out) == 1, spls_beta, 0) # Shrink non-selected beta to zero
  assign(paste0("spls_beta_",arr),spls_beta) # Assign name
  spls_roc=readRDS(paste0("../Results/strat_sex_spls/roc_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("spls_roc_",arr),spls_roc) # Assign name
}

rownames(lasso_hat_params)=arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("f","m"),each=2),".",c(1,2))
rownames(spls_hat_params)=arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("f","m"),each=2),".",c(1,2))

## Print model parameters
print(lasso_hat_params)
print(spls_hat_params)

# Reorder rows and transform to dataframe for ggplot
foo = function(f1, m1, f2, m2, order){
  x=data.frame(f1)
  x$m1=m1[match(rownames(x),names(m1))]
  x$f2=f2[match(rownames(x),names(f2))]
  x$m2=m2[match(rownames(x),names(m2))]
  x = slice(x, match(order,rownames(x)))
}
colnames=names(lasso_beta_lung.f.1)
myorder=c(colnames[5:38],colnames[1:4],colnames[39:length(colnames)])

## Lung
# LASSO
lasso_beta_lung=foo(lasso_beta_lung.f.1, lasso_beta_lung.m.1,
                    lasso_beta_lung.f.2, lasso_beta_lung.m.2, myorder)
# sPLS
spls_beta_lung=foo(spls_beta_lung.f.1, spls_beta_lung.m.1,
                   spls_beta_lung.f.2, spls_beta_lung.m.2, myorder)

## Bladder
# LASSO
lasso_beta_bladder=foo(lasso_beta_bladder.f.1, lasso_beta_bladder.m.1,
                    lasso_beta_bladder.f.2, lasso_beta_bladder.m.2, myorder)
# sPLS
spls_beta_bladder=foo(spls_beta_bladder.f.1, spls_beta_bladder.m.1,
                   spls_beta_bladder.f.2, spls_beta_bladder.m.2, myorder)

# Calculate CI for ROC coordinates
foo2 = function(f1, m1, f2, m2){
  f1.coord = data.frame(fpr=1-f1$specificities,tpr=f1$sensitivities)
  m1.coord = data.frame(fpr=1-m1$specificities,tpr=m1$sensitivities)
  f2.coord = data.frame(fpr=1-f2$specificities,tpr=f2$sensitivities)
  m2.coord = data.frame(fpr=1-m2$specificities,tpr=m2$sensitivities)
  x=list(f1.coord, m1.coord, f2.coord, m2.coord)
  names(x) = c("f1","m1","f2","m2")
  return(x)
}
# foo.ci = function(f1, m1, f2, m2){
#   f1.ci = cbind(seq(1, 0, -0.01),ci.se(f1, seq(1, 0, -0.01), boot.n=1000)[,-2])
#   m1.ci = cbind(seq(1, 0, -0.01),ci.se(m1,seq(1, 0, -0.01), boot.n=1000)[,-2])
#   f2.ci = cbind(seq(1, 0, -0.01),ci.se(f2, seq(1, 0, -0.01), boot.n=1000)[,-2])
#   m2.ci = cbind(seq(1, 0, -0.01),ci.se(m2, seq(1, 0, -0.01),boot.n=1000)[,-2])
#   x=list(f1.ci, m1.ci, f2.ci, m2.ci)
#   names(x) = c("f1","m1","f2","m2")
#   return(x)
# }

## Lung
# LASSO
lasso_roc_lung=foo2(lasso_roc_lung.f.1, lasso_roc_lung.m.1,
                    lasso_roc_lung.f.2, lasso_roc_lung.m.2)
# lasso_ci_lung=foo.ci(lasso_roc_lung.f.1, lasso_roc_lung.m.1,
#                      lasso_roc_lung.f.2, lasso_roc_lung.m.2)
# sPLS
spls_roc_lung=foo2(spls_roc_lung.f.1, spls_roc_lung.m.1,
                   spls_roc_lung.f.2, spls_roc_lung.m.2)
# spls_ci_lung=foo.ci(spls_roc_lung.f.1, spls_roc_lung.m.1,
#                     spls_roc_lung.f.2, spls_roc_lung.m.2)

## Bladder
# LASSO
lasso_roc_bladder=foo2(lasso_roc_bladder.f.1, lasso_roc_bladder.m.1,
                       lasso_roc_bladder.f.2, lasso_roc_bladder.m.2)
# lasso_ci_bladder=foo.ci(lasso_roc_bladder.f.1, lasso_roc_bladder.m.1,
#                         lasso_roc_bladder.f.2, lasso_roc_bladder.m.2)
# sPLS
spls_roc_bladder=foo2(spls_roc_bladder.f.1, spls_roc_bladder.m.1,
                      spls_roc_bladder.f.2, spls_roc_bladder.m.2)
# spls_ci_bladder=foo.ci(spls_roc_bladder.f.1, spls_roc_bladder.m.1,
#                        spls_roc_bladder.f.2, spls_roc_bladder.m.2)

# saveRDS(lasso_ci_lung,"../Results/strat_sex_lasso/lasso_ci_lung.rds")
# saveRDS(lasso_ci_bladder,"../Results/strat_sex_lasso/lasso_ci_bladder.rds")
# saveRDS(spls_ci_lung,"../Results/strat_sex_spls/spls_ci_lung.rds")
# saveRDS(spls_ci_bladder,"../Results/strat_sex_spls/spls_ci_bladder.rds")

# Calculate AUC and confidence intervals
foo3 = function(f1, m1, f2, m2, f1_calib, m1_calib, f2_calib, m2_calib){
  f1 = round(as.numeric(f1[f1_calib,]),2)
  m1 = round(as.numeric(m1[m1_calib,]),2)
  f2 = round(as.numeric(f2[f2_calib,]),2)
  m2 = round(as.numeric(m2[m2_calib,]),2)
  x=c(paste0(f1[2]," [",f1[1],"-",f1[3],"]"),
      paste0(m1[2]," [",m1[1],"-",m1[3],"]"),
      paste0(f2[2]," [",f2[1],"-",f2[3],"]"),
      paste0(m2[2]," [",m2[1],"-",m2[3],"]"))
      
}
foo4 = function(f1, m1, f2, m2){
  f1 = round(as.numeric(ci.auc(f1)),2)
  m1 = round(as.numeric(ci.auc(m1)),2)
  f2 = round(as.numeric(ci.auc(f2)),2)
  m2 = round(as.numeric(ci.auc(m2)),2)
  x=c(paste0(f1[2]," [",f1[1],"-",f1[3],"]"),
      paste0(m1[2]," [",m1[1],"-",m1[3],"]"),
      paste0(f2[2]," [",f2[1],"-",f2[3],"]"),
      paste0(m2[2]," [",m2[1],"-",m2[3],"]"))
}

## Lung
# LASSO
lasso_auc_lung=foo3(lasso_auc_lung.f.1, lasso_auc_lung.m.1,
                    lasso_auc_lung.f.2, lasso_auc_lung.m.2,
                    lasso_calib_lung.f.1, lasso_calib_lung.m.1,
                    lasso_calib_lung.f.2, lasso_calib_lung.m.2)
# sPLS
spls_auc_lung=foo4(spls_roc_lung.f.1, spls_roc_lung.m.1,
                   spls_roc_lung.f.2, spls_roc_lung.m.2)

## Bladder
# LASSO
lasso_auc_bladder=foo3(lasso_auc_bladder.f.1, lasso_auc_bladder.m.1,
                       lasso_auc_bladder.f.2, lasso_auc_bladder.m.2,
                       lasso_calib_bladder.f.1, lasso_calib_bladder.m.1,
                       lasso_calib_bladder.f.2, lasso_calib_bladder.m.2)
# sPLS
spls_auc_bladder=foo4(spls_roc_bladder.f.1, spls_roc_bladder.m.1,
                      spls_roc_bladder.f.2, spls_roc_bladder.m.2)

### Main (LASSO Base model)----
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 20),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")


## Lung
xlim=c(0.25,1.75)
ylim=c(0.25,1.75)
lasso_beta_lung$var_cat=variable_cat
lasso_beta_lung$label=1:(nrow(lasso_beta_lung))
lasso_beta_lung$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
lasso_beta_lung$mycolour_lab=darken(lasso_beta_lung$mycolour_point, amount=0.5)

p1=ggplot(lasso_beta_lung,
          aes(f1, m1, label=ifelse((f1==1&m1==1),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(colour=lasso_beta_lung$mycolour_point) +
  geom_label_repel(color=lasso_beta_lung$mycolour_lab,
                   size=3.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Odds Ratio (Females)")+
  ylab("Mean Odds Ratio (Males)") +
  ggtitle("Lung cancer, Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

## Bladder
xlim=c(0.8,1.2)
ylim=c(0.8,1.2)
lasso_beta_bladder$var_cat=variable_cat
lasso_beta_bladder$label=1:(nrow(lasso_beta_bladder))
lasso_beta_bladder$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
lasso_beta_bladder$mycolour_lab=darken(lasso_beta_bladder$mycolour_point, amount=0.5)

p2=ggplot(lasso_beta_bladder,
          aes(f1, m1, label=ifelse((f1==1&m1==1),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(colour=lasso_beta_bladder$mycolour_point) +
  geom_label_repel(color=lasso_beta_bladder$mycolour_lab,
                  size=3.5, segment.color="grey",
                  segment.size=0.5, max.overlaps = Inf,
                  label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Odds Ratio (Females)")+
  ylab("Mean Odds Ratio (Males)") +
  ggtitle("Bladder cancer, Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/Final/Main/strat_sex_lasso_lung_base.pdf", width = 5, height = 5)
p1
dev.off()
pdf("../Figures/Final/Main/strat_sex_lasso_bladder_base.pdf", width = 5, height = 5)
p2
dev.off()

### Presentation (LASSO Base model)----
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 20),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")
mylabels=plot_annot$label.point

## Lung
xlim=c(0.25,1.75)
ylim=c(0.25,1.75)
lasso_beta_lung$var_cat=variable_cat
lasso_beta_lung$label=1:(nrow(lasso_beta_lung))
lasso_beta_lung$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
lasso_beta_lung$mycolour_lab=darken(lasso_beta_lung$mycolour_point, amount=0.5)

p3=ggplot(lasso_beta_lung,
          aes(f1, m1, label=ifelse((f1==1|m1==1),"",mylabels))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(colour=lasso_beta_lung$mycolour_point) +
  geom_label_repel(size=3.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   nudge_y = 0.02, nudge_x = 0.02, box.padding = 1,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Odds Ratio (Females)")+
  ylab("Mean Odds Ratio (Males)") +
  ggtitle("Lung cancer, Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

## Bladder
xlim=c(0.8,1.2)
ylim=c(0.8,1.2)
lasso_beta_bladder$var_cat=variable_cat
lasso_beta_bladder$label=1:(nrow(lasso_beta_bladder))
lasso_beta_bladder$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
lasso_beta_bladder$mycolour_lab=darken(lasso_beta_bladder$mycolour_point, amount=0.5)

p4=ggplot(lasso_beta_bladder,
          aes(f1, m1, label=ifelse((f1==1&m1==1),"",mylabels))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(colour=lasso_beta_bladder$mycolour_point) +
  geom_label_repel(size=3.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   nudge_y = 0.02, nudge_x = 0.02, box.padding = 1,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Odds Ratio (Females)")+
  ylab("Mean Odds Ratio (Males)") +
  ggtitle("Bladder cancer, Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/Final/Presentation/strat_sex_lasso_lung_base.pdf", width = 5, height = 5)
p3
dev.off()
pdf("../Figures/Final/Presentation/strat_sex_lasso_bladder_base.pdf", width = 5, height = 5)
p4
dev.off()

### Supplementary (LASSO, Model removed effect of smoking) ----
xlim=c(0.7,1.3)
ylim=c(0.7,1.3)

p5=ggplot(lasso_beta_lung,
          aes(f2, m2, label=ifelse((f2==1&m2==1),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(colour=lasso_beta_lung$mycolour_point) +
  geom_label_repel(color=lasso_beta_lung$mycolour_lab,
                  size=3.5, segment.color="grey",
                  segment.size=0.5, max.overlaps = Inf,
                  label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Odds Ratio (Females)")+
  ylab("Mean Odds Ratio (Male)") +
  ggtitle("Lung cancer, Model removed effect of smoking")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

p6=ggplot(lasso_beta_bladder,
          aes(f2, m2, label=ifelse((f2==1&m2==1),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(colour=lasso_beta_bladder$mycolour_point) +
  geom_label_repel(color=lasso_beta_bladder$mycolour_lab,
                  size=3.5, segment.color="grey",
                  segment.size=0.5, max.overlaps = Inf,
                  label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Odds Ratio (Females)")+
  ylab("Mean Odds Ratio (Male)") +
  ggtitle("Bladder cancer, Model removed effect of smoking")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")


pdf("../Figures/Final/Supplementary/strat_sex_lasso_lung_rem.pdf", width = 5, height = 5)
p5
dev.off()
pdf("../Figures/Final/Supplementary/strat_sex_lasso_bladder_rem.pdf", width = 5, height = 5)
p6
dev.off()

### Supplementary (sPLS-DA)----
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 20),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")

## Lung
# Base
xlim=c(-0.8,0.8)
ylim=c(-0.8,0.8)
spls_beta_lung$var_cat=variable_cat
spls_beta_lung$label=1:(nrow(spls_beta_lung))
spls_beta_lung$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
spls_beta_lung$mycolour_lab=darken(spls_beta_lung$mycolour_point, amount=0.5)
p7=ggplot(spls_beta_lung,
          aes(f1, m1, label=ifelse((abs(f1)<0.01&abs(m1)<0.01),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=spls_beta_lung$mycolour_point) +
  geom_label_repel(color=spls_beta_lung$mycolour_lab,
                  size=3.5, segment.color="grey",
                  segment.size=0.5, max.overlaps = Inf,
                  label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Loading Coefficient (Females)")+
  ylab("Mean Loading Coefficient (Males)") +
  ggtitle("Lung cancer, Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(-0.5,0.5)
ylim=c(-0.5,0.5)
p8=ggplot(spls_beta_lung,
          aes(f2, m2, label=ifelse((abs(f2)<0.01&abs(m2)<0.01),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=spls_beta_lung$mycolour_point) +
  geom_label_repel(color=spls_beta_lung$mycolour_lab,
                  size=3.5, segment.color="grey",
                  segment.size=0.5, max.overlaps = Inf,
                  label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Loading Coefficient (Females)")+
  ylab("Mean Loading Coefficient (Males)") +
  ggtitle("Lung cancer, Model removed effect of smoking")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

## Bladder
# Base
xlim=c(-0.25,0.25)
ylim=c(-0.25,0.25)
spls_beta_bladder$var_cat=variable_cat
spls_beta_bladder$label=1:(nrow(spls_beta_bladder))
spls_beta_bladder$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
spls_beta_bladder$mycolour_lab=darken(spls_beta_bladder$mycolour_point, amount=0.5)
p9=ggplot(spls_beta_bladder,
          aes(f1, m1, label=ifelse((abs(f1)<0.01&abs(m1)<0.01),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=spls_beta_bladder$mycolour_point) +
  geom_label_repel(color=spls_beta_bladder$mycolour_lab,
                  size=3.5, segment.color="grey",
                  segment.size=0.5, max.overlaps = Inf,
                  label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Loading Coefficient (Females)")+
  ylab("Mean Loading Coefficient (Males)") +
  ggtitle("Bladder cancer, Base model")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(-0.5,0.5)
ylim=c(-0.5,0.5)
p10=ggplot(spls_beta_bladder,
          aes(f2, m2, label=ifelse((abs(f2)<0.01&abs(m2)<0.01),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=spls_beta_bladder$mycolour_point) +
  geom_label_repel(color=spls_beta_bladder$mycolour_lab,
                  size=3.5, segment.color="grey",
                  segment.size=0.5, max.overlaps = Inf,
                  label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Mean Loading Coefficient (Females)")+
  ylab("Mean Loading Coefficient (Males)") +
  ggtitle("Bladder cancer, Model removed effect of smoking")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/Final/Supplementary/strat_sex_spls_lung_base.pdf", width = 5, height = 5)
p7
dev.off()
pdf("../Figures/Final/Supplementary/strat_sex_spls_lung_rem.pdf", width = 5, height = 5)
p8
dev.off()
pdf("../Figures/Final/Supplementary/strat_sex_spls_bladder_base.pdf", width = 5, height = 5)
p9
dev.off()
pdf("../Figures/Final/Supplementary/strat_sex_spls_bladder_rem.pdf", width = 5, height = 5)
p10
dev.off()

### Supplementary (Performance Prediction) ----
mycolours = c("lightcoral",
              "steelblue",
              darken("lightcoral",0.5),
              darken("steelblue",0.5))

## LASSO
# Lung
{pdf("../Figures/Final/Supplementary/strat_sex_lasso_roc_lung.pdf", width = 7, height = 7)
  par(mar=c(5,5,1,1))
  plot(lasso_roc_lung[[1]], ylim = c(0,1),
       col = mycolours[1], type="n", lwd = 2,
       xlab = "False Positive Rate", ylab = "True Positive Rate")
  abline(0,1, lty = 3, col = "grey")
  # polygon(x=c(rev(lasso_ci_lung[[1]][,1]),lasso_ci_lung[[1]][,1]),
  #         y=c(lasso_ci_lung[[1]][,2],rev(lasso_ci_lung[[1]][,3])),
  #         col = alpha(mycolours[1],0.1), border = mycolours[1])
  # polygon(x=c(rev(lasso_ci_lung[[2]][,1]),lasso_ci_lung[[2]][,1]),
  #         y=c(lasso_ci_lung[[2]][,2],rev(lasso_ci_lung[[2]][,3])),
  #         col = alpha(mycolours[2],0.1), border = mycolours[2])
  # polygon(x=c(rev(lasso_ci_lung[[3]][,1]),lasso_ci_lung[[3]][,1]),
  #         y=c(lasso_ci_lung[[3]][,2],rev(lasso_ci_lung[[3]][,3])),
  #         col = alpha(mycolours[3],0.1), border = mycolours[3])
  # polygon(x=c(rev(lasso_ci_lung[[4]][,1]),lasso_ci_lung[[4]][,1]),
  #         y=c(lasso_ci_lung[[4]][,2],rev(lasso_ci_lung[[4]][,3])),
  #         col = alpha(mycolours[4],0.1), border = mycolours[4])
  lines(lasso_roc_lung[[1]], col = mycolours[1], type="l", lwd = 2)
  lines(lasso_roc_lung[[2]], col = mycolours[2], type="l", lwd = 2)
  lines(lasso_roc_lung[[3]], col = mycolours[3], type="l", lwd = 2)
  lines(lasso_roc_lung[[4]], col = mycolours[4], type="l", lwd = 2)
  legend("bottomright",
         legend = c(paste0("Females: LASSO Base model; AUC = ",
                           lasso_auc_lung[1]),
                    paste0("Females: LASSO Model removed effect of smoking; AUC = ",
                           lasso_auc_lung[3]),
                    paste0("Males: LASSO Base model; AUC = ",
                           lasso_auc_lung[2]),
                    paste0("Males: LASSO Model removed effect of smoking; AUC = ",
                           lasso_auc_lung[4])),
         lty = 1, lwd = 2, col = mycolours[c(1,3,2,4)], cex = 0.7)
  dev.off()
}

# Bladder
{pdf("../Figures/Final/Supplementary/strat_sex_lasso_roc_bladder.pdf", width = 7, height = 7)
  par(mar=c(5,5,1,1))
  plot(lasso_roc_bladder[[1]], ylim = c(0,1),
       col = mycolours[1], type="n", lwd = 2,
       xlab = "False Positive Rate", ylab = "True Positive Rate")
  abline(0,1, lty = 3, col = "grey")
  # polygon(x=c(rev(lasso_ci_bladder[[1]][,1]),lasso_ci_bladder[[1]][,1]),
  #         y=c(lasso_ci_bladder[[1]][,2],rev(lasso_ci_bladder[[1]][,3])),
  #         col = alpha(mycolours[1],0.1), border = mycolours[1])
  # polygon(x=c(rev(lasso_ci_bladder[[2]][,1]),lasso_ci_bladder[[2]][,1]),
  #         y=c(lasso_ci_bladder[[2]][,2],rev(lasso_ci_bladder[[2]][,3])),
  #         col = alpha(mycolours[2],0.1), border = mycolours[2])
  # polygon(x=c(rev(lasso_ci_bladder[[3]][,1]),lasso_ci_bladder[[3]][,1]),
  #         y=c(lasso_ci_bladder[[3]][,2],rev(lasso_ci_bladder[[3]][,3])),
  #         col = alpha(mycolours[3],0.1), border = mycolours[3])
  # polygon(x=c(rev(lasso_ci_bladder[[4]][,1]),lasso_ci_bladder[[4]][,1]),
  #         y=c(lasso_ci_bladder[[4]][,2],rev(lasso_ci_bladder[[4]][,3])),
  #         col = alpha(mycolours[4],0.1), border = mycolours[4])
  lines(lasso_roc_bladder[[1]], col = mycolours[1], type="l", lwd = 2)
  lines(lasso_roc_bladder[[2]], col = mycolours[2], type="l", lwd = 2)
  lines(lasso_roc_bladder[[3]], col = mycolours[3], type="l", lwd = 2)
  lines(lasso_roc_bladder[[4]], col = mycolours[4], type="l", lwd = 2)
  legend("bottomright",
         legend = c(paste0("Females: LASSO Base model; AUC = ",
                           lasso_auc_bladder[1]),
                    paste0("Females: LASSO Model removed effect of smoking; AUC = ",
                           lasso_auc_bladder[3]),
                    paste0("Males: LASSO Base model; AUC = ",
                           lasso_auc_bladder[2]),
                    paste0("Males: LASSO Model removed effect of smoking; AUC = ",
                           lasso_auc_bladder[4])),
         lty = 1, lwd = 2, col = mycolours[c(1,3,2,4)], cex = 0.7)
  dev.off()
}

## sPLS
# Lung
{pdf("../Figures/Final/Supplementary/strat_sex_spls_roc_lung.pdf", width = 7, height = 7)
  par(mar=c(5,5,1,1))
  plot(spls_roc_lung[[1]], ylim = c(0,1),
       col = mycolours[1], type="n", lwd = 2,
       xlab = "False Positive Rate", ylab = "True Positive Rate")
  abline(0,1, lty = 3, col = "grey")
  # polygon(x=c(rev(spls_ci_lung[[1]][,1]),spls_ci_lung[[1]][,1]),
  #         y=c(spls_ci_lung[[1]][,2],rev(spls_ci_lung[[1]][,3])),
  #         col = alpha(mycolours[1],0.1), border = mycolours[1])
  # polygon(x=c(rev(spls_ci_lung[[2]][,1]),spls_ci_lung[[2]][,1]),
  #         y=c(spls_ci_lung[[2]][,2],rev(spls_ci_lung[[2]][,3])),
  #         col = alpha(mycolours[2],0.1), border = mycolours[2])
  # polygon(x=c(rev(spls_ci_lung[[3]][,1]),spls_ci_lung[[3]][,1]),
  #         y=c(spls_ci_lung[[3]][,2],rev(spls_ci_lung[[3]][,3])),
  #         col = alpha(mycolours[3],0.1), border = mycolours[3])
  # polygon(x=c(rev(spls_ci_lung[[4]][,1]),spls_ci_lung[[4]][,1]),
  #         y=c(spls_ci_lung[[4]][,2],rev(spls_ci_lung[[4]][,3])),
  #         col = alpha(mycolours[4],0.1), border = mycolours[4])
  lines(spls_roc_lung[[1]], col = mycolours[1], type="l", lwd = 2)
  lines(spls_roc_lung[[2]], col = mycolours[2], type="l", lwd = 2)
  lines(spls_roc_lung[[3]], col = mycolours[3], type="l", lwd = 2)
  lines(spls_roc_lung[[4]], col = mycolours[4], type="l", lwd = 2)
  legend("bottomright",
         legend = c(paste0("Females: sPLS-DA Base model; AUC = ",
                           spls_auc_lung[1]),
                    paste0("Females: sPLS-DA Model removed effect of smoking; AUC = ",
                           spls_auc_lung[3]),
                    paste0("Males: sPLS-DA Base model; AUC = ",
                           spls_auc_lung[2]),
                    paste0("Males: sPLS-DA Model removed effect of smoking; AUC = ",
                           spls_auc_lung[4])),
         lty = 1, lwd = 2, col = mycolours[c(1,3,2,4)], cex = 0.7)
  dev.off()
}


# Bladder
{pdf("../Figures/Final/Supplementary/strat_sex_spls_roc_bladder.pdf", width = 7, height = 7)
  par(mar=c(5,5,1,1))
  plot(spls_roc_bladder[[1]], ylim = c(0,1),
       col = mycolours[1], type="n", lwd = 2,
       xlab = "False Positive Rate", ylab = "True Positive Rate")
  abline(0,1, lty = 3, col = "grey")
  # polygon(x=c(rev(spls_ci_bladder[[1]][,1]),spls_ci_bladder[[1]][,1]),
  #         y=c(spls_ci_bladder[[1]][,2],rev(spls_ci_bladder[[1]][,3])),
  #         col = alpha(mycolours[1],0.1), border = mycolours[1])
  # polygon(x=c(rev(spls_ci_bladder[[2]][,1]),spls_ci_bladder[[2]][,1]),
  #         y=c(spls_ci_bladder[[2]][,2],rev(spls_ci_bladder[[2]][,3])),
  #         col = alpha(mycolours[2],0.1), border = mycolours[2])
  # polygon(x=c(rev(spls_ci_bladder[[3]][,1]),spls_ci_bladder[[3]][,1]),
  #         y=c(spls_ci_bladder[[3]][,2],rev(spls_ci_bladder[[3]][,3])),
  #         col = alpha(mycolours[3],0.1), border = mycolours[3])
  # polygon(x=c(rev(spls_ci_bladder[[4]][,1]),spls_ci_bladder[[4]][,1]),
  #         y=c(spls_ci_bladder[[4]][,2],rev(spls_ci_bladder[[4]][,3])),
  #         col = alpha(mycolours[4],0.1), border = mycolours[4])
  lines(spls_roc_bladder[[1]], col = mycolours[1], type="l", lwd = 2)
  lines(spls_roc_bladder[[2]], col = mycolours[2], type="l", lwd = 2)
  lines(spls_roc_bladder[[3]], col = mycolours[3], type="l", lwd = 2)
  lines(spls_roc_bladder[[4]], col = mycolours[4], type="l", lwd = 2)
  legend("bottomright",
         legend = c(paste0("Females: sPLS-DA Base model; AUC = ",
                           spls_auc_bladder[1]),
                    paste0("Females: sPLS-DA Model removed effect of smoking; AUC = ",
                           spls_auc_bladder[3]),
                    paste0("Males: sPLS-DA Base model; AUC = ",
                           spls_auc_bladder[2]),
                    paste0("Males: sPLS-DA Model removed effect of smoking; AUC = ",
                           spls_auc_bladder[4])),
         lty = 1, lwd = 2, col = mycolours[c(1,3,2,4)], cex = 0.7)
  dev.off()
}


### Supplementary (Selection proportion) ----
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
lasso_selprop_lung.f=foo(lasso_selprop_lung.f.1,
                         lasso_selprop_lung.f.2,
                         myorder)
spls_selprop_lung.f=foo(spls_selprop_lung.f.1,
                        spls_selprop_lung.f.2,
                        myorder)
lasso_selprop_bladder.m=foo(lasso_selprop_bladder.m.1,
                            lasso_selprop_bladder.m.2,
                            myorder)

spls_selprop_bladder.m=foo(spls_selprop_bladder.m.1,
                           spls_selprop_bladder.m.2,
                           myorder)
lasso_selprop_bladder.f=foo(lasso_selprop_bladder.f.1,
                            lasso_selprop_bladder.f.2,
                            myorder)

spls_selprop_bladder.f=foo(spls_selprop_bladder.f.1,
                           spls_selprop_bladder.f.2,
                           myorder)
lasso_selprop_lung.m=foo(lasso_selprop_lung.m.1,
                         lasso_selprop_lung.m.2,
                         myorder)
spls_selprop_lung.m=foo(spls_selprop_lung.m.1,
                        spls_selprop_lung.m.2,
                        myorder)
## Glabal settings
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
myrange=c(0,1)

## Female
# Lung
pi_spls=c(spls_hat_params[1:2,2]) # sPLS Selection proportion threshold
pi_lasso=c(lasso_hat_params[1:2,2]) # LASSO Selection proportion threshold
file_path="lung_female"
spls=spls_selprop_lung.f
lasso=lasso_selprop_lung.f
background_colour="darkturquoise" # For lung
{pdf(paste0("../Figures/Final/Supplementary/strat_sex_selprop_",file_path,".pdf"), height = 6, width = 11)
  par(oma=c(1, 5, 3, 1), mfrow=c(2,1),las=0)
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

# Bladder
pi_spls=c(spls_hat_params[5:6,2]) # sPLS Selection proportion threshold
pi_lasso=c(lasso_hat_params[5:6,2]) # LASSO Selection proportion threshold
file_path="bladder_female"
spls=spls_selprop_bladder.f
lasso=lasso_selprop_bladder.f
background_colour="hotpink" # For bladder
{pdf(paste0("../Figures/Final/Supplementary/strat_sex_selprop_",file_path,".pdf"), height = 6, width = 11)
  par(oma=c(1, 5, 3, 1), mfrow=c(2,1),las=0)
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


## Male
pi_spls=c(spls_hat_params[3:4,2]) # sPLS Selection proportion threshold
pi_lasso=c(lasso_hat_params[3:4,2]) # LASSO Selection proportion threshold
file_path="lung_male"
spls=spls_selprop_lung.m
lasso=lasso_selprop_lung.m
background_colour="darkturquoise" # For lung
{pdf(paste0("../Figures/Final/Supplementary/strat_sex_selprop_",file_path,".pdf"), height = 6, width = 11)
  par(oma=c(1, 5, 3, 1), mfrow=c(2,1),las=0)
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


# Bladder
pi_spls=c(spls_hat_params[7:8,2]) # sPLS Selection proportion threshold
pi_lasso=c(lasso_hat_params[7:8,2]) # LASSO Selection proportion threshold
file_path="bladder_male"
spls=spls_selprop_bladder.m
lasso=lasso_selprop_bladder.m
background_colour="hotpink" # For bladder
{pdf(paste0("../Figures/Final/Supplementary/strat_sex_selprop_",file_path,".pdf"), height = 6, width = 11)
  par(oma=c(1, 5, 3, 1), mfrow=c(2,1),las=0)
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


# # Make legend
# pdf("../Figures/strat_sex_viz/selprop_legend.pdf", width=4, height=1)
# par(mar=c(1,1,1,1), mfrow=c(1,1), xpd=TRUE)
# plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
# legend("center",lty=1, lwd=1, legend=models, col=mycolours)
# dev.off()









