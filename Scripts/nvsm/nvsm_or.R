# never smokers viz 
# by ines on april 14

# GRID:
# odds ratios : bc vs lc
# full population vs. never smokers
# one LC and one BC
# for LASSO and sPLS

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

### Loading packages----
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(ggrepel)
library(ggpubr)
library(cowplot)

source("penalisation_functions.R")

### Plot labels----
plot_annot=read_csv("../Dictionaries/nvsm_annot.csv")
plot_annot=plot_annot[-c(1,2,21:25, 42),] # Remove age, sex, BMI & smoking

##### Load base model LASSO results--
beta_lung.2=readRDS("../Results/lasso/average_beta_lung.2.rds")
beta_bladder.2=readRDS("../Results/lasso/average_beta_bladder.2.rds")
out_lung.2=readRDS("../Results/lasso/out_lung.2.rds")
out_bladder.2=readRDS("../Results/lasso/out_bladder.2.rds")

##### Shrink non-selected beta to zero--
beta_lung.2=ifelse(CalibratedStableRegression(out_lung.2) == 1, beta_lung.2, 0)
beta_bladder.2=ifelse(CalibratedStableRegression(out_bladder.2) == 1, beta_bladder.2, 0)

##### conver to OR
or_lung.2 <- exp(beta_lung.2)
or_bladder.2 <- exp(beta_bladder.2)


forest <- data.frame(or_lung.2)
forest$or_bladder.2<-or_bladder.2


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
lung <- lasso_beta_lung_base_denoised[-35]
bladder <- lasso_beta_bladder_base_denoised[-35]
forest$or_lung_nvsm <- lung
forest$or_bladder_nvsm <- bladder


### OR plots ----
## Glabal settings
mylabels=plot_annot$label.point
myref=plot_annot$ref

### LASSO OR scatter plots-----
rownames(forest)
forest$`Variable group` <- NA
forest$`Variable group`[1:18] <- 'Sociodemographic'
forest$`Variable group`[19:34] <- 'Health risk'
forest$`Variable group`[35:42] <- 'Environmental'
forest$`Variable group`[43:58] <- 'Medical'
forest$`Variable group`[59:86] <- 'Biomarkers'

xlim=c(0.75,1.25)
ylim=c(0.75,1.25)

mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")

forest$label=c(1:42,47:90)
forest$mycolour_point=rep(mycolours,times=c(18,16,8,16,28))
forest$mycolour_lab=darken(forest$mycolour_point, amount=0.5)


plot1 <- ggplot(forest, aes(x=or_lung.2, y=or_lung_nvsm, 
                            label=ifelse(or_lung.2==1&or_lung_nvsm==1,"",label))) + 
  geom_point(colour=forest$mycolour_point) +
  geom_label_repel(color=forest$mycolour_lab,
                   size=3.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.1)) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  ylab('Never smoker/No smoker in HH - Base model (Mean OR)') +
  xlab('Full population - Model removed effect of smoking (Mean OR)') +
  ggtitle('Lung cancer') +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

plot2 <- ggplot(forest, aes(x=or_bladder.2, y=or_bladder_nvsm, color = `Variable group`, label=ifelse((or_bladder.2==1&or_bladder_nvsm==1),"",label))) + 
  geom_point(colour=forest$mycolour_point) +
  geom_label_repel(color=forest$mycolour_lab,
                   size=3.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  ylab('Never smoker/No smoker in HH - Base model (Mean OR)') +
  xlab('Full population - Model removed effect of smoking (Mean OR)') +
  ggtitle('Bladder cancer') +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")
plot3 <- ggarrange(plot1, plot2, common.legend = T, legend = 'right', nrow = 1)

pdf("../Figures/Report/or_nvsm_grid.pdf", height = 5, width = 10)
plot3
dev.off()

pdf("../Figures/Report/or_nvsm_lung.pdf", height = 5, width = 5)
plot1
dev.off()

pdf("../Figures/Report/or_nvsm_bladder.pdf", height = 5, width = 5)
plot2
dev.off()

##### Load base model sPLS results--- 
out_lung.s2=readRDS("../Results/PLS_319/train_test_spls/sPLS_out_lung_TT.2.rds")
out_bladder.s2=readRDS("../Results/PLS_319/train_test_spls/sPLS_out_bladder_TT.2.rds")

##### Load loadings--
load_lung.2 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_load_lung_TT.2.rds")
load_bladder.2 <- readRDS("../Results/PLS_319/train_test_spls/sPLS_load_bladder_TT.2.rds")

##### Shrink non-selected loadings to zero--
load_lung.2=ifelse(CalibratedStableRegression(out_lung.s2) == 1, load_lung.2, 0)
load_bladder.2=ifelse(CalibratedStableRegression(out_bladder.s2) == 1, load_bladder.2, 0)

##### reverse loadings if necessary
load_lung.2 <- load_lung.2*(-1)

# make dataframes for lung and bladder separately
lung <- spls_beta_lung_base_denoised[-35] # get rid of HH smokers
bladder <- spls_beta_bladder_base_denoised[-35]

spls_lung <- data.frame(load_lung.2)
spls_lung$lc_nvsm<-lung

spls_bladder <- data.frame(load_bladder.2)
spls_bladder$lc_nvsm<-bladder

mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")

spls_lung$label=c(1:42,47:90)
spls_lung$mycolour_point=rep(mycolours,times=c(18,16,8,16,28))
spls_lung$mycolour_lab=darken(spls_lung$mycolour_point, amount=0.5)

spls_bladder$label=c(1:42,47:90)
spls_bladder$mycolour_point=rep(mycolours,times=c(18,16,8,16,28))
spls_bladder$mycolour_lab=darken(spls_bladder$mycolour_point, amount=0.5)

## sPLS plots ----

# Lung Cancer
xlim=c(-0.5,0.5)
ylim=c(-0.5,0.5)
plot4=ggplot(spls_lung,
           aes(x=load_lung.2, y=lc_nvsm, label=ifelse((abs(load_lung.2)<0.01&abs(lc_nvsm)<0.01),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=spls_lung$mycolour_point) +
  geom_label_repel(color=spls_lung$mycolour_lab,
                   size=3.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  ylab('Never smoker/No smoker in HH - Base model (Mean LC)') +
  xlab('Full population - Model removed effect of smoking (Mean LC)') +
  ggtitle("Lung cancer")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")
xlim=c(-0.3,0.3)
ylim=c(-0.3,0.3)
plot5=ggplot(spls_bladder,
           aes(x=load_bladder.2, y=lc_nvsm, label=ifelse((abs(load_bladder.2)<0.01&abs(lc_nvsm)<0.01),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=spls_bladder$mycolour_point) +
  geom_label_repel(color=spls_bladder$mycolour_lab,
                   size=3.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  ylab('Never smoker/No smoker in HH - Base model (Mean LC)') +
  xlab('Full population - Model removed effect of smoking (Mean LC)') +
  ggtitle("Bladder cancer")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/Report/lc_nvsm_lung.pdf", width = 5, height = 5)
plot4
dev.off()
pdf("../Figures/Report/lc_nvsm_bladder.pdf", width = 5, height = 5)
plot5
dev.off()
