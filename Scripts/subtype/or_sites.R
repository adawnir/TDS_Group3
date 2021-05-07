### OR plots
## Ines, April 14

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


### OR plots----

colnames=names(lasso_beta_mal.lower.1)
myorder=c(colnames[5:46],colnames[1:4],colnames[47:length(colnames)])

beta_lower.1 = c(lasso_beta_mal.lower.1[5:38],lasso_beta_mal.lower.1[1:4],lasso_beta_mal.lower.1[39:length(lasso_beta_mal.lower.1)])
beta_upper.1 = c(lasso_beta_mal.upper.1[5:38],lasso_beta_mal.upper.1[1:4],lasso_beta_mal.upper.1[39:length(lasso_beta_mal.upper.1)])

forest <- as.data.frame(beta_lower.1)
forest$betas_upper.1<-beta_upper.1

forestsm <- as.data.frame(lasso_beta_mal.lower.2)
forestsm$beta_upper.2<-lasso_beta_mal.upper.2


### Selection proportion plots----
## Glabal settings
mylabels=plot_annot$label.point
myref=plot_annot$ref

### OR scatter plots-----
rownames(forest)

xlim=c(0.5,1.5)
ylim=c(0.5,1.5)

mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")

forest$label=c(1:90)
forest$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
forest$mycolour_lab=darken(forest$mycolour_point, amount=0.5)

forestsm$label=c(1:42,47:90)
forestsm$mycolour_point=rep(mycolours,times=c(18,16,8,16,28))
forestsm$mycolour_lab=darken(forestsm$mycolour_point, amount=0.5)

plot1 <- ggplot(forest, aes(x=beta_lower.1, y=beta_upper.1, 
                            label=ifelse(beta_lower.1==1&beta_upper.1==1,"",label))) + 
  geom_point(colour=forest$mycolour_point) +
  geom_label_repel(color=forest$mycolour_lab,
                   size=3.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  ylab('Upper lobe, bronchus, or lung (Mean OR)') +
  xlab('Lower lobe, bronchus, or lung (Mean OR)') +
  ggtitle('Lung cancer, base model') +
#  xlim(xlim) +
#  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

plot2 <- ggplot(forestsm, aes(x=lasso_beta_mal.lower.2, y=beta_upper.2, 
                            label=ifelse(lasso_beta_mal.lower.2==1&beta_upper.2==1,"",label))) +
  geom_point(colour=forestsm$mycolour_point) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_label_repel(color=forestsm$mycolour_lab,
                   size=3.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  ylab('Upper lobe, bronchus, or lung (Mean OR)') +
  xlab('Lower lobe, bronchus, or lung (Mean OR)') +
  ggtitle('Lung cancer, model removed effect of smoking') +
#  xlim(xlim) +
#  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")
plot3 <- ggarrange(plot1, plot2, common.legend = T, legend = 'right', nrow = 1)

pdf("../Figures/Report/or_sites_base.pdf", height = 5, width = 5)
plot1
dev.off()

pdf("../Figures/Report/or_sites_smoking.pdf", height = 5, width = 5)
plot2
dev.off()

## sPLS plots ----

# change order of the betas
lc_lower.1 = c(spls_beta_mal.lower.1[5:38],spls_beta_mal.lower.1[1:4],spls_beta_mal.lower.1[39:length(spls_beta_mal.lower.1)])
lc_upper.1 = c(spls_beta_mal.upper.1[5:38],spls_beta_mal.upper.1[1:4],spls_beta_mal.upper.1[39:length(spls_beta_mal.upper.1)])

# make dataframes for base vs. smoking models
spls_base <- data.frame(lc_lower.1)
spls_base$lc_upper.1 <- lc_upper.1

# reverse sign of models adjusted for smokig
lc_lower.2<- (spls_beta_mal.lower.2*(-1))
spls_sm <- data.frame(lc_lower.2)
spls_sm$lc_upper.2 <- (spls_beta_mal.upper.2*(-1))


mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")

spls_base$label=c(1:90)
spls_base$mycolour_point=rep(mycolours,times=c(18,20,8,16,28))
spls_base$mycolour_lab=darken(spls_base$mycolour_point, amount=0.5)

spls_sm$label=c(1:42,47:90)
spls_sm$mycolour_point=rep(mycolours,times=c(18,16,8,16,28))
spls_sm$mycolour_lab=darken(spls_sm$mycolour_point, amount=0.5)


# Lung Cancer
xlim=c(-0.5,0.5)
ylim=c(-0.5,0.5)
plot4=ggplot(spls_base,
             aes(x=lc_lower.1, y=lc_upper.1, label=ifelse((abs(lc_lower.1)<0.01|abs(lc_upper.1)<0.01),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=spls_base$mycolour_point) +
  geom_label_repel(color=spls_base$mycolour_lab,
                   size=3.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.7)) +
  ylab('Upper lobe, bronchus, or lung (Mean LC)') +
  xlab('Lower lobe, bronchus, or lung (Mean LC)') +
  ggtitle('Lung cancer, base model') +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")
xlim=c(-0.25,0.25)
ylim=c(-0.25,0.25)
plot5=ggplot(spls_sm,
             aes(x=lc_lower.2, y=lc_upper.2, label=ifelse((abs(lc_lower.2)<0.01&abs(lc_upper.2)<0.01),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(colour=spls_sm$mycolour_point) +
  geom_label_repel(color=spls_sm$mycolour_lab,
                   size=3.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.2, na.rm=TRUE, fill = alpha(c("white"),0.5)) +
  ylab('Upper lobe, bronchus, or lung (Mean OR)') +
  xlab('Lower lobe, bronchus, or lung (Mean OR)') +
  ggtitle('Lung cancer, model removed effect of smoking') +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/Report/lc_sites_base.pdf", width = 5, height = 5)
plot4
dev.off()
pdf("../Figures/Report/lc_sites_sm.pdf", width = 5, height = 5)
plot5
dev.off()




### UNUSED CODE ----
# variable_cat=c(rep("Sociodemographic",18),
#                rep("Health risk", 20),
#                rep("Environmental", 8),
#                rep("Medical", 16), 
#                rep("Biomarkers", 28))
# variable_cat_86=c(rep("Sociodemographic",18),
#                   rep("Health risk", 16),
#                   rep("Environmental", 8),
#                   rep("Medical", 16),
#                   rep("Biomarkers", 28))
# models=c("Base model", "Model with effect of smoking removed")
# mycolours=c("navy","red")
# names(betas.1)[names(betas.1) == "variable_cat"] <- 'Variable group'

# names(betas.2)[names(betas.2) == "variable_cat_86"] <- 'Variable group'

# betas_slower.1 <- as.data.frame(beta_slower.1)
# betas_supper.1<-as.data.frame(beta_supper.1)
# betas.spls.1<-data.frame(betas_slower.1, betas_supper.1, variable_cat)
# names(betas.spls.1)[names(betas.spls.1) == "variable_cat"] <- 'Variable group'
# 
# betas_slower.2 <- as.data.frame(spls_beta_mal.lower.2)
# betas_supper.2<-as.data.frame(spls_beta_mal.upper.2)
# betas.spls.2<-data.frame(betas_slower.2, betas_supper.2, variable_cat_86)
# names(betas.spls.2)[names(betas.spls.2) == "variable_cat_86"] <- 'Variable group'

### OR scatter plots-----
numberlabels1 <- 1:90
numberlabels1 <- as.character(numberlabels1)

numberlabels2 <- c(1:42,47:90)
numberlabels2 <- as.character(numberlabels2)

plot1 <- ggplot(betas.1, aes(x=beta_lower.1, y=beta_upper.1, color = `Variable group`)) + 
  geom_point(size=2) + geom_text_repel(label=numberlabels1) + 
  geom_abline(linetype = 'dashed', colour = 'grey') +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'grey') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'grey') +
  ylab('Odds Ratio (Upper lobe, bronchus, or lung)') + xlab('Odds Ratio (Lower lobe, bronchus, or lung)') +
  ggtitle('Base model') + theme_bw()
p5 <- plot1 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))


plot2 <- ggplot(betas.2, aes(x=lasso_beta_mal.lower.2, y=lasso_beta_mal.upper.2, color = `Variable group`)) + 
  geom_point(size=2) + geom_text_repel(label=numberlabels2) + 
  geom_abline(linetype = 'dashed', colour = 'grey') +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'grey') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'grey') +
  ylab('Odds Ratio (Upper lobe, bronchus, or lung)') + xlab('Odds Ratio (Lower lobe, bronchus, or lung)') +
  ggtitle('Model with effect of smoking removed') + theme_bw()
p6 <- plot2 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

plot3 <- ggplot(betas.spls.1, aes(x=beta_slower.1, y=beta_supper.1, color = `Variable group`)) + 
  geom_point(size=2) + geom_text_repel(label=numberlabels1) + 
  geom_abline(linetype = 'dashed', colour = 'grey') +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'grey') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'grey') +
  ylab('Loading Coefficient (Upper lobe, bronchus, or lung)') + xlab('Loading Coefficient (Lower lobe, bronchus, or lung)') +
  ggtitle('Base model') + theme_bw()
p3 <- plot3 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))


plot4 <- ggplot(betas.spls.2, aes(x=spls_beta_mal.lower.2, y=spls_beta_mal.upper.2, color = `Variable group`)) + 
  geom_point(size=2) + geom_text_repel(label=numberlabels2) + 
  geom_abline(linetype = 'dashed', colour = 'grey') +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'grey') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'grey') +
  ylab('Loading Coefficient (Upper lobe, bronchus, or lung)') + xlab('Loading Coefficient (Lower lobe, bronchus, or lung)') +
  ggtitle('Model with effect of smoking removed') + theme_bw()
p4 <- plot4 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))


p7 <- ggarrange(p5, p6, common.legend = T, legend = 'right', nrow = 1)

pdf("../Figures/Presentation/or_sites.pdf", height = 5, width = 12)
p7
dev.off()

p8 <- ggarrange(p3, p4, common.legend = T, legend = 'right', nrow = 1)

pdf("../Figures/Presentation/lc_sites.pdf", height = 5, width = 12)
p8
dev.off()

p9 <- ggarrange(p5, p6, p3, p4, common.legend = T, legend = 'right', nrow = 1)

pdf("../Figures/Presentation/sites_all.pdf", height = 5, width = 24)
p9
dev.off()
