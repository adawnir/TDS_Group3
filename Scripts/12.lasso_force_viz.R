### TDS Project -- Stability selection LASSO Logistic Regression with forced smoking variables Visualisation
## Programme created by Rin Wada on 6 May

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

source("penalisation_functions.R")

library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(cowplot)
library(ggrepel)

##########################   selection proportion    ##########################

plot_annot=read_csv("../Dictionaries/plot_annot.csv")[-c(1,2,21:25,42:45),]

variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 16),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
mycolours=c("grey50","tomato","forestgreen","royalblue","gold")

#### Load selection proportion--
lung=readRDS("../Results/lasso_forced/selprop_lung.0.rds")
bladder=readRDS("../Results/lasso_forced/selprop_bladder.0.rds")

lung_out=readRDS("../Results/lasso_forced/out_lung.0.rds")
bladder_out=readRDS("../Results/lasso_forced/out_bladder.0.rds")

##### Extract calibrated pi--
hat_params_lung=GetArgmax(lung_out)
hat_params_bladder=GetArgmax(bladder_out)

# Reorder selprop
lung = lung[c(12:97)]
bladder = bladder[c(12:97)]

## LASSO - base
xlim=c(0,1.05)
ylim=c(0,1.05)

label=plot_annot$label.point
mycolour_point=rep(mycolours,times=c(18,16,8,16,28))
mycolour_lab=darken(mycolour_point, amount=0.5)

mydata = data.frame(lung = lung, bladder = bladder)

p1=ggplot(mydata,
          aes(lung, bladder,
              label=ifelse((lung < hat_params_lung[,2] & bladder < hat_params_bladder[,2]),"",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = hat_params_lung[,2]),linetype = "dashed",colour = "forestgreen") +
  geom_hline(aes(yintercept = hat_params_bladder[,2]),linetype = "dashed",colour = "forestgreen") +
  geom_point(colour=mycolour_point) +
  geom_label_repel(color=mycolour_lab,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, box.padding = 1,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (Lung cancer)")+
  ylab("Selection Proportion (Bladder cancer)")+
  ggtitle("LASSO, Model adjusted for age, sex and BMI by force")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../Figures/Final/Supplementary/lasso_force_selprop.pdf", width = 5, height = 5)
p1
dev.off()
