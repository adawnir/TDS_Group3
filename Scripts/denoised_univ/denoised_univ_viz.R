### TDS Project --  Univariate analysis Visualisation (Forest plot and Manhattan plot)
## Programme created by Ines on 11 March reviewed by Rin Wada on 15 March

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/"
setwd(project_path)

# Loading packages
library(RColorBrewer)
library(tidyverse)
library(plotrix)

for (m in 1:4){
  arr=paste0(rep(c("lung","bladder"),each=2),".",1:2)[m]
  betas = readRDS(paste0("../Results/denoised_univ/",arr,"_betas.rds"))
  pval = readRDS(paste0("../Results/denoised_univ/",arr,"_pval.rds")) 
  assign(paste0(arr,"_betas"), betas)
  assign(paste0(arr,"_pval"), pval)
}

bonf=0.05/72
names(lung.1_pval)[which(lung.1_pval < bonf & bladder.1_pval < bonf)]
names(bladder.1_pval)[which(bladder.1_pval < bonf)]
names(lung.1_pval)[which(lung.1_pval < bonf)]

names(lung.2_pval)[which(lung.2_pval < bonf & bladder.2_pval < bonf)]
names(bladder.2_pval)[which(bladder.2_pval < bonf)]
names(lung.2_pval)[which(lung.2_pval < bonf)]

