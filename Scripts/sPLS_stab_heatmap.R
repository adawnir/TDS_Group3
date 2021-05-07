
### TDS Project -- Stability heatmap sPLS ARRAY JOB (RUN ON HPC)
## Programme created by Vivian on 20 March 

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

suppressPackageStartupMessages(library(pheatmap))
library(mixOmics)
suppressPackageStartupMessages(library(sgPLS))

source("pls_functions_cali.R")


##### Parameters--
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


##### Load data set--
arr=paste0(rep(c("lung_TT","bladder_TT"),each=2),".",1:2)[m]
dat=readRDS(paste0("../Results/PLS_319/train_test/",arr,".rds"))


## stability selection
if (grepl("1$",arr)){
  y <- dat[[1]][,1]
  x <- dat[[1]][,-1]
  
  set.seed(1)
  Stab.1 = StabilityPlot(X = x, Y =  y, NIter = 100, plot = F)
  pheatmap(Stab.1, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE, height = 30, width = 60, fontsize = 25, 
           filename = paste0("../Figures/PLS/sPLS_stab_heatmap",arr,".pdf"))
} else {
  y <- dat[[1]][,1]
  x <- dat[[1]][,-1]
  
  set.seed(1)
  Stab.2 = StabilityPlot(X = x, Y =  y, NIter = 100, plot = F)
  pheatmap(Stab.2, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE, height = 30, width = 60, fontsize = 25, 
           filename = paste0("../Figures/PLS/sPLS_stab_heatmap",arr,".pdf"))

}


