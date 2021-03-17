
### TDS Project -- sgPLS stability heatmap on HPC
## Programme created by Vivian on 17 March

project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/PLS"
setwd(project_path)

suppressPackageStartupMessages(library(pheatmap))
library(mixOmics)
suppressPackageStartupMessages(library(sgPLS))

source("../pls_functions.R")


##### function-- (editing)
StabilityPlot_sg  <- function(X, Y, NIter=100, plot=TRUE){
  MyStab=NULL
  
  pb=txtProgressBar(style=3)
  for (NVar in seq((ncol(X)-1), 1, by=-1)){
    setTxtProgressBar(pb, 1-(NVar-1)/(ncol(X)))
    TmpStab=NULL
    for (k in 1:NIter){
      s=sample(seq(1, nrow(X)), size=0.8*nrow(X), replace = FALSE) # subsampling procedure
      X_sub=X[s,]
      rownames(X_sub)=s
      Y_sub=Y[s]
      
      TmpsPLS=sgplsda(X_sub, Y_sub, keepX=NVar, ncomp=1)
      TmpStab=rbind(TmpStab, TmpsPLS$loadings$X[,1])
    }
    MyStab=rbind(MyStab, (apply(TmpStab, 2, FUN=function(x){sum(x!=0)})/NIter))
  }
  
  MyStab=rbind(rep(1, ncol(X)), MyStab)
  rownames(MyStab)=seq(ncol(X), 1, by=-1)
  
  if (plot){
    pheatmap(MyStab, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE)
  }
  
  return(MyStab)
}


##### dataset--
lung.1 <- readRDS("../../Results/denoised/lung.1_denoised.rds")
lung.2 <- readRDS("../../Results/denoised/lung.2_denoised.rds")
bladder.1 <- readRDS("../../Results/denoised/bladder.1_denoised.rds")
bladder.2 <- readRDS("../../Results/denoised/bladder.2_denoised.rds")


##### 
## bladder.1
x_bladder.1 <- bladder.1[,-1]
x_bladder.1 <- cbind(x_bladder.1[,5:46], x_bladder.1[,1:4], x_bladder.1[, 47:98])
y_bladder.1 <- bladder.1[,1]

## lung.1
x_lung.1 <- lung.1[,-1]
x_lung.1 <- cbind(x_lung.1[,5:46], x_lung.1[,1:4], x_lung.1[, 47:98])
y_lung.1 <- lung.1[,1]


##### 
## bladder.2
x_bladder.2 <- bladder.2[,-1]
y_bladder.2 <- bladder.2[,1]

## lung.2
x_lung.2 <- lung.2[,-1]
y_lung.2 <- lung.2[,1]


##### stability

set.seed(1)
Stab_bladder.1 = StabilityPlot(X = x_bladder.1, Y =  y_bladder.1, NIter = 100)
pheatmap(Stab_bladder.1, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE,
         height = 30, width = 60, fontsize = 20, filename = "stab_bladder_1.pdf")


set.seed(1)
Stab_bladder.2 = StabilityPlot(X = x_bladder.2, Y =  y_bladder.2, NIter = 100)
pheatmap(Stab_bladder.2, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE,
         height = 30, width = 60, fontsize = 20, filename = "stab_bladder_2.pdf")


set.seed(1)
Stab_lung.1 = StabilityPlot(X = x_lung.1, Y =  y_lung.1, NIter = 100)
pheatmap(Stab_lung.1, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE,
         height = 30, width = 60, fontsize = 20, filename = "stab_lung_1.pdf")


set.seed(1)
Stab_blung.2 = StabilityPlot(X = x_lung.2, Y =  y_lung.2, NIter = 100)
pheatmap(Stab_lung.2, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE,
         height = 30, width = 60, fontsize = 20, filename = "stab_lung_2.pdf")

