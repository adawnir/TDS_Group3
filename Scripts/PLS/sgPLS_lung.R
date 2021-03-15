

### TDS Project -- sgPLS stability selection on lung on HPC for extract output
## Programme created by Vivian on 13 March

rm(list=ls())

project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/PLS"
setwd(project_path)

source("template/penalisation_functions.R")
source("template/functions.R")

suppressPackageStartupMessages(library(mixOmics))
suppressPackageStartupMessages(library(sgPLS))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(pheatmap))

##### load data
lung.1 <- readRDS("../../Results/denoised/lung.1_denoised.rds")
lung.2 <- readRDS("../../Results/denoised/lung.2_denoised.rds")
bladder.1 <- readRDS("../../Results/denoised/bladder.1_denoised.rds")
bladder.2 <- readRDS("../../Results/denoised/bladder.2_denoised.rds")


##### Group index
Xgroups_1 = c(18, 46, 54, 70)
Xgroups_2 = c(18, 42, 50, 66)


##### Stability selection on sparse group PLS-DA--
### lung.1
X_sg1 <-  lung.1[, -1]
X_sg1 <- cbind(X_sg1[,5:46], X_sg1[,1:4], X_sg1[, 47:98])
Y_sg1 <-  lung.1[, 1]

niter=100 # TO INCREASE FOR ACTUAL ANALYSES
t0=Sys.time()
alpha_list=seq(0.05,0.95,by=0.05)
stability_alpha=NULL
for (k in 1:length(alpha_list)){
  myalpha=alpha_list[k]
  tmp=CalibrateRegression(xdata = X_sg1, ydata = Y_sg1, Lambda=1:(length(Xgroups_1)+1), 
                          K = niter, family="binomial", implementation="SparseGroupPLS", 
                          ind.block.x = Xgroups_1, alpha.x=myalpha)
  assign(paste0("out_", k), tmp)
  stability_alpha=cbind(stability_alpha, tmp$S)
}
rownames(stability_alpha)=tmp$Lambda
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))


## Stability score for different numbers of groups and alpha--
colnames(stability_alpha)=alpha_list
pdf("sg_Cali_plot_lung.1.pdf", width=10, height=7)
pheatmap(stability_alpha, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
dev.off()

## Calibrating the parameter alpha--
hat_alpha_id=which.max(apply(stability_alpha,2,max))
hat_alpha=alpha_list[hat_alpha_id]


## Calibrating the number of groups and threshold in selection proportion (pi)--
sg_out_1 = eval(parse(text=paste0("out_", hat_alpha_id)))
 
saveRDS(sg_out_1, "sgPLS_stability_selection_lung.1.rds")


### lung.2
X_sg2 <-  lung.1[, -1]
Y_sg2 <-  lung.1[, 1]

niter=100 # TO INCREASE FOR ACTUAL ANALYSES
t0=Sys.time()
alpha_list=seq(0.05,0.95,by=0.05)
stability_alpha=NULL
for (k in 1:length(alpha_list)){
  myalpha=alpha_list[k]
  tmp=CalibrateRegression(xdata = X_sg2, ydata = Y_sg2, Lambda=1:(length(Xgroups_1)+1), 
                          K = niter, family="binomial", implementation="SparseGroupPLS", 
                          ind.block.x = Xgroups_2, alpha.x=myalpha)
  assign(paste0("out_", k), tmp)
  stability_alpha=cbind(stability_alpha, tmp$S)
}
rownames(stability_alpha)=tmp$Lambda
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))


## Stability score for different numbers of groups and alpha--
colnames(stability_alpha)=alpha_list
pdf("sg_Cali_plot_lung.2.pdf", width=10, height=7)
pheatmap(stability_alpha, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
dev.off()

## Calibrating the parameter alpha--
hat_alpha_id=which.max(apply(stability_alpha,2,max))
hat_alpha=alpha_list[hat_alpha_id]


## Calibrating the number of groups and threshold in selection proportion (pi)--
sg_out_2 = eval(parse(text=paste0("out_", hat_alpha_id)))

saveRDS(sg_out_2, "sgPLS_stability_selection_lung.2.rds")

