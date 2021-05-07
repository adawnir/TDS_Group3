# smoker spls
# by ines on march 23, updated april 9 for new data & 22nd
rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

## Load packages
LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}

LoadPackages(c("pheatmap","glasso","glmnet","igraph","pROC","sgPLS"))

source("penalisation_functions.R")
source("pls_functions.R")

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


## Load data set
arr=paste0(rep(c("lung","bladder"),each=2),c("_base_denoised_nvsm","_one_hot"))[m] ### Change name for other stratification
dat<-readRDS(paste0("../Results/nvsm/",arr,".rds")) ### Change path for other stratification

## Make data set
y=dat$case_status
x=subset(dat, select=-c(case_status))

## Create directories
ifelse(dir.exists("../Figures/nvsm_spls"),"",dir.create("../Figures/nvsm_spls")) ### Change path for other stratification
ifelse(dir.exists("../Results/nvsm_spls"),"",dir.create("../Results/nvsm_spls")) ### Change path for other stratification

t0=Sys.time()
## Running stability selection
out <- CalibrateRegression(xdata = x, ydata = y, Lambda=1:ncol(x), family="binomial",
                           implementation="SparsePLS", K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

# Save plots
pdf(paste0("../Figures/nvsm_spls/out_",arr,".pdf"), height=5, width=12)
CalibrationPlot(out)
dev.off()

# stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
pdf(paste0("../Figures/nvsm_spls/out_unconstr_2d_",arr,".pdf"), height=5, width=12) ### Change path for other stratification
CalibrationPlot(out,bi_dim=FALSE)
dev.off()

# Visualising selection proportion paths
stab_mat=out$selprop[,sort.list(apply(out$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat)=1:nrow(stab_mat)

pdf(paste0("../Figures/nvsm_spls/stab_mat_",arr,".pdf"), height=8, width=12) ### Change path for other stratification
pheatmap(stab_mat, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
dev.off()

# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out)[1]

selprop=out$selprop[hat_lambda_id,]

# Computing average beta coefficients from models with calibrated lambda
average_beta=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})

# Save data sets
saveRDS(out, paste0("../Results/nvsm_spls/out_",arr,".rds")) ### Change path for other stratification
saveRDS(selprop, paste0("../Results/nvsm_spls/selprop_",arr,".rds")) ### Change path for other stratification
saveRDS(average_beta, paste0("../Results/nvsm_spls/beta_",arr,".rds")) ### Change path for other stratification


