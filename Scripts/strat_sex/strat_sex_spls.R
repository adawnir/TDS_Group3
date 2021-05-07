### TDS Project -- Stability selection PLS ARRAY JOB (RUN ON HPC)
## Programme created by Vivian on 19 March reviewed by Rin on 20 March

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
arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("f","m"),each=2),".",1:2)[m] ### Change name for other stratification
dat=readRDS(paste0("../Results/strat_sex_denoised/",arr,"_denoised.rds")) ### Change path for other stratification

## Make data set
y=dat$case_status
x=dat[,-1]
train=readRDS(paste0("../Results/strat_sex_denoised/",arr,"_split.rds"))
y_train=y[train]
x_train=x[train,]
y_test=y[-train]
x_test=x[-train, ]

## Create directories
ifelse(dir.exists("../Figures/strat_sex_spls"),"",dir.create("../Figures/strat_sex_spls")) ### Change path for other stratification
ifelse(dir.exists("../Results/strat_sex_spls"),"",dir.create("../Results/strat_sex_spls")) ### Change path for other stratification

t0=Sys.time()
## Running stability selection
out <- CalibrateRegression(xdata = x_train, ydata = y_train, Lambda=1:ncol(x), family="binomial",
                           implementation="SparsePLS", K=100)
t1=Sys.time()
print(t1-t0)

# Save plots
pdf(paste0("../Figures/strat_sex_spls/out_",arr,".pdf"), height=5, width=12)
CalibrationPlot(out)
dev.off()

# stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
pdf(paste0("../Figures/strat_sex_spls/out_unconstr_2d_",arr,".pdf"), height=5, width=12) ### Change path for other stratification
CalibrationPlot(out,bi_dim=FALSE)
dev.off()

# Visualising selection proportion paths
stab_mat=out$selprop[,sort.list(apply(out$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat)=1:nrow(stab_mat)

pdf(paste0("../Figures/strat_sex_spls/stab_mat_",arr,".pdf"), height=8, width=12) ### Change path for other stratification
pheatmap(stab_mat, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
dev.off()

# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out)[1]

selprop=out$selprop[hat_lambda_id,]

# Computing average beta coefficients from models with calibrated lambda
average_beta=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})

# Save data sets
saveRDS(out, paste0("../Results/strat_sex_spls/out_",arr,".rds")) ### Change path for other stratification
saveRDS(selprop, paste0("../Results/strat_sex_spls/selprop_",arr,".rds")) ### Change path for other stratification
saveRDS(average_beta, paste0("../Results/strat_sex_spls/beta_",arr,".rds")) ### Change path for other stratification

# Prediction performance (with recalibration)
selected=CalibratedStableRegression(out)
selected=names(selected[selected==1])
mypls=plsda(X=x_train[,selected], Y=y_train, ncomp=1)
predictions=predict(mypls, newdata=x_test[,selected])
y_pred=predictions$predict[,2,1]
myroc=roc(response=y_test, predictor=y_pred)
saveRDS(myroc, paste0("../Results/strat_sex_spls/roc_",arr,".rds"))

# # Misclassification rates based on max dist (with recalibration)
# y_pred <- predictions$class$max.dist
# mr_controls <- sum(y_pred == 1)/length(y_pred)
# mr_cases <- sum(y_pred == 0)/length(y_pred)
# 
# saveRDS(mr_controls, paste0("../Results/strat_sex_spls/mr_controls_",arr,".rds"))
# saveRDS(mr_cases, paste0("../Results/strat_sex_spls/mr_cases_",arr,".rds"))


