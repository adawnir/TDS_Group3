# nvsm LASSO (HPC)
# by ines on march 24

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

## Load packages
LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}

LoadPackages(c("pheatmap","glasso","glmnet","igraph","pROC","caret"))

source("penalisation_functions.R")

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])

## Load data set
arr=c("lung.nvsm.1","bladder.nvsm.1")[m]
dat=readRDS(paste0("../Results/nvsm_denoised/",arr,"_denoised.rds"))

## Make data set
y=dat$case_status
x=subset(dat, select=-c(eid,case_status,nvsm,pvsmn,pvsmy,sm))

x=as.matrix(x)


# Create directories
ifelse(dir.exists("../Figures/strat_nvsm_lasso"),"",dir.create("../Figures/strat_nvsm_lasso"))
ifelse(dir.exists("../Results/strat_nvsm_lasso"),"",dir.create("../Results/strat_nvsm_lasso"))

# Running stability selection
out=CalibrateRegression(xdata=x, ydata=y, K=100, tau=0.5, verbose=FALSE,
                        family="binomial")
# Save plot
pdf(paste0("../Figures/strat_nvsm_lasso/out_",arr,".pdf"), height=5, width=12)
CalibrationPlot(out)
dev.off()

# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out)[1]

# Computing average beta coefficients from models with calibrated lambda
average_beta=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})

# Calibrated selection proportions 
selprop=out$selprop[hat_lambda_id,]

# Save
saveRDS(out, paste0("../Results/strat_nvsm_lasso/out_",arr,".rds")) ### Change path for other stratification
saveRDS(average_beta, paste0("../Results/strat_nvsm_lasso/average_beta_",arr,".rds")) ### Change path for other stratification
saveRDS(selprop, paste0("../Results/strat_nvsm_lasso/selprop_",arr,".rds")) ### Change path for other stratification

# Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
selprop_nonzero=selprop[selprop>0]
myorder=names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
myaucs=NULL
myaucs_table=NULL

for (k in 1:length(myorder)){
  # Using the average beta as a beta, only computing the intercept in the model below (no impact on AUC)
  mymodel=glm(y~offset(x[,myorder[1:k],drop=FALSE]%*%
                              matrix(average_beta[myorder[1:k]],ncol=1)),
              family="binomial")
  myroc=roc(response=y, predictor=mymodel$fitted.values)
  myaucs=c(myaucs, myroc$auc)
  myaucs_table=rbind(myaucs_table, formatC(as.numeric(ci.auc(myroc)), format="f",
                                           digits=4))
}
rownames(myaucs_table)=myorder
colnames(myaucs_table)=c("li","auc","ui")
saveRDS(myaucs_table, paste0("../Results/strat_nvsm_lasso/auc_",arr,".rds")) ### Change path for other stratification
