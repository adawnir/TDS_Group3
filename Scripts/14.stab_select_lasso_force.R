### TDS Project -- Stability selection LASSO Logistic Regression with forced smoking variables ARRAY JOB (RUN ON HPC)
## Programme created by Rin Wada on 24 March

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
arr=paste0(c("lung","bladder"),".",1)[m]
dat=readRDS(paste0("../Results/denoised/",arr,"_denoised.rds"))

## Make data set
y=dat$case_status
x=as.matrix(dat[,-1])
set.seed(100)
train=createDataPartition(y,p=0.8,list=F)
y_train=y[train]
x_train=x[train,]
y_test=y[-train]
x_test=x[-train, ]

# For base model: Run model with forced smoking variables to check for bias
# Create directories
ifelse(dir.exists("../Figures/lasso_forced"),"",dir.create("../Figures/lasso_forced"))
ifelse(dir.exists("../Results/lasso_forced"),"",dir.create("../Results/lasso_forced"))

t0=Sys.time()
out=CalibrateRegression(xdata=x_train, ydata=y_train,
                        penalty.factor=c(rep(0,4),rep(1,ncol(x_train)-4)),
                        K=100, tau=0.5, verbose=FALSE, family="binomial")
t1=Sys.time()
print(t1-t0)

# Save plot
pdf(paste0("../Figures/lasso_forced/out_",arr,".pdf"), height=5, width=12)
CalibrationPlot(out)
dev.off()

# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out)[1]

# Computing average beta coefficients from models with calibrated lambda
average_beta=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})

# Calibrated selection proportions 
selprop=out$selprop[hat_lambda_id,]

# Save
saveRDS(out, paste0("../Results/lasso_forced/out_",arr,".rds"))
saveRDS(average_beta, paste0("../Results/lasso_forced/average_beta_",arr,".rds"))
saveRDS(selprop, paste0("../Results/lasso_forced/selprop_",arr,".rds"))

selprop_nonzero=selprop[selprop>0]
myorder=names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]

### Using average beta coefficients
# Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
myaucs=NULL
myaucs_table=NULL

t0=Sys.time()
for (k in 1:length(myorder)){
  # Using the average beta as a beta, only computing the intercept in the model below (no impact on AUC)
  mymodel=glm(y_test~offset(x_test[,myorder[1:k],drop=FALSE]%*%
                              matrix(average_beta[myorder[1:k]],ncol=1)),
              family="binomial")
  myroc=roc(response=y_test, predictor=mymodel$fitted.values)
  myaucs=c(myaucs, myroc$auc)
  myaucs_table=rbind(myaucs_table, formatC(as.numeric(ci.auc(myroc)), format="f",
                                           digits=4))
}
t1=Sys.time()
print(t1-t0)

rownames(myaucs_table)=myorder
colnames(myaucs_table)=c("li","auc","ui")
saveRDS(myaucs_table, paste0("../Results/lasso_forced/auc_average_beta_",arr,".rds"))

### Recalibration
# Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
myaucs=NULL
myaucs_table=NULL

t0=Sys.time()
for (k in 1:length(myorder)){
  # Recalibration of the beta coefficients on the training set
  mymodel_recalib=glm(y_train~x_train[,myorder[1:k],drop=FALSE], family="binomial")
  # Prediction using a logistic model with recalibrated beta coefficients
  mymodel=glm(y_test~offset(x_test[,myorder[1:k],drop=FALSE]%*%matrix(coef(mymodel_recalib)[-1], ncol=1)), family="binomial")
  myroc=roc(response=y_test, predictor=mymodel$fitted.values)
  myaucs=c(myaucs, myroc$auc)
  myaucs_table=rbind(myaucs_table, formatC(as.numeric(ci.auc(myroc)), format="f", digits=4))
}
t1=Sys.time()
print(t1-t0)

rownames(myaucs_table)=myorder
colnames(myaucs_table)=c("li","auc","ui")
saveRDS(myaucs_table, paste0("../Results/lasso_forced/auc_recalib_beta_",arr,".rds"))

# Misclassification rates
mr_controls=mr_cases=NULL
thr_list=seq(0.1,0.9,by=0.1)
for (thr in thr_list){
  predicted_cat=ifelse(mymodel$fitted.values[y_test==0]>=thr, yes=1, no=0)
  mr_controls=c(mr_controls, sum(predicted_cat==1)/length(predicted_cat))
  predicted_cat=ifelse(mymodel$fitted.values[y_test==1]>=thr, yes=1, no=0)
  mr_cases=c(mr_cases, sum(predicted_cat==0)/length(predicted_cat))
}
saveRDS(mr_controls, paste0("../Results/lasso_forced/mr_controls_",arr,".rds"))
saveRDS(mr_cases, paste0("../Results/lasso_forced/mr_cases_",arr,".rds"))
