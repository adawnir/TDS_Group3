### TDS Project -- Stratified analysis by sex: Stability selection LASSO Logistic Regression ARRAY JOB (RUN ON HPC)
## Programme created by Rin Wada on 15 March
#Edited by Fergal for education stratification

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
arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("h","l"),each=2),".",1:2)[m] ### Change name for other stratification
dat=readRDS(paste0("../Results/strat_edu_denoised/",arr,"_denoised.rds")) ### Change path for other stratification

## Make data set
y=dat$case_status
x=as.matrix(dat[,-1])
set.seed(100)
train=createDataPartition(y,p=0.8,list=F)
y_train=y[train]
x_train=x[train,]
y_test=y[-train]
x_test=x[-train, ]

## Create directories
ifelse(dir.exists("../Figures/strat_edu_lasso"),"",dir.create("../Figures/strat_edu_lasso")) ### Change path for other stratification
ifelse(dir.exists("../Results/strat_edu_lasso"),"",dir.create("../Results/strat_edu_lasso")) ### Change path for other stratification

## Running stability selection
out=CalibrateRegression(xdata=x_train, ydata=y_train, K=100, tau=0.5, verbose=FALSE,
                        family="binomial")
# Save plot
pdf(paste0("../Figures/strat_edu_lasso/out_",arr,".pdf"), height=5, width=12) ### Change path for other stratification
CalibrationPlot(out)
dev.off()

# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out)[1]

# Computing average beta coefficients from models with calibrated lambda
average_beta=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})

# Calibrated selection proportions 
selprop=out$selprop[hat_lambda_id,]

# Save
saveRDS(out, paste0("../Results/strat_edu_lasso/out_",arr,".rds")) ### Change path for other stratification
saveRDS(average_beta, paste0("../Results/strat_edu_lasso/average_beta_",arr,".rds")) ### Change path for other stratification
saveRDS(selprop, paste0("../Results/strat_edu_lasso/selprop_",arr,".rds")) ### Change path for other stratification

# Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
selprop_nonzero=selprop[selprop>0]
myorder=names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
myaucs=NULL
myaucs_table=NULL

calib=sum(CalibratedStableRegression(out)) # Number of selected variables

t0=Sys.time()
for (k in 1:length(myorder)){
  # Recalibration of the beta coefficients on the training set
  mymodel_recalib=glm(y_train~x_train[,myorder[1:k],drop=FALSE], family="binomial")
  # Prediction using a logistic model with recalibrated beta coefficients
  mymodel=glm(y_test~offset(x_test[,myorder[1:k],drop=FALSE]%*%matrix(coef(mymodel_recalib)[-1], ncol=1)), family="binomial")
  myroc=roc(response=y_test, predictor=mymodel$fitted.values)
  if(k==calib){
    saveRDS(myroc, paste0("../Results/strat_edu_lasso/roc_",arr,".rds")) ### Change path for other stratification
  }
  myaucs=c(myaucs, myroc$auc)
  myaucs_table=rbind(myaucs_table, formatC(as.numeric(ci.auc(myroc)), format="f", digits=4))
}
t1=Sys.time()
print(t1-t0)
rownames(myaucs_table)=myorder
colnames(myaucs_table)=c("li","auc","ui")
saveRDS(myaucs_table, paste0("../Results/strat_edu_lasso/auc_",arr,".rds")) ### Change path for other stratification
