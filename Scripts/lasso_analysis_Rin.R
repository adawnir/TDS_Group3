### TDS Project -- LASSO Logistic Regression Visulisation
## Programme created by Rin Wada on 25 Feb 2021

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

# Load results
setwd("/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/")
ridge=readRDS("../Results/ridge_lung_model_est.rds")
lasso1=readRDS("../Results/lasso_lung_model1_est.rds")
lasso2=readRDS("../Results/lasso_lung_model2_est.rds")

# Loading packages
library(RColorBrewer)
beta=cbind(lasso1[-1,1], lasso2[-1,1])
beta=beta[rowSums(beta)!=0,]
OR=cbind(beta,rep(NA, 20), rep(NA, 20))
OR=as.vector(t(exp(OR)))
OR=OR[-c(length(OR)-1, length(OR))]

MyPal = brewer.pal("Paired", n = 12)

par(mar=c(6,5,3,3))
plot(OR, col=c(MyPal[1], MyPal[2], NA, NA), xaxt="n",
     ylab="Odds Ratios",  type="h", lwd=3, xlab="")
axis(1, at=seq(1.5, 20*5, by=5), labels = rownames(lasso1[,1]), las=2)
legend("topleft", lty=1, lwd=3, col=c(MyPal[1], MyPal[2]), 
       legend = c("Lasso without smoking confounder", "Lass with smoking confounder"))

ridge_bladder_model1_est=readRDS("../Results/lasso_lung_model_est.rds")
lasso_bladder_model1_est=readRDS("../Results/lasso_lung_model1_est.rds")
lasso_bladder_model2_est=readRDS("../Results/lasso_lung_model2_est.rds")

bladder=inner_joing(ridge_bladder_model1_est,lasso_bladder_model1_est,lasso_bladder_model2_est)




