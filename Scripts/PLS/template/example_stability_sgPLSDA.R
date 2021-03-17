rm(list=ls())

setwd("~/Dropbox/Teaching/Computational_Epidemiology/2020-21/07-practical")

LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}

LoadPackages(c("pheatmap","glasso","glmnet","igraph","pROC","sgPLS"))
source("penalisation_functions.R")
source("~/Desktop/tosend_stability/functions.R")


# Data simulation
simul=SimulateXY(n=500, pk=100, family="binomial")
x=simul$X
y=simul$Y

# Randomly generated groups
group_ind=c(10,20,50)

# Stability selection on sparse group PLS-DA
niter=10 # TO INCREASE FOR ACTUAL ANALYSES
t0=Sys.time()
alpha_list=seq(0.05,0.95,by=0.05)
stability_alpha=NULL
for (k in 1:length(alpha_list)){
  myalpha=alpha_list[k]
  tmp=CalibrateRegression(xdata=x, ydata=y, Lambda=1:(length(group_ind)+1), K=niter,
                          family="binomial", implementation="SparseGroupPLS", ind.block.x=group_ind, alpha.x=myalpha)
  assign(paste0("out_", k), tmp)
  stability_alpha=cbind(stability_alpha, tmp$S)
}
rownames(stability_alpha)=tmp$Lambda
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

# Stability score for different numbers of groups and alpha
colnames(stability_alpha)=alpha_list
pheatmap(stability_alpha, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)

# Calibrating the parameter alpha
hat_alpha_id=which.max(apply(stability_alpha,2,max))
hat_alpha=alpha_list[hat_alpha_id]

# Calibrating the number of groups and threshold in selection proportion (pi)
out=eval(parse(text=paste0("out_", hat_alpha_id)))
CalibrationPlot(out)

# Selection proportions for best alpha over different numbers of groups
mygroups=NULL
group_size=c(group_ind[1],diff(c(group_ind,ncol(x))))
for (i in 1:(length(group_size))){
  mygroups=c(mygroups,rep(i, group_size[i]))
}
names(mygroups)=colnames(x)
new=SeparateMatrix(out$selprop, groups=mygroups)
rownames(new)=1:nrow(new)
pheatmap(new, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, na_col="white")

# Extracting average loadings from sgPLS-DA calibrated by stability
myselprop=out$selprop[which.max(out$S),]
myloadings=out$Beta[which.max(out$S),,]
myloadings=apply(myloadings,1,FUN=function(x){mean(x[x!=0])})
myloadings[is.na(myloadings)]=0
myloadings=myloadings[colnames(new)]
names(myloadings)[is.na(names(myloadings))]=""
myselprop=myselprop[names(myloadings)]

# Figure with average loadings from sgPLS-DA calibrated by stability (in red if stably selected)
par(mar=c(5,5,1,1))
hat_pi=GetArgmax(out)[2]
plot(myloadings, type="h", lwd=5, las=1, col=ifelse(myselprop>=hat_pi, yes="darkred", no="grey"),
     xlab="", ylab="Average Loadings Coefficients", cex.lab=1.5, xaxt="n")
abline(v=c(0,which(names(myloadings)=="")), lty=3, col="grey")
for (k in 1:length(myloadings)){
  if (names(myloadings)[k]!=""){
    axis(side=1, at=k, labels=names(myloadings)[k], las=2, col.axis=ifelse(myselprop[k]>=hat_pi, yes="darkred", no="black"))
  }
}


