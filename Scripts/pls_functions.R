SparsePLS=function(x, y, lambda, family="binomial", ncomp=1){
  mybeta=matrix(NA, nrow=length(lambda), ncol=ncol(x))
  for (k in 1:length(lambda)){
    nvarx=lambda[k]
    if (family=="binomial"){
      mymodel=splsda(X=x, Y=y, ncomp=ncomp, keepX=nvarx)
    } else {
      mymodel=spls(X=x, Y=y, ncomp=ncomp, keepX=nvarx)
    }
    mybeta[k,]=mymodel$loadings$X[,1]
  }
  colnames(mybeta)=colnames(x)
  rownames(mybeta)=paste0("s",0:(length(lambda)-1))
  
  return(mybeta)
}


GroupPLS=function(x, y, lambda, family="binomial", ncomp=1, ...){
  mybeta=matrix(NA, nrow=length(lambda), ncol=ncol(x))
  for (k in 1:length(lambda)){
    nvarx=lambda[k]
    if (family=="binomial"){
      mymodel=gPLSda(X=x, Y=y, ncomp=ncomp, keepX=nvarx, ...)
    } else {
      mymodel=gPLS(X=x, Y=y, ncomp=ncomp, keepX=nvarx, ...)
    }
    mybeta[k,]=mymodel$loadings$X[,1]
  }
  colnames(mybeta)=colnames(x)
  rownames(mybeta)=paste0("s",0:(length(lambda)-1))
  
  return(mybeta)
}


SparseGroupPLS=function(x, y, lambda, family="binomial", ncomp=1, ...){
  mybeta=matrix(NA, nrow=length(lambda), ncol=ncol(x))
  for (k in 1:length(lambda)){
    nvarx=lambda[k]
    if (family=="binomial"){
      mymodel=sgPLSda(X=x, Y=y, ncomp=ncomp, keepX=nvarx, ...)
    } 
    if (family%in%c("gaussian","mvPLS")){
      mymodel=sgPLS(X=x, Y=y, ncomp=ncomp, keepX=nvarx, ...)
    }
    mybeta[k,]=mymodel$loadings$X[,1]
  }
  
  # if (family=="mgaussian"){
  #   mybeta=array(mybeta,dim=c(nrow(mybeta),ncol(mybeta),1))
  # }

  colnames(mybeta)=colnames(x)
  rownames(mybeta)=paste0("s",0:(length(lambda)-1))
  
  return(mybeta)
}


SeparateMatrix=function(mat, groups){
  mat=mat[,names(groups), drop=FALSE]
  xvar=apply(mat,2,sum)
  xgroup=sapply(split(xvar, f=groups), mean)
  xgroup=names(sort(xgroup, decreasing=TRUE))
  var_list=sort(xvar, decreasing=TRUE)
  mat=mat[,names(var_list), drop=FALSE]
  new=NULL
  for (k in 1:length(unique(groups))){
    new=cbind(new, mat[,which(groups[colnames(mat)]==xgroup[k]), drop=FALSE], rep(NA, nrow(mat)))
  }
  return(new)
}