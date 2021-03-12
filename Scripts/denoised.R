
var_name <- colnames(mul_data)[7]
dim(mul_data)
view(multi.0[1:10, ])
X <- data.frame(model.matrix(~ employment, multi.0[,-1]))

mygrep=grep(paste0("^","employment"),colnames(X))
res=NULL
for (j in mygrep){
  model = glm(X[,j]~ age_baseline + sex + bmi,data = multi.0,
              family="binomial")
  res <- data.frame(cbind(res,model$residuals))
  
}
  
  

foo = function(i, dat, smoking = FALSE){
  set.seed(101)
  var_name <- colnames(dat)[i]
  confound <- c("age_baseline","sex", "bmi")  # Change confounders accordingly
  if(smoking == TRUE){
    confound <- append(confound,"smoking")
  }
  if(is.numeric(dat[,i])){
    model <- lm(as.formula(paste(var_name,paste(confound,collapse="+"),sep="~")),data=dat)
    res <- model$residuals
  } else {
    X <- data.frame(model.matrix(~., dat))[,-1]
    # Find all columns with var_name
    mygrep=grep(paste0(var_name),colnames(X))
    res=NULL
    for (j in mygrep){
      model = glm(as.formula(paste("X[,j]",paste(confound,collapse="+"),sep="~")),data=dat,
                  family="binomial")
      res <- data.frame(cbind(res, model$residuals))
    }
  }
  return(res)
}

t0=Sys.time()
test <- sapply(7:8, foo, dat = mul_data)
t1=Sys.time()
print(t1-t0)

test[[3]]
