
### TDS Project -- denoise data for adjust confounders

library(dplyr)
install.packages("sure")
library(MASS)
library(sure)

##### load data---
covar_m <- readRDS("../Results/covar_models.rds")
dim(covar_m)    # 452753     48

cc <- readRDS("../Results/case_control.rds")
dim(cc)     # 452753     10
table(cc$case_status)

biomk_im <- readRDS("../Results/biomarker_imp_master.rds")
dim(biomk_im)    # 387926     29


##### merge three datasets---
cc <- cc[, which(names(cc) %in% c("eid", "case_status"))]
mul_data <- inner_join(cc, covar_m, by="eid") %>%
  right_join(biomk_im, by="eid") %>% na.omit()
dim(mul_data)   # 232374     77
sum(is.na(mul_data))
table(mul_data$case_status)

saveRDS(mul_data, "../Results/PLS_data.rds")

##### data type checking
str(mul_data)


##### denoised---
##### extract the residuals from series of linear models 
##### regressing each of predictors as the outcome, with confounders as predictors

myData <- mul_data

### model 1: age, gender, BMI (outcome: 75-3 = 72)
confound.1 <- myData[,which(names(myData) %in% c("age_baseline", "sex", "bmi"))]
myData.1 <- myData[,-which(names(myData) %in% c("age_baseline", "sex", "bmi"))]
myData.1 <- cbind(confound.1, myData.1)
  

## Make empty data.frame
denoised.m1 = NULL

## obtain residuals
t0=Sys.time()
for (i in 6:77) {
  set.seed(101)
  var_name=colnames(myData.1)[i]
  conf <- paste0("age_baseline+", "sex+", "bmi")
  if(is.numeric(myData.1[,i])){
    model = lm(as.formula(paste(var_name, conf, sep="~")), data = myData.1)
    res=model$residuals
  } else if(length(levels(myData.1[,i])) == 2) {
    model = glm(as.formula(paste(var_name, conf, sep="~")), data = myData.1, family="binomial")
    res=model$residuals
  }
  else {
    model = polr(as.formula(paste(var_name, conf, sep="~")), data = myData.1,  Hess=TRUE)
    res=resids(model)
  } 
  denoised.m1  <- cbind(denoised.m1, res)
}
t1=Sys.time()
print(t1-t0)

colnames(denoised.m1) = colnames(myData.1)[6:77]
sum(is.na(denoised.m1))
dim(denoised.m1)    # 232374     72
denoised.m1 <- cbind(myData.1[,c(4,5)], denoised.m1)


## output data
saveRDS(denoised.m1, "../Results/denoised_1.rds")


## obtain residuals
foo=function(i,dat){
  set.seed(101)
  var_name=colnames(dat)[i]
  conf <- paste0("age_baseline+", "sex+", "bmi")
  if(is.numeric(dat[,i])){
    model = lm(as.formula(paste(var_name, conf, sep="~")), data = dat)
    res=model$residuals
  } else if(length(levels(dat[,i])) == 2) {
    model = glm(as.formula(paste(var_name, conf, sep="~")), data = dat, family="binomial")
    res=model$residuals
  }
  else {
    model = polr(as.formula(paste(var_name, conf, sep="~")), data = dat,  Hess=TRUE)
    res=resids(model)
  } 
  return(res)
}


resid.1 <- sapply(6:80, foo, dat=myData.1)
resid.1 <- data.frame(resid.1)
colnames(resid.1) <- colnames(myData.1)[6:80]
resid.1 <- cbind(myData.1[,c(4,5)], resid.1)




### model 2: age, gender, BMI + smoking status (outcome: 75-4 = 71)
confound.2 <- myData[,which(names(myData) %in% c("age_baseline", "sex", "bmi", "smoking"))]
myData.2 <- myData[,-which(names(myData) %in% c("age_baseline", "sex", "bmi", "smoking"))]
myData.2 <- cbind(confound.2, myData.2)


## Make empty data.frame
res.2 <- matrix(0, nrow=nrow(myData), ncol=74) %>% data.frame()
colnames(res.2) <- colnames(myData.2)[7:80]

## obtain residuals
foo.2=function(i,dat){
  set.seed(101)
  var_name=colnames(dat)[i]
  conf <- paste0("age_baseline+", "sex+", "bmi+", "smoking")
  if(is.numeric(dat[,i])){
    model = lm(as.formula(paste(var_name, conf, sep="~")), data = dat)
    res=model$residuals
  } else if(length(levels(dat[,i])) == 2) {
    model = glm(as.formula(paste(var_name, conf, sep="~")), data = dat, family="binomial")
    res=model$residuals
  }
  else {
    model = polr(as.formula(paste(var_name, conf, sep="~")), data = dat,  Hess=TRUE)
    res=resids(model)
  } 
  return(res)
}


t0=Sys.time()
resid.2 <- sapply(7:80, foo.2, dat=myData.2)
t1=Sys.time()
print(t1-t0)

resid.2 <- data.frame(resid.2)
colnames(resid.2) <- colnames(myData.2)[7:80]
resid.2 <- cbind(myData.2[,c(5,6)], resid.2)


## Make empty data.frame

p.2 <- matrix(0, nrow=nrow(myData), ncol=74) %>% data.frame()
colnames(p.2) <- colnames(myData.2)[7:80]

## obtain residuals
t0=Sys.time()
for (i in 7:15) {
  set.seed(101)
  var_na=colnames(myData.2)[i]
  conf <- paste0("age_baseline+", "sex+", "bmi+", "smoking")
  if(is.numeric(myData.2[,i])){
    model = lm(as.formula(paste(var_na, conf, sep="~")), data = myData.2)
    res=model$residuals
  } else if(length(levels(myData.2[,i])) == 2) {
    model = glm(as.formula(paste(var_na, conf, sep="~")), data = myData.2, family="binomial")
    res=model$residuals
  }
  else {
    model = polr(as.formula(paste(var_na, conf, sep="~")), data = myData.2,  Hess=TRUE)
    res=resids(model)
  } 
  p.2[, i-6] <- res
}
t1=Sys.time()
print(t1-t0)


## output data
saveRDS(resid.2, "../../TDS_Group3/Results/denoised_2.rds")

