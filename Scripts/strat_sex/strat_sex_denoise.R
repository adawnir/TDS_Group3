### TDS Project -- Stratified analysis by sex: Creating multivariate data set and denoise
## Programme created by Rin Wada on 15 March

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(tidyverse)

covar=readRDS("../Results/covar_models.rds")
biomarker_imp=readRDS("../Results/biomarker_imp_master.rds")
case_control=readRDS("../Results/case_control.rds")

covar = covar %>%
  select((eid:sex), bmi,smoking,(ethnic:hh_income),(phys_score:alcohol),
         (mat_smoke:autoimmune))

case_control=case_control %>% select(eid,case_status)

mydata=inner_join(case_control,covar,by="eid") %>%
  right_join(biomarker_imp, by="eid") %>%
  na.omit() %>%
  select(-eid)

# Separate by case
lung_data=mydata[mydata$case_status!="bladder",]
bladder_data=mydata[mydata$case_status!="lung",]
# Recode case/control status
lung_data$case_status = as.factor(ifelse(as.character(lung_data$case_status) == "control",0,1))
bladder_data$case_status = as.factor(ifelse(as.character(bladder_data$case_status) == "control",0,1))

# Separate by sex
lung.f=lung_data[lung_data$sex=="Female",]
lung.m=lung_data[lung_data$sex=="Male",]
bladder.f=bladder_data[bladder_data$sex=="Female",]
bladder.m=bladder_data[bladder_data$sex=="Male",]

# toy
#set.seed(100)
#sub=createDataPartition(lung.f$case_status, p=0.1, list=F)
#lung.f=lung.f[sub,]

### Make two data sets ----
### Denoised for age, sex and bmi = 1; Denoised for age, sex, bmi and smoking = 2
## Denoise data
foo = function(i, dat, smoking = FALSE){
  var_name=colnames(dat)[i]
  print(var_name)
  confound=c("age_baseline", "bmi")  # Change confounders accordingly
  if(smoking==TRUE){
    confound=append(confound,"smoking")
  }
  if(is.numeric(dat[,i])){
    model = lm(as.formula(paste(var_name,paste(confound,collapse="+"),sep="~")),data=dat)
    res=model$residuals
  } else {
    X=as.data.frame(model.matrix(~., dat[,-1])[,-1])
    # Find all columns with var_name
    mygrep=grep(paste0("^",var_name),colnames(X))
    res=NULL
    for (j in mygrep){
      model = glm(as.formula(paste("X[,j]",paste(confound,collapse="+"),sep="~")),data=dat,
                  family="binomial")
      res=cbind(res,model$residuals)
    }
  }
  return(res)
}

## Run denoise function
lung.f.1=sapply(5:ncol(lung.f), foo, dat=lung.f)
lung.f.1=do.call("cbind",lung.f.1)
lung.f.2=sapply(6:ncol(lung.f), foo, dat=lung.f, smoking=T)
lung.f.2=do.call("cbind",lung.f.2)
lung.m.1=sapply(5:ncol(lung.m), foo, dat=lung.m)
lung.m.1=do.call("cbind",lung.m.1)
lung.m.2=sapply(6:ncol(lung.m), foo, dat=lung.m, smoking=T)
lung.m.2=do.call("cbind",lung.m.2)

bladder.f.1=sapply(5:ncol(bladder.f), foo, dat=bladder.f)
bladder.f.1=do.call("cbind",bladder.f.1)
bladder.f.2=sapply(6:ncol(bladder.f), foo, dat=bladder.f, smoking=T)
bladder.f.2=do.call("cbind",bladder.f.2)
bladder.m.1=sapply(5:ncol(bladder.m), foo, dat=bladder.m)
bladder.m.1=do.call("cbind",bladder.m.1)
bladder.m.2=sapply(6:ncol(bladder.m), foo, dat=bladder.m, smoking=T)
bladder.m.2=do.call("cbind",bladder.m.2)

## Get one-hot encoding names
tmp=as.data.frame(model.matrix(~.,lung.f[,-1])[,-1])
mynames=c("case_status",colnames(tmp))

# Rename columns and add Y
lung.f.1=cbind(lung.f$case_status,as.data.frame(lung.f.1))
colnames(lung.f.1)=mynames[c(1,9:length(mynames))]
lung.f.2=cbind(lung.f$case_status,as.data.frame(lung.f.2))
colnames(lung.f.2)=mynames[c(1,13:length(mynames))]
lung.m.1=cbind(lung.m$case_status,as.data.frame(lung.m.1))
colnames(lung.m.1)=mynames[c(1,9:length(mynames))]
lung.m.2=cbind(lung.m$case_status,as.data.frame(lung.m.2))
colnames(lung.m.2)=mynames[c(1,13:length(mynames))]

bladder.f.1=cbind(bladder.f$case_status,as.data.frame(bladder.f.1))
colnames(bladder.f.1)=mynames[c(1,9:length(mynames))]
bladder.f.2=cbind(bladder.f$case_status,as.data.frame(bladder.f.2))
colnames(bladder.f.2)=mynames[c(1,13:length(mynames))]
bladder.m.1=cbind(bladder.m$case_status,as.data.frame(bladder.m.1))
colnames(bladder.m.1)=mynames[c(1,9:length(mynames))]
bladder.m.2=cbind(bladder.m$case_status,as.data.frame(bladder.m.2))
colnames(bladder.m.2)=mynames[c(1,13:length(mynames))]

arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("f","m"),each=2),".",1:2)
ifelse(dir.exists("../Results/strat_sex_lasso"),"",dir.create("../Results/strat_sex_lasso"))
for (i in 1:length(arr)) {
  saveRDS(eval(parse(text=arr[i])),paste0("../Results/strat_sex_lasso/",arr[i],"_denoised.rds"))
}