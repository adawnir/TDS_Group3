# stratified analysis by subtype: denoise
# ines, march 15 

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(tidyverse)

covar=readRDS("../Results/covar_models.rds")
biomarker_imp=readRDS("../Results/biomarker_imp_master.rds")
case_control=readRDS("../Results/case_control.rds")
subtype=readRDS("../Results/lung_subtype.rds")

covar = covar %>%
  select((eid:sex), bmi,smoking,(ethnic:hh_income),(phys_score:alcohol),
         (mat_smoke:autoimmune))


mydata=inner_join(case_control,covar,by="eid") %>%
  right_join(biomarker_imp, by="eid") %>%
  na.omit() 
  
mydata=left_join(mydata,subtype, by="eid") %>%
  select(-eid)

# get rid of all bladder cases & recode case control status
lung_data=mydata[mydata$case_status!="bladder",]
lung_data$case_status = as.factor(ifelse(as.character(lung_data$case_status) == "control",0,1))

# separate by subtype
subtypes <- levels(subtype$subtype)
insitu.lung=subset(lung_data, subtype==subtypes[1] | is.na(subtype))
insitu.trachea=subset(lung_data, subtype==subtypes[2] | is.na(subtype)) # no trachea cases
mal.trachea=subset(lung_data, subtype==subtypes[3] | is.na(subtype))
mal.lung=subset(lung_data, subtype==subtypes[4] | is.na(subtype))
mal.lower=subset(lung_data, subtype==subtypes[5] | is.na(subtype))
mal.main=subset(lung_data, subtype==subtypes[6] | is.na(subtype))
mal.middle=subset(lung_data, subtype==subtypes[7] | is.na(subtype))
mal.overlap=subset(lung_data, subtype==subtypes[8] | is.na(subtype))
mal.upper=subset(lung_data, subtype==subtypes[9] | is.na(subtype))

insitu.lung=select(insitu.lung,-77)
insitu.trachea=select(insitu.trachea,-77)
mal.trachea=select(mal.trachea,-77)
mal.lung=select(mal.lung,-77)
mal.lower=select(mal.lower,-77)
mal.main=select(mal.main,-77)
mal.middle=select(mal.middle,-77)
mal.overlap=select(mal.overlap,-77)
mal.upper=select(mal.upper,-77)

### Make two data sets ----
### Denoised for age, sex and bmi = 1; Denoised for age, sex, bmi and smoking = 2
## Denoise data
foo = function(i, dat, smoking = FALSE){
  var_name=colnames(dat)[i]
  print(var_name)
  confound=c("age_baseline","sex", "bmi")  # Change confounders accordingly
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
insitu.lung.1=sapply(5:ncol(insitu.lung), foo, dat=insitu.lung)
insitu.lung.1=do.call("cbind",insitu.lung.1)
insitu.lung.2=sapply(6:ncol(insitu.lung), foo, dat=insitu.lung, smoking=T)
insitu.lung.2=do.call("cbind",insitu.lung.2)

insitu.trachea.1=sapply(5:ncol(insitu.trachea), foo, dat=insitu.trachea)
insitu.trachea.1=do.call("cbind",insitu.trachea.1)
insitu.trachea.2=sapply(6:ncol(insitu.trachea), foo, dat=insitu.trachea, smoking=T)
insitu.trachea.2=do.call("cbind",insitu.trachea.2)

mal.trachea.1=sapply(5:ncol(mal.trachea), foo, dat=mal.trachea)
mal.trachea.1=do.call("cbind",mal.trachea.1)
mal.trachea.2=sapply(6:ncol(mal.trachea), foo, dat=mal.trachea, smoking=T)
mal.trachea.2=do.call("cbind",mal.trachea.2)

mal.lung.1=sapply(5:ncol(mal.lung), foo, dat=mal.lung)
mal.lung.1=do.call("cbind",mal.lung.1)
mal.lung.2=sapply(6:ncol(mal.lung), foo, dat=mal.lung, smoking=T)
mal.lung.2=do.call("cbind",mal.lung.2)

mal.lower.1=sapply(5:ncol(mal.lower), foo, dat=mal.lower)
mal.lower.1=do.call("cbind",mal.lower.1)
mal.lower.2=sapply(6:ncol(mal.lower), foo, dat=mal.lower, smoking=T)
mal.lower.2=do.call("cbind",mal.lower.2)

mal.main.1=sapply(5:ncol(mal.main), foo, dat=mal.main)
mal.main.1=do.call("cbind",mal.main.1)
mal.main.2=sapply(6:ncol(mal.main), foo, dat=mal.main, smoking=T)
mal.main.2=do.call("cbind",mal.main.2)

mal.middle.1=sapply(5:ncol(mal.middle), foo, dat=mal.middle)
mal.middle.1=do.call("cbind",mal.middle.1)
mal.middle.2=sapply(6:ncol(mal.middle), foo, dat=mal.middle, smoking=T)
mal.middle.2=do.call("cbind",mal.middle.2)

mal.overlap.1=sapply(5:ncol(mal.overlap), foo, dat=mal.overlap)
mal.overlap.1=do.call("cbind",mal.overlap.1)
mal.overlap.2=sapply(6:ncol(mal.overlap), foo, dat=mal.overlap, smoking=T)
mal.overlap.2=do.call("cbind",mal.overlap.2)

mal.upper.1=sapply(5:ncol(mal.upper), foo, dat=mal.upper)
mal.upper.1=do.call("cbind",mal.upper.1)
mal.upper.2=sapply(6:ncol(mal.upper), foo, dat=mal.upper, smoking=T)
mal.upper.2=do.call("cbind",mal.upper.2)

# getting a lot of errors here - before i go further, does it make sense to denoise with so few cases ?
## One hot encoding for non-denoised data
tmp=as.data.frame(model.matrix(~.,lung.0[,-1])[,-1])
lung.0=cbind(lung.0$case_status,tmp)
colnames(lung.0)[1]="case_status"

tmp=as.data.frame(model.matrix(~.,bladder.0[,-1])[,-1])
bladder.0=cbind(bladder.0$case_status,tmp)
colnames(bladder.0)[1]="case_status"

# Rename columns and add Y
lung.1=cbind(lung.0$case_status,as.data.frame(lung.1))
colnames(lung.1)=colnames(lung.0)[c(1,9:ncol(lung.0))]
lung.2=cbind(lung.0$case_status,as.data.frame(lung.2))
colnames(lung.2)=colnames(lung.0)[c(1,13:ncol(lung.0))]

bladder.1=cbind(bladder.0$case_status,as.data.frame(bladder.1))
colnames(bladder.1)=colnames(bladder.0)[c(1,9:ncol(bladder.0))]
bladder.2=cbind(bladder.0$case_status,as.data.frame(bladder.2))
colnames(bladder.2)=colnames(bladder.0)[c(1,13:ncol(bladder.0))]

# Save data sets
ifelse(dir.exists("../Results/denoised.site"),"",dir.create("../Results/denoised.site"))
saveRDS(lung.0,"../Results/denoised.site/lung.0_denoised.rds")
saveRDS(lung.1,"../Results/denoised.site/lung.1_denoised.rds")
saveRDS(lung.2,"../Results/denoised.site/lung.2_denoised.rds")
saveRDS(bladder.0,"../Results/denoised.site/bladder.0_denoised.rds")
saveRDS(bladder.1,"../Results/denoised.site/bladder.1_denoised.rds")
saveRDS(bladder.2,"../Results/denoised.site/bladder.2_denoised.rds")