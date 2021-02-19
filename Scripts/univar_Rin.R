### TDS Project -- Univariate logistic regression analysis of risk factors
### Programme created by Rin Wada on 18 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

# Load datasets
demsocial_data=readRDS("../Results/dems_social.rds")
elf_data=readRDS("../Results/elf_data.rds")
environ_data=readRDS("../Results/environ_data.rds")
medical_data=readRDS("../Results/medical_data.rds")
comorbid_data=readRDS("../Results/comorbid.rds")

### Without confounding ----
# Make datasets - inner_join if number of rows are equal, right/left join if unequal
mydata=right_join(demsocial_data, elf_data, by="eid") %>%
  inner_join(environ_data) %>%
  inner_join(medical_data) %>%
  inner_join(comorbid_data)
colnames(mydata)
# Resturcutre data - case/control status in first column, confouding factors is subsequent
mydata=mydata %>% select(case_status,age,gender,BMI,ethnic,employment,townsend,householdincome,own.rent,
                         accommodation,education,case_status,maternal_smoking,breastfed,no2,nox,pm10,
                         pm2.5_absorb,pm2.5,pm2.5_10,traff_intens_near,traff_intens_major,inv_dist_major,
                         close_major_rd_load,num_med_cat,parent_COPD,parent_breast,parent_bowel,
                         parent_lung,parent_prostate,cardiovascular,hypertension,diabetes,respiratory,
                         autoimmune, cancer)
str(mydata) # Ensure categorical variables are coded as factors and continuous numerical

# Separate by case
lung_data=mydata[mydata$case_status!="bladder",]
bladder_data=mydata[mydata$case_status!="lung",]
# Recode case/control status
lung_data$case_status = as.factor(ifelse(as.character(lung_data$case_status) == "control",0,1))
bladder_data$case_status = as.factor(ifelse(as.character(bladder_data$case_status) == "control",0,1))

# Make empty table
p=matrix(0, nrow=ncol(mydata[-1]), ncol=2) %>% data.frame()
rownames(p)=colnames(mydata[-1])
colnames(p)=c("lung", "bladder")

# Models
foo=function(i,dat){
  var_name=colnames(dat)[i]
  if(is.numeric(dat[,i])){
    model = lm(as.formula(paste(var_name,"case_status",sep="~")), data = dat)
    pval=summary(model)$coefficients[2,4]
  } else {
    model = glm(as.formula(paste("case_status",var_name,sep="~")), data = dat, family="binomial")
    pval=anova(model,test = 'Chisq')$`Pr(>Chi)`[2]
  }
  return(pval)
}
library(parallel)
t0=Sys.time()
no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("lung_data", "foo"))
pvalues=parSapply(cl=cl, 2:ncol(lung_data), foo, dat=lung_data) # Change col index accordingly
stopCluster(cl)
t1=Sys.time()
print(t1-t0)
p$lung=unlist(pvalues)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("bladder_data", "foo"))
pvalues=parSapply(cl=cl, 2:ncol(bladder_data), foo, dat=bladder_data) # Change col index accordingly
stopCluster(cl)
p$bladder=unlist(pvalues)

### With confounding by age at recruitment and gender ----

# Models
foo=function(i,dat){
  var_name=colnames(dat)[i]
  pred=c("case_status","age","gender") # Change confounders accordingly
  confound=c("age","gender")  # Change confounders accordingly
  if(is.numeric(dat[,i])){
    model = lm(as.formula(paste(var_name,paste(pred, collapse="+"),sep="~")), data = dat)
    pval=summary(model)$coefficients[2,4]
  } else {
    model = glm(as.formula(paste("case_status",paste(c(var_name,confound),collapse="+"),sep="~")),
                data = dat, family="binomial")
    model0 = glm(as.formula(paste("case_status",paste(confound,collapse="+"),sep="~")),
                 data = dat[complete.cases(dat[,i]),], family="binomial")
    pval=anova(model0,model,test = 'Chisq')$`Pr(>Chi)`[2]
  }
  return(pval)
}

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("lung_data", "foo"))
pvalues=parSapply(cl=cl, 4:ncol(lung_data), foo, dat=lung_data) # Change col index accordingly
stopCluster(cl)
p$lung_confound=c(rep(NA,2),unlist(pvalues)) # Change # of NA depending on # of confounders

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("bladder_data", "foo"))
pvalues=parSapply(cl=cl, 4:ncol(bladder_data), foo, dat=bladder_data) # Change col index accordingly
stopCluster(cl)
p$bladder_confound=c(rep(NA,2),unlist(pvalues)) # Change # of NA depending on # of confounders

# Export p-values
ifelse(dir.exists("../Exports"),"",dir.create("../Exports"))
write.csv(p, file = "../Exports/univar_pval_Rin.csv") # Change file name
