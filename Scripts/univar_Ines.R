### TDS Project -- Univariate logistic regression analysis of risk factors
### Programme created by Rin Wada on 18 Feb 2021
### Update with new dataset names by Ines on 25 Feb 2021 (no code was deleted)

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)
library(parallel)

# Load datasets: Rin 
# demsocial_data=readRDS("../Results/dems_social.rds")
# elf_data=readRDS("../Results/elf_data.rds")
# environ_data=readRDS("../Results/environ_data.rds")
# medical_data=readRDS("../Results/medical_data.rds")
# comorbid_data=readRDS("../Results/comorbid.rds")

### Without confounding ----
# Make datasets - inner_join if number of rows are equal, right/left join if unequal
# mydata=right_join(demsocial_data, elf_data, by="eid") %>%
#   inner_join(environ_data) %>%
#   inner_join(medical_data) %>%
#   inner_join(comorbid_data)
#colnames(mydata)

# Load new datasets: Ines, Feb 25

biomarkers=readRDS("../Results/biomarker_imp_master.rds")
covar=readRDS("../Results/covar_master.rds")
cc=readRDS("../Results/case_control.rds")

mydata=right_join(biomarkers, covar, by="eid") %>%
   inner_join(cc)
colnames(mydata)
paste((names(mydata)), collapse = ", ") # extract colnames

# Restructure data - case/control status in first column, confounding factors is subsequent
mydata=mydata %>% select(case_status,age_baseline,sex,bmi,smoking, ethnic, 
                         townsend,employment, education, 
                         type_accom, own_rent, num_hh, hh_income, total_meat, white_meat, 
                         red_meat, pro_meat, total_fru_veg, salt, tea, coffee, 
                         water, alcohol, mat_smoke, smoke_hh, num_walk, num_mod, 
                         num_vig, sleep, no2, nox, pm10, pm2.5_absorb, pm2.5, pm2.5_10, 
                         close_major_rd, num_med, parent_COPD, parent_diabetes, 
                         parent_hypertension, parent_stroke, parent_CHD, 
                         parent_breast, parent_bowel, parent_lung, parent_prostate, 
                         cardiovascular, hypertension, diabetes, respiratory, autoimmune,
                         Alanine.aminotransferase,Albumin, Alkaline.phosphatase, 
                         Apolipoprotein.A, Apolipoprotein.B,Aspartate.aminotransferase, 
                         C.reactive.protein, Calcium, 
                         Cholesterol, Creatinine, Cystatin.C, Direct.bilirubin, 
                         Gamma.glutamyltransferase, Glucose, Glycated.haemoglobin..HbA1c., 
                         HDL.cholesterol, IGF.1, LDL.direct, Lipoprotein.A, Phosphate, 
                         SHBG, Testosterone, Total.bilirubin, Total.protein, Triglycerides, 
                         Urate, Urea, Vitamin.D)
str(mydata) # Ensure categorical variables are coded as factors and continuous numerical

# Separate by case
lung_data=mydata[mydata$case_status!="bladder",]
bladder_data=mydata[mydata$case_status!="lung",]
# Recode case/control status
lung_data$case_status = as.factor(ifelse(as.character(lung_data$case_status) == "control",0,1))
bladder_data$case_status = as.factor(ifelse(as.character(bladder_data$case_status) == "control",0,1))

# Make empty table
p <- matrix(0, nrow=ncol(mydata)-1, ncol=4) %>% data.frame()
rownames(p) <- colnames(mydata)[2:79]
colnames(p) <- c("lung_confound", "bladder_confound", "lung_confound_smoking", "bladder_confound_smoking")

### With confounding by age, sex, bmi ----

# Models
foo=function(i,dat){
  var_name=colnames(dat)[i]
  pred=c("case_status","age_baseline","sex", "bmi") # Change confounders accordingly
  confound=c("age_baseline","sex", "bmi")  # Change confounders accordingly
  model = glm(as.formula(paste("case_status",paste(c(var_name,confound),collapse="+"),sep="~")),
                data = dat, family="binomial")
  model0 = glm(as.formula(paste("case_status",paste(confound,collapse="+"),sep="~")),
                 data = dat[complete.cases(dat[,i]),], family="binomial")
  pval=anova(model0,model,test = 'Chisq')$`Pr(>Chi)`[2]
  return(pval)
}

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("lung_data", "foo"))
pvalues=parSapply(cl=cl, 5:ncol(lung_data), foo, dat=lung_data) # Change col index accordingly
stopCluster(cl)
p$lung_confound=c(rep(NA,3),unlist(pvalues)) # Change # of NA depending on # of confounders

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("bladder_data", "foo"))
pvalues=parSapply(cl=cl, 5:ncol(bladder_data), foo, dat=bladder_data) # Change col index accordingly
stopCluster(cl)
p$bladder_confound=c(rep(NA,3),unlist(pvalues)) # Change # of NA depending on # of confounders

### With confounding by age, sex, bmi, smoking ----

foo=function(i,dat){
  var_name=colnames(dat)[i]
  pred=c("case_status","age_baseline","sex", "bmi", "smoking") # Change confounders accordingly
  confound=c("age_baseline","sex", "bmi", "smoking")  # Change confounders accordingly
  model = glm(as.formula(paste("case_status",paste(c(var_name,confound),collapse="+"),sep="~")),
                data = dat, family="binomial")
  model0 = glm(as.formula(paste("case_status",paste(confound,collapse="+"),sep="~")),
                 data = dat[complete.cases(dat[,i]),], family="binomial")
  pval=anova(model0,model,test = 'Chisq')$`Pr(>Chi)`[2]
  return(pval)
}

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("lung_data", "foo"))
pvalues=parSapply(cl=cl, 6:ncol(lung_data), foo, dat=lung_data) # Change col index accordingly
stopCluster(cl)
p$lung_confound_smoking=c(rep(NA,4),unlist(pvalues)) # Change # of NA depending on # of confounders

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("bladder_data", "foo"))
pvalues=parSapply(cl=cl, 6:ncol(bladder_data), foo, dat=bladder_data) # Change col index accordingly
stopCluster(cl)
p$bladder_confound_smoking=c(rep(NA,4),unlist(pvalues)) # Change # of NA depending on # of confounders


# Export p-values
write.csv(p, file = "../Exports/univar_pval_Ines.csv") # Change file name

### P values of covariate categories ----

fit <- glm(case_status~mat_smoke, data=lung_data, family="binomial"(link=logit))
summary(fit)

fit2 <- glm(case_status~mat_smoke, data=bladder_data, family="binomial"(link=logit))
summary(fit2)


