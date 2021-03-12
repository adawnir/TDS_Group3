# stratification by age at diagnosis:
# by ines gerard-ursin, march 11, wk8

# get the ORs for:
# young diagnosis vs full population (already in forest plot rds, <67 vs. full)
# younger diagnosis vs older diagnosis (needs to be generated = one group < 67 vs. >67)
# separate by median age

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)
library(parallel)

biomarkers=readRDS("../Results/biomarker_imp_master.rds")
covar=readRDS("../Results/covar_master.rds")
cc=readRDS("../Results/case_control.rds")

med_age_diag <- median(cc$age_diag, na.rm=TRUE)
# Restructure data - case/control status in first column
colnames(covar)
covar = covar %>%
  select((eid:sex), bmi,smoking,(ethnic:hh_income),(total_meat:alcohol),
         (mat_smoke:autoimmune))

mydata=cc %>% select(eid, case_status, age_diag) %>%
  left_join(covar, by="eid") %>%
  left_join(biomarkers, by="eid") %>%
  select(-eid)

colnames(mydata)
# Scale biomarkers and continuous environmental variables
# (all continuous variables that are not composite scores)
mydata[c(31:36,53:80)]=scale(mydata[c(31:36,53:80)])

summary(mydata) # check mean = 0

str(mydata) # Ensure categorical variables are coded as factors and continuous numerical

# Separate by case
lung_data=mydata[mydata$case_status!="bladder",]
bladder_data=mydata[mydata$case_status!="lung",]
# Recode case/control status
lung_data$case_status = as.factor(ifelse(as.character(lung_data$case_status) == "control",0,1)) # control is 0
bladder_data$case_status = as.factor(ifelse(as.character(bladder_data$case_status) == "control",0,1))

# Separate into age groups : case_age: young < median, old > median


lung_data$young=as.factor(ifelse(as.numeric(lung_data$case_status)==1 & 
                                   !(is.na(lung_data$age_diag)) & 
                                   lung_data$age_diag < med_age_diag, 1, 0)) # if its a case, has age_diag, and age_diag < median

lung_data$old=as.factor(ifelse(as.numeric(lung_data$case_status)==1 & 
                                 !(is.na(lung_data$age_diag)) & 
                                 lung_data$age_diag > med_age_diag, 1, 0)) # if its a case, has age_diag, and age_diag > median

lung_young <- subset(lung_data, old != 1, select=-c(young,old))
lung_old <- subset(lung_data, young != 1, select=-c(young,old))

bladder_data$young=as.factor(ifelse(bladder_data$case_status==1 & 
                                      !(is.na(bladder_data$age_diag)) & 
                                      bladder_data$age_diag < med_age_diag, 1, 0)) # if its a case, has age_diag, and age_diag < median
bladder_data$old=as.factor(ifelse(bladder_data$case_status==1 & 
                                    !(is.na(bladder_data$age_diag)) & 
                                    bladder_data$age_diag > med_age_diag, 1, 0)) # if its a case, has age_diag, and age_diag > median

bladder_young <- subset(bladder_data, old != 1, select=-c(young,old))
bladder_old <- subset(bladder_data, young != 1, select=-c(young,old))

### Logistic regression----
# Get beta coefficients, standard errors, glm p-values and anova p-values
foo = function(i, dat, smoking = FALSE){
  var_name=colnames(dat)[i]
  print(var_name)
  confound=c("age_baseline","sex", "bmi")  # Change confounders accordingly
  if(smoking==TRUE){
    confound=append(confound,"smoking")
  }
  model = glm(as.formula(paste("case_status",paste(c(var_name,confound),collapse="+"),sep="~")),
              data = dat, family="binomial")
  model0 = glm(as.formula(paste("case_status",paste(confound,collapse="+"),sep="~")),
               data = dat[complete.cases(dat[,i]),], family="binomial")
  summary_table=summary(model)$coefficients
  res = list(summary_table[grepl(paste0("^",var_name),rownames(summary_table)),-3],
             anova(model0, model, test = 'Chisq')$`Pr(>Chi)`[2])
  names(res) = c('glm_output', 'anova_output')
  return(res)
}

# export all the values for young & old datasets
no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("lung_old", "foo"))
lung_res_old.1=parLapply(cl=cl, 5:ncol(lung_old), foo, dat=lung_old)
names(lung_res_old.1)=colnames(lung_old)[5:ncol(lung_old)]
lung_res_old.2=parLapply(cl=cl, 6:ncol(lung_old), foo, dat=lung_old, smoking=T)
names(lung_res_old.2)=colnames(lung_old)[6:ncol(lung_old)]
stopCluster(cl)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("lung_young", "foo"))
lung_res_young.1=parLapply(cl=cl, 5:ncol(lung_young), foo, dat=lung_young)
names(lung_res_young.1)=colnames(lung_young)[5:ncol(lung_young)]
lung_res_young.2=parLapply(cl=cl, 6:ncol(lung_young), foo, dat=lung_young, smoking=T)
names(lung_res_young.2)=colnames(lung_young)[6:ncol(lung_young)]
stopCluster(cl)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("bladder_old", "foo"))
bladder_res_old.1=parLapply(cl=cl, 5:ncol(bladder_old), foo, dat=bladder_old)
names(bladder_res_old.1)=colnames(bladder_old)[5:ncol(bladder_old)]
bladder_res_old.2=parLapply(cl=cl, 6:ncol(bladder_old), foo, dat=bladder_old, smoking=T)
names(bladder_res_old.2)=colnames(bladder_old)[6:ncol(bladder_old)]
stopCluster(cl)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("bladder_young", "foo"))
bladder_res_young.1=parLapply(cl=cl, 5:ncol(bladder_young), foo, dat=bladder_young)
names(bladder_res_young.1)=colnames(bladder_young)[5:ncol(bladder_young)]
bladder_res_young.2=parLapply(cl=cl, 6:ncol(bladder_young), foo, dat=bladder_young, smoking=T)
names(bladder_res_young.2)=colnames(bladder_young)[6:ncol(bladder_young)]
stopCluster(cl)

ifelse(dir.exists("../Results/univ"),"",dir.create("../Results/univ"))
saveRDS(lung_res_old.1, "../Results/univ/univ_lung_res_old_1.rds")
saveRDS(lung_res_old.2, "../Results/univ/univ_lung_res_old_2.rds")
saveRDS(lung_res_young.1, "../Results/univ/univ_lung_res_young_1.rds")
saveRDS(lung_res_young.2, "../Results/univ/univ_lung_res_young_2.rds")

saveRDS(bladder_res_old.1, "../Results/univ/univ_bladder_res_old_1.rds")
saveRDS(bladder_res_old.2, "../Results/univ/univ_bladder_res_old_2.rds")
saveRDS(bladder_res_young.1, "../Results/univ/univ_bladder_res_young_1.rds")
saveRDS(bladder_res_young.2, "../Results/univ/univ_bladder_res_young_2.rds")

### Make data for forest plot ----
# Create empty rows for smoking variable
smokingNA=matrix(NA, nrow=5, ncol=4)
rownames(smokingNA)=rownames(lung_res_old.1[["smoking"]]$glm_output)

foo=function(x, smoking = FALSE){
  name=gsub("_res","",substitute(x))
  # Extract first element of nest list and rbind for each model results
  table=do.call("rbind", lapply(x, `[[`, 1))
  tmp=apply(table, 1, function(y) exp(y[1]+c(0,-1,1)*qnorm(0.975)*y[2]))
  res=t(tmp)
  res=cbind(res,-log10(table[,3]))
  if(smoking==T){
    # Add NA to model 2 tables
    res=rbind(smokingNA,res)
  }
  # Rename columns
  colnames(res)=paste0(c("or_","l95_","u95_","logp_"),name)
  return(res)
}
# cbind into one table
forest=cbind(foo(lung_res_old.1),foo(lung_res_old.2, smoking = T),
             foo(lung_res_young.1),foo(lung_res_young.2, smoking = T),
             foo(bladder_res_old.1),foo(bladder_res_old.2, smoking = T),
             foo(bladder_res_young.1),foo(bladder_res_young.2, smoking = T))
saveRDS(forest, "../Results/forest_plot_agediag.rds")
