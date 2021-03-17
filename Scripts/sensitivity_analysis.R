# Sensitivity analysis
# Fergal

# 2 cancers (lung and bladder)
# 2 stratifications (time to diag. and age at diag)
# 2 comparisons (medians and quartiles)
# 2 models (base and with smoking)
# 2 further comparisons (vs full population and vs upper median/quartile)
# 2 plots (ORs and p values)
# 64 plots total

# Firstly, lets create all the data we're going to need
# Each time we will create data for both cancers and both models (base and with smoking)
# So need to make data for each stratification
# And for each stratification need to make data for medians (> and <) and quartiles (<Q1 and >Q3)
# And for all of these we need p values AND ORs with CIs
# We also need the full population data
# = 18 separate dataset creations we can then combine

# Following code taken from master_univar.R

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(tidyverse)
library(parallel)

# Load new datasets
biomarkers=readRDS("../Results/biomarker_imp_master.rds")
covar=readRDS("../Results/covar_models.rds")
cc=readRDS("../Results/case_control.rds")

# Restructure data - case/control status in first column
colnames(covar)
covar = covar %>%
  select((eid:sex), bmi,smoking,(ethnic:hh_income),(phys_score:alcohol),
         (mat_smoke:autoimmune))

mydata=cc %>% select(eid, case_status) %>%
  left_join(covar, by="eid") %>%
  left_join(biomarkers, by="eid") %>%
  select(-eid)

colnames(mydata)
# Scale biomarkers and continuous environmental variables
# (all continuous variables that are not composite scores)
mydata[c(27:32,49:76)]=scale(mydata[c(27:32,49:76)])

summary(mydata) # check mean = 0

str(mydata) # Ensure categorical variables are coded as factors and continuous numerical

# Separate by case
lung_data=mydata[mydata$case_status!="bladder",]
bladder_data=mydata[mydata$case_status!="lung",]
# Recode case/control status
lung_data$case_status = as.factor(ifelse(as.character(lung_data$case_status) == "control",0,1))
bladder_data$case_status = as.factor(ifelse(as.character(bladder_data$case_status) == "control",0,1))

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

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("lung_data", "foo"))
lung_res.1=parLapply(cl=cl, 5:ncol(lung_data), foo, dat=lung_data)
names(lung_res.1)=colnames(lung_data)[5:ncol(lung_data)]
lung_res.2=parLapply(cl=cl, 6:ncol(lung_data), foo, dat=lung_data, smoking=T)
names(lung_res.2)=colnames(lung_data)[6:ncol(lung_data)]
stopCluster(cl)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("bladder_data", "foo"))
bladder_res.1=parLapply(cl=cl, 5:ncol(bladder_data), foo, dat=bladder_data)
names(bladder_res.1)=colnames(bladder_data)[5:ncol(bladder_data)]
bladder_res.2=parLapply(cl=cl, 6:ncol(bladder_data), foo, dat=bladder_data, smoking=T)
names(bladder_res.2)=colnames(bladder_data)[6:ncol(bladder_data)]
stopCluster(cl)


### Get data for ORs and CIs
# Create empty rows for smoking variable
smokingNA=matrix(NA, nrow=4, ncol=4)
rownames(smokingNA)=rownames(lung_res.1[[1]]$glm_output)

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
ORs_CIs_fullpop=cbind(foo(lung_res.1),foo(lung_res.2, smoking = T),
             foo(bladder_res.1),foo(bladder_res.2, smoking = T))
tmp=as.data.frame(model.matrix(~.,lung_data[,-1])[,-1])
mynames=colnames(tmp)
rownames(ORs_CIs_fullpop)=mynames[-c(1:7)]
ORs_CIs_fullpop <- as.data.frame(ORs_CIs_fullpop)

### Make data set for p vals ----
pvals_fullpop=cbind(do.call("rbind", lapply(lung_res.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(lung_res.2, `[[`, 2))),
                do.call("rbind", lapply(bladder_res.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(bladder_res.2, `[[`, 2))))
# Change to log scale
pvals_fullpop=-log10(pvals_fullpop)
# Change column name
colnames(pvals_fullpop)=paste0(rep(c("lung","bladder"),each=2),".",1:2)
pvals_fullpop <- as.data.frame(pvals_fullpop)

### We now have the data we need for the full population


#### Next: time to diagnosis with medians

mydata=cc %>% select(eid, case_status, time_diag_days) %>%
  left_join(covar, by="eid") %>%
  left_join(biomarkers, by="eid") %>%
  select(-eid)

colnames(mydata)
# Scale biomarkers and continuous environmental variables
# (all continuous variables that are not composite scores)
mydata[c(28:33,50:77)]=scale(mydata[c(28:33,50:77)])

summary(mydata) # check mean = 0

str(mydata) # Ensure categorical variables are coded as factors and continuous numerical

# Separate by case
lung_data=mydata[mydata$case_status!="bladder",]
bladder_data=mydata[mydata$case_status!="lung",]

#Get median diagnosis times
lungonly <- mydata[mydata$case_status == 'lung',]
bladderonly <- mydata[mydata$case_status == 'bladder',]
median_lung <- median(lungonly$time_diag_days)
median_bladder <- median(bladderonly$time_diag_days)


#Split into early diagnosis + controls and late diagnosis + controls
lungearlydiag <- lung_data[lung_data$case_status == 'control'
                           | (lung_data$case_status == 'lung' & lung_data$time_diag_days <= median_lung),]
lunglatediag <- lung_data[lung_data$case_status == 'control'
                          | (lung_data$case_status == 'lung' & lung_data$time_diag_days > median_lung),]
bladderearlydiag <- bladder_data[bladder_data$case_status == 'control'
                                 | (bladder_data$case_status == 'bladder' & bladder_data$time_diag_days <= median_bladder),]
bladderlatediag <- bladder_data[bladder_data$case_status == 'control'
                                | (bladder_data$case_status == 'bladder' & bladder_data$time_diag_days > median_bladder),]


# Recode case/control status
lungearlydiag$case_status = as.factor(ifelse(as.character(lungearlydiag$case_status) == "control",0,1))
lunglatediag$case_status = as.factor(ifelse(as.character(lunglatediag$case_status) == "control",0,1))
bladderearlydiag$case_status = as.factor(ifelse(as.character(bladderearlydiag$case_status) == "control",0,1))
bladderlatediag$case_status = as.factor(ifelse(as.character(bladderlatediag$case_status) == "control",0,1))

#Remove time to diagnosis column before doing the analysis
lungearlydiag <- lungearlydiag[-c(2)]
lunglatediag <- lunglatediag[-c(2)]
bladderearlydiag <- bladderearlydiag[-c(2)]
bladderlatediag <- bladderlatediag[-c(2)]

### Logistic regression----
# Get beta coefficients, standard errors, glm p-values and anova p-values
# Early diagnosis first
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

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("lungearlydiag", "foo"))
lung_res.1=parLapply(cl=cl, 5:ncol(lungearlydiag), foo, dat=lungearlydiag)
names(lung_res.1)=colnames(lungearlydiag)[5:ncol(lungearlydiag)]
lung_res.2=parLapply(cl=cl, 6:ncol(lungearlydiag), foo, dat=lungearlydiag, smoking=T)
names(lung_res.2)=colnames(lungearlydiag)[6:ncol(lungearlydiag)]
stopCluster(cl)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("bladderearlydiag", "foo"))
bladder_res.1=parLapply(cl=cl, 5:ncol(bladderearlydiag), foo, dat=bladderearlydiag)
names(bladder_res.1)=colnames(bladderearlydiag)[5:ncol(bladderearlydiag)]
bladder_res.2=parLapply(cl=cl, 6:ncol(bladderearlydiag), foo, dat=bladderearlydiag, smoking=T)
names(bladder_res.2)=colnames(bladderearlydiag)[6:ncol(bladderearlydiag)]
stopCluster(cl)

### Get data for ORs and CIs
# Create empty rows for smoking variable
smokingNA=matrix(NA, nrow=4, ncol=4)
rownames(smokingNA)=rownames(lung_res.1[[1]]$glm_output)

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
ORs_CIs_early_median=cbind(foo(lung_res.1),foo(lung_res.2, smoking = T),
                      foo(bladder_res.1),foo(bladder_res.2, smoking = T))
tmp=as.data.frame(model.matrix(~.,lung_data[,-1])[,-1])
mynames=colnames(tmp)
rownames(ORs_CIs_early_median)=mynames[-c(1:7)]
ORs_CIs_early_median <- as.data.frame(ORs_CIs_early_median)

### P vals
pvals_early_median=cbind(do.call("rbind", lapply(lung_res.1, `[[`, 2)),
                 c(NA, do.call("rbind", lapply(lung_res.2, `[[`, 2))),
                 do.call("rbind", lapply(bladder_res.1, `[[`, 2)),
                 c(NA, do.call("rbind", lapply(bladder_res.2, `[[`, 2))))
# Change to log scale
pvals_early_median=-log10(pvals_early_median)
# Change column name
colnames(pvals_early_median)=paste0(rep(c("lung","bladder"),each=2),".",1:2)
pvals_early_median <- as.data.frame(pvals_early_median)




#Now for late diagnosis
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

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("lunglatediag", "foo"))
lung_res.1=parLapply(cl=cl, 5:ncol(lunglatediag), foo, dat=lunglatediag)
names(lung_res.1)=colnames(lunglatediag)[5:ncol(lunglatediag)]
lung_res.2=parLapply(cl=cl, 6:ncol(lunglatediag), foo, dat=lunglatediag, smoking=T)
names(lung_res.2)=colnames(lunglatediag)[6:ncol(lunglatediag)]
stopCluster(cl)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("bladderlatediag", "foo"))
bladder_res.1=parLapply(cl=cl, 5:ncol(bladderlatediag), foo, dat=bladderlatediag)
names(bladder_res.1)=colnames(bladderlatediag)[5:ncol(bladderlatediag)]
bladder_res.2=parLapply(cl=cl, 6:ncol(bladderlatediag), foo, dat=bladderlatediag, smoking=T)
names(bladder_res.2)=colnames(bladderlatediag)[6:ncol(bladderlatediag)]
stopCluster(cl)

### Get data for ORs and CIs
# Create empty rows for smoking variable
smokingNA=matrix(NA, nrow=4, ncol=4)
rownames(smokingNA)=rownames(lung_res.1[[1]]$glm_output)

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
ORs_CIs_late_median=cbind(foo(lung_res.1),foo(lung_res.2, smoking = T),
                           foo(bladder_res.1),foo(bladder_res.2, smoking = T))
tmp=as.data.frame(model.matrix(~.,lung_data[,-1])[,-1])
mynames=colnames(tmp)
rownames(ORs_CIs_late_median)=mynames[-c(1:7)]
ORs_CIs_late_median <- as.data.frame(ORs_CIs_late_median)

### P vals
pvals_late_median=cbind(do.call("rbind", lapply(lung_res.1, `[[`, 2)),
                         c(NA, do.call("rbind", lapply(lung_res.2, `[[`, 2))),
                         do.call("rbind", lapply(bladder_res.1, `[[`, 2)),
                         c(NA, do.call("rbind", lapply(bladder_res.2, `[[`, 2))))
# Change to log scale
pvals_late_median=-log10(pvals_late_median)
# Change column name
colnames(pvals_late_median)=paste0(rep(c("lung","bladder"),each=2),".",1:2)
pvals_late_median <- as.data.frame(pvals_late_median)


#Just clocked Ines has already made some of the data
#Lets see what she's got for us

forest <- readRDS('../Results/forest_plot_agediag.rds')
forest <- as.data.frame(forest)

#Got the ORs and CIs but not the p values we needs so still need to run the univar models
#Can use her code however...

### Age at diagnosis for medians ###

mydata=cc %>% select(eid, case_status, age_diag) %>%
  left_join(covar, by="eid") %>%
  left_join(biomarkers, by="eid") %>%
  select(-eid)

colnames(mydata)
# Scale biomarkers and continuous environmental variables
# (all continuous variables that are not composite scores)
mydata[c(28:33,50:77)]=scale(mydata[c(28:33,50:77)])

summary(mydata) # check mean = 0

str(mydata) # Ensure categorical variables are coded as factors and continuous numerical

# Separate by case
lung_data=mydata[mydata$case_status!="bladder",]
bladder_data=mydata[mydata$case_status!="lung",]

#Get median diagnosis times
lungonly <- mydata[mydata$case_status == 'lung',]
bladderonly <- mydata[mydata$case_status == 'bladder',]
median_lung <- median(lungonly$age_diag)
median_bladder <- median(bladderonly$age_diag)


#Split into young diagnosis + controls and old diagnosis + controls
lungearlydiag <- lung_data[lung_data$case_status == 'control'
                           | (lung_data$case_status == 'lung' & lung_data$age_diag <= median_lung),]
lunglatediag <- lung_data[lung_data$case_status == 'control'
                          | (lung_data$case_status == 'lung' & lung_data$age_diag > median_lung),]
bladderearlydiag <- bladder_data[bladder_data$case_status == 'control'
                                 | (bladder_data$case_status == 'bladder' & bladder_data$age_diag <= median_bladder),]
bladderlatediag <- bladder_data[bladder_data$case_status == 'control'
                                | (bladder_data$case_status == 'bladder' & bladder_data$age_diag > median_bladder),]


# Recode case/control status
lungearlydiag$case_status = as.factor(ifelse(as.character(lungearlydiag$case_status) == "control",0,1))
lunglatediag$case_status = as.factor(ifelse(as.character(lunglatediag$case_status) == "control",0,1))
bladderearlydiag$case_status = as.factor(ifelse(as.character(bladderearlydiag$case_status) == "control",0,1))
bladderlatediag$case_status = as.factor(ifelse(as.character(bladderlatediag$case_status) == "control",0,1))

#Remove time to diagnosis column before doing the analysis
lungearlydiag <- lungearlydiag[-c(2)]
lunglatediag <- lunglatediag[-c(2)]
bladderearlydiag <- bladderearlydiag[-c(2)]
bladderlatediag <- bladderlatediag[-c(2)]

###univar and data creating code here###



###Quartiles for time to diagnosis

mydata=cc %>% select(eid, case_status, time_diag_days) %>%
  left_join(covar, by="eid") %>%
  left_join(biomarkers, by="eid") %>%
  select(-eid)

colnames(mydata)
# Scale biomarkers and continuous environmental variables
# (all continuous variables that are not composite scores)
mydata[c(28:33,50:77)]=scale(mydata[c(28:33,50:77)])

summary(mydata) # check mean = 0

str(mydata) # Ensure categorical variables are coded as factors and continuous numerical

# Separate by case
lung_data=mydata[mydata$case_status!="bladder",]
bladder_data=mydata[mydata$case_status!="lung",]

#Get median diagnosis times
lungonly <- mydata[mydata$case_status == 'lung',]
bladderonly <- mydata[mydata$case_status == 'bladder',]
median_lung <- median(lungonly$time_diag_days)
median_bladder <- median(bladderonly$time_diag_days)


#Split into early diagnosis + controls and late diagnosis + controls
lungearlydiag <- lung_data[lung_data$case_status == 'control'
                           | (lung_data$case_status == 'lung' & lung_data$time_diag_days <= median_lung),]
lunglatediag <- lung_data[lung_data$case_status == 'control'
                          | (lung_data$case_status == 'lung' & lung_data$time_diag_days > median_lung),]
bladderearlydiag <- bladder_data[bladder_data$case_status == 'control'
                                 | (bladder_data$case_status == 'bladder' & bladder_data$time_diag_days <= median_bladder),]
bladderlatediag <- bladder_data[bladder_data$case_status == 'control'
                                | (bladder_data$case_status == 'bladder' & bladder_data$time_diag_days > median_bladder),]


# Recode case/control status
lungearlydiag$case_status = as.factor(ifelse(as.character(lungearlydiag$case_status) == "control",0,1))
lunglatediag$case_status = as.factor(ifelse(as.character(lunglatediag$case_status) == "control",0,1))
bladderearlydiag$case_status = as.factor(ifelse(as.character(bladderearlydiag$case_status) == "control",0,1))
bladderlatediag$case_status = as.factor(ifelse(as.character(bladderlatediag$case_status) == "control",0,1))

#Remove time to diagnosis column before doing the analysis
lungearlydiag <- lungearlydiag[-c(2)]
lunglatediag <- lunglatediag[-c(2)]
bladderearlydiag <- bladderearlydiag[-c(2)]
bladderlatediag <- bladderlatediag[-c(2)]




