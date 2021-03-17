#Sensitivity analysis: quartiles

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


#Time to diag, quartiles


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
q1_lung <- quantile(lungonly$time_diag_days,0.25, na.rm = TRUE)
q3_lung <- quantile(lungonly$time_diag_days,0.75, na.rm = TRUE)
q1_bladder <- quantile(bladderonly$time_diag_days,0.25, na.rm = TRUE)
q3_bladder <- quantile(bladderonly$time_diag_days,0.75, na.rm = TRUE)


#Split into early diagnosis + controls and late diagnosis + controls
lungearlydiag <- lung_data[lung_data$case_status == 'control'
                           | (lung_data$case_status == 'lung' & lung_data$time_diag_days <= q1_lung),]
lunglatediag <- lung_data[lung_data$case_status == 'control'
                          | (lung_data$case_status == 'lung' & lung_data$time_diag_days > q3_lung),]
bladderearlydiag <- bladder_data[bladder_data$case_status == 'control'
                                 | (bladder_data$case_status == 'bladder' & bladder_data$time_diag_days <= q1_bladder),]
bladderlatediag <- bladder_data[bladder_data$case_status == 'control'
                                | (bladder_data$case_status == 'bladder' & bladder_data$time_diag_days > q3_bladder),]


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
ORs_CIs_early_quartile=cbind(foo(lung_res.1),foo(lung_res.2, smoking = T),
                           foo(bladder_res.1),foo(bladder_res.2, smoking = T))
tmp=as.data.frame(model.matrix(~.,lung_data[,-1])[,-1])
mynames=colnames(tmp)
rownames(ORs_CIs_early_quartile)=mynames[-c(1:7)]
ORs_CIs_early_quartile <- as.data.frame(ORs_CIs_early_quartile)

### P vals
pvals_early_quartile=cbind(do.call("rbind", lapply(lung_res.1, `[[`, 2)),
                         c(NA, do.call("rbind", lapply(lung_res.2, `[[`, 2))),
                         do.call("rbind", lapply(bladder_res.1, `[[`, 2)),
                         c(NA, do.call("rbind", lapply(bladder_res.2, `[[`, 2))))
# Change to log scale
pvals_early_quartile=-log10(pvals_early_quartile)
# Change column name
colnames(pvals_early_quartile)=paste0(rep(c("lung","bladder"),each=2),".",1:2)
pvals_early_quartile <- as.data.frame(pvals_early_quartile)




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
ORs_CIs_late_quartile=cbind(foo(lung_res.1),foo(lung_res.2, smoking = T),
                          foo(bladder_res.1),foo(bladder_res.2, smoking = T))
tmp=as.data.frame(model.matrix(~.,lung_data[,-1])[,-1])
mynames=colnames(tmp)
rownames(ORs_CIs_late_quartile)=mynames[-c(1:7)]
ORs_CIs_late_quartile <- as.data.frame(ORs_CIs_late_quartile)

### P vals
pvals_late_quartile=cbind(do.call("rbind", lapply(lung_res.1, `[[`, 2)),
                        c(NA, do.call("rbind", lapply(lung_res.2, `[[`, 2))),
                        do.call("rbind", lapply(bladder_res.1, `[[`, 2)),
                        c(NA, do.call("rbind", lapply(bladder_res.2, `[[`, 2))))
# Change to log scale
pvals_late_quartile=-log10(pvals_late_quartile)
# Change column name
colnames(pvals_late_quartile)=paste0(rep(c("lung","bladder"),each=2),".",1:2)
pvals_late_quartile <- as.data.frame(pvals_late_quartile)





### Age at diagnosis for quartiles ###

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
q1_lung <- quantile(lungonly$age_diag,0.25, na.rm = TRUE)
q3_lung <- quantile(lungonly$age_diag,0.75, na.rm = TRUE)
q1_bladder <- quantile(bladderonly$age_diag,0.25, na.rm = TRUE)
q3_bladder <- quantile(bladderonly$age_diag,0.75, na.rm = TRUE)


#Split into young diagnosis + controls and old diagnosis + controls
lungearlydiag <- lung_data[lung_data$case_status == 'control'
                           | (lung_data$case_status == 'lung' & lung_data$age_diag <= q1_lung),]
lunglatediag <- lung_data[lung_data$case_status == 'control'
                          | (lung_data$case_status == 'lung' & lung_data$age_diag > q3_lung),]
bladderearlydiag <- bladder_data[bladder_data$case_status == 'control'
                                 | (bladder_data$case_status == 'bladder' & bladder_data$age_diag <= q1_bladder),]
bladderlatediag <- bladder_data[bladder_data$case_status == 'control'
                                | (bladder_data$case_status == 'bladder' & bladder_data$age_diag > q3_bladder),]


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



# Young diagnosis first

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
ORs_CIs_young_quartile=cbind(foo(lung_res.1),foo(lung_res.2, smoking = T),
                           foo(bladder_res.1),foo(bladder_res.2, smoking = T))
tmp=as.data.frame(model.matrix(~.,lung_data[,-1])[,-1])
mynames=colnames(tmp)
rownames(ORs_CIs_young_quartile)=mynames[-c(1:7)]
ORs_CIs_young_quartile <- as.data.frame(ORs_CIs_young_quartile)

### P vals
pvals_young_quartile=cbind(do.call("rbind", lapply(lung_res.1, `[[`, 2)),
                         c(NA, do.call("rbind", lapply(lung_res.2, `[[`, 2))),
                         do.call("rbind", lapply(bladder_res.1, `[[`, 2)),
                         c(NA, do.call("rbind", lapply(bladder_res.2, `[[`, 2))))
# Change to log scale
pvals_young_quartile=-log10(pvals_young_quartile)
# Change column name
colnames(pvals_young_quartile)=paste0(rep(c("lung","bladder"),each=2),".",1:2)
pvals_young_quartile <- as.data.frame(pvals_young_quartile)



#Now old diagnosis

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
ORs_CIs_old_quartile=cbind(foo(lung_res.1),foo(lung_res.2, smoking = T),
                         foo(bladder_res.1),foo(bladder_res.2, smoking = T))
tmp=as.data.frame(model.matrix(~.,lung_data[,-1])[,-1])
mynames=colnames(tmp)
rownames(ORs_CIs_old_quartile)=mynames[-c(1:7)]
ORs_CIs_old_quartile <- as.data.frame(ORs_CIs_old_quartile)

### P vals
pvals_old_quartile=cbind(do.call("rbind", lapply(lung_res.1, `[[`, 2)),
                       c(NA, do.call("rbind", lapply(lung_res.2, `[[`, 2))),
                       do.call("rbind", lapply(bladder_res.1, `[[`, 2)),
                       c(NA, do.call("rbind", lapply(bladder_res.2, `[[`, 2))))
# Change to log scale
pvals_old_quartile=-log10(pvals_old_quartile)
# Change column name
colnames(pvals_old_quartile)=paste0(rep(c("lung","bladder"),each=2),".",1:2)
pvals_old_quartile <- as.data.frame(pvals_old_quartile)



#Combine the datasets we have created

#First change column names
colnames(ORs_CIs_young_quartile) <- paste0('young_quartile_', colnames(ORs_CIs_young_quartile))
colnames(ORs_CIs_old_quartile) <- paste0('old_quartile_', colnames(ORs_CIs_old_quartile))

#Then join
ORs_age_quartile <- cbind(ORs_CIs_young_quartile, ORs_CIs_old_quartile)

saveRDS(ORs_age_quartile, "../Results/sensitivity/ageatdiag_quartile_ORs.rds")


#change column names
colnames(pvals_young_quartile) <- paste0('young_quartile_', colnames(pvals_young_quartile))
colnames(pvals_old_quartile) <- paste0('old_quartile_', colnames(pvals_old_quartile))

#Then join
pvals_age_quartile <- cbind(pvals_young_quartile, pvals_old_quartile)

saveRDS(pvals_age_quartile, "../Results/sensitivity/ageatdiag_quartile_pvals.rds")



#change column names
colnames(ORs_CIs_early_quartile) <- paste0('early_quartile_', colnames(ORs_CIs_early_quartile))
colnames(ORs_CIs_late_quartile) <- paste0('late_quartile_', colnames(ORs_CIs_late_quartile))

#Then join
ORs_time_quartile <- cbind(ORs_CIs_early_quartile, ORs_CIs_late_quartile)

saveRDS(ORs_time_quartile, "../Results/sensitivity/timetodiag_quartile_ORs.rds")


#change column names
colnames(pvals_early_quartile) <- paste0('early_quartile_', colnames(pvals_early_quartile))
colnames(pvals_late_quartile) <- paste0('late_quartile_', colnames(pvals_late_quartile))

#Then join
pvals_time_quartile <- cbind(pvals_early_quartile, pvals_late_quartile)

saveRDS(pvals_time_quartile, "../Results/sensitivity/timetodiag_quartile_pvals.rds")