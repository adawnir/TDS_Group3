#Sensitivity analysis stratified by time to diagnosis
#Fergal

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

#Load data
cc <- readRDS('../Results/case_control.rds')
covar <- readRDS("../Results/covar_models.rds")
biomarkers <- readRDS("../Results/biomarker_imp_master.rds")

# Restructure data - case/control status in first column
colnames(covar)
covar = covar %>%
  select((eid:sex), bmi,smoking,(ethnic:hh_income),(phys_score:alcohol),
         (mat_smoke:autoimmune))

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

saveRDS(lung_res.1, "../Results/univ/univ_lung_res_earlydiag_1.rds")
saveRDS(lung_res.2, "../Results/univ/univ_lung_res_earlydiag_2.rds")
saveRDS(bladder_res.1, "../Results/univ/univ_bladder_res_earlydiag_1.rds")
saveRDS(bladder_res.2, "../Results/univ/univ_bladder_res_earlydiag_2.rds")


#Now for late diagnosis
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

saveRDS(lung_res.1, "../Results/univ/univ_lung_res_latediag_1.rds")
saveRDS(lung_res.2, "../Results/univ/univ_lung_res_latediag_2.rds")
saveRDS(bladder_res.1, "../Results/univ/univ_bladder_res_latediag_1.rds")
saveRDS(bladder_res.2, "../Results/univ/univ_bladder_res_latediag_2.rds")