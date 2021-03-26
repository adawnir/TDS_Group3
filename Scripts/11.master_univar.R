### TDS Project -- Univariate logistic regression analysis -- Parallelisation
### Programme created by Rin Wada on 26 Feb 2021 reviewed on 8 March

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

ifelse(dir.exists("../Results/univ"),"",dir.create("../Results/univ"))
saveRDS(lung_res.1, "../Results/univ/univ_lung_res_1.rds")
saveRDS(lung_res.2, "../Results/univ/univ_lung_res_2.rds")
saveRDS(bladder_res.1, "../Results/univ/univ_bladder_res_1.rds")
saveRDS(bladder_res.2, "../Results/univ/univ_bladder_res_2.rds")

### Make data for forest plot ----
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
forest=cbind(foo(lung_res.1),foo(lung_res.2, smoking = T),
             foo(bladder_res.1),foo(bladder_res.2, smoking = T))
tmp=as.data.frame(model.matrix(~.,lung_data[,-1])[,-1])
mynames=colnames(tmp)
rownames(forest)=mynames[-c(1:7)]

saveRDS(forest, "../Results/forest_plot.rds")

### Make data set for Manhattan plot ----
manhattan=cbind(do.call("rbind", lapply(lung_res.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(lung_res.2, `[[`, 2))),
                do.call("rbind", lapply(bladder_res.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(bladder_res.2, `[[`, 2))))
# Change to log scale
manhattan=-log10(manhattan)
# Change column name
colnames(manhattan)=paste0(rep(c("lung","bladder"),each=2),".",1:2)
saveRDS(manhattan, "../Results/manhattan_plot.rds")
