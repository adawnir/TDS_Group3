### TDS Project -- Stratified analysis by sex: Univariate logistic regression
### Programme created by Rin Wada on 15 March

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

# Separate by sex
lung.f=lung_data[lung_data$sex=="Female",]
lung.m=lung_data[lung_data$sex=="Male",]
bladder.f=bladder_data[bladder_data$sex=="Female",]
bladder.m=bladder_data[bladder_data$sex=="Male",]

### Logistic regression----
# Get beta coefficients, standard errors, glm p-values and anova p-values
foo = function(i, dat, smoking = FALSE){
  var_name=colnames(dat)[i]
  print(var_name)
  confound=c("age_baseline", "bmi")  # Change confounders accordingly
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
clusterExport(cl, c("lung.f","lung.m","bladder.f","bladder.m", "foo"))
lung.f.1=parLapply(cl=cl, 5:ncol(lung.f), foo, dat=lung.f)
names(lung.f.1)=colnames(lung.f)[5:ncol(lung.f)]
lung.f.2=parLapply(cl=cl, 6:ncol(lung.f), foo, dat=lung.f, smoking=T)
names(lung.f.2)=colnames(lung.f)[6:ncol(lung.f)]
lung.m.1=parLapply(cl=cl, 5:ncol(lung.m), foo, dat=lung.m)
names(lung.m.1)=colnames(lung.m)[5:ncol(lung.m)]
lung.m.2=parLapply(cl=cl, 6:ncol(lung.m), foo, dat=lung.m, smoking=T)
names(lung.m.2)=colnames(lung.m)[6:ncol(lung.m)]
bladder.f.1=parLapply(cl=cl, 5:ncol(bladder.f), foo, dat=bladder.f)
names(bladder.f.1)=colnames(bladder.f)[5:ncol(bladder.f)]
bladder.f.2=parLapply(cl=cl, 6:ncol(bladder.f), foo, dat=bladder.f, smoking=T)
names(bladder.f.2)=colnames(bladder.f)[6:ncol(bladder.f)]
bladder.m.1=parLapply(cl=cl, 5:ncol(bladder.m), foo, dat=bladder.m)
names(bladder.m.1)=colnames(bladder.m)[5:ncol(bladder.m)]
bladder.m.2=parLapply(cl=cl, 6:ncol(bladder.m), foo, dat=bladder.m, smoking=T)
names(bladder.m.2)=colnames(bladder.m)[6:ncol(bladder.m)]
stopCluster(cl)

ifelse(dir.exists("../Results/strat_sex_univ"),"",dir.create("../Results/strat_sex_univ"))
arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("f","m"),each=2),".",1:2)
for (i in 1:length(arr)) {
  saveRDS(eval(parse(text=arr[i])),paste0("../Results/strat_sex_univ/",arr[i],".rds"))
}

### Make data for forest plot ----
# Create empty rows for smoking variable
smokingNA=matrix(NA, nrow=4, ncol=4)
rownames(smokingNA)=rownames(lung.f.1[[1]]$glm_output)

foo=function(x, smoking = FALSE){
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
  colnames(res)=paste0(c("or_","l95_","u95_","logp_"), substitute(x))
  return(res)
}
# cbind into one table
forest=cbind(foo(lung.f.1),foo(lung.f.2, smoking = T),
             foo(lung.m.1),foo(lung.m.2, smoking = T),
             foo(bladder.f.1),foo(bladder.f.2, smoking = T),
             foo(bladder.m.1),foo(bladder.m.2, smoking = T))
tmp=as.data.frame(model.matrix(~.,lung.f[,-1])[,-1])
mynames=colnames(tmp)
rownames(forest)=mynames[-c(1:7)]
saveRDS(forest, "../Results/strat_sex_univ/forest_plot.rds")

### Make data set for Manhattan plot ----
manhattan=cbind(do.call("rbind", lapply(lung.f.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(lung.f.2, `[[`, 2))),
                do.call("rbind", lapply(lung.m.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(lung.m.2, `[[`, 2))),
                do.call("rbind", lapply(bladder.f.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(bladder.f.2, `[[`, 2))),
                do.call("rbind", lapply(bladder.m.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(bladder.m.2, `[[`, 2))))

# Change to log scale
manhattan=-log10(manhattan)
# Change column name
colnames(manhattan)=arr
saveRDS(manhattan, "../Results/strat_sex_univ/manhattan_plot.rds")
