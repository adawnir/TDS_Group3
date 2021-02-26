### TDS Project -- Table 1 p-values
### Programme created by Rin Wada on 26 Feb 2021

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(data.table)
library(tidyverse)

# Load new datasets
biomarkers=readRDS("../Results/biomarker_master.rds")
covar=readRDS("../Results/covar_master.rds")
cc=readRDS("../Results/case_control.rds")

mydata=cc %>% select(eid, case_status) %>%
  left_join(covar, by="eid") %>%
  select((eid:sex), bmi, smoking,(ethnic:hh_income),(total_meat:alcohol),
         (mat_smoke:autoimmune)) %>%
  left_join(biomarkers, by="eid") %>%
  select(-eid)
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
    tt = t.test(as.formula(paste(paste0("`",var_name,"`"),"case_status",sep="~")), data = dat)
    pval=tt$p.value
  } else {
    cst = chisq.test(dat[,i],dat[,1])
    pval=cst$p.value
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

# Export p-values
ifelse(dir.exists("../Exports"),"",dir.create("../Exports"))
write.csv(p, file = "../Exports/table1_pval_Rin.csv") # Change file name
