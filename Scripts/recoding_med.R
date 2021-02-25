### TDS Project -- Recoding medical factors
## Programme created by Rin Wada on 10 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

# Load data sets
case_control=readRDS("../Results/case_control.rds")
recoded_covar=readRDS("../Results/recoded_covar.rds")

# Select medical data from covariates
medical_ids=as.character(c(137,20107,20110))

column_id=grep("eid", colnames(recoded_covar))
found_fieldids=NULL
for (i in 1:length(medical_ids)){
  mygrep=grep(paste0("X",medical_ids[i],"."), fixed=TRUE, colnames(recoded_covar))
  if (length(mygrep)>0){
    found_fieldids=c(found_fieldids, medical_ids[i])
  }
  column_id=c(column_id, mygrep)
}
setdiff(medical_ids,found_fieldids) # Field ids not found in dataset
covar_med=recoded_covar[,column_id]

# Number of medications (field code: 137)
sum(is.na(covar_med$X137.0.0)) # 862 missing
# Filter study population
medical_data=covar_med %>%
  rename(num_med=X137.0.0) %>%
  select(eid,num_med) %>%
  right_join(case_control, by="eid")
# Covert to category
summary(medical_data$num_med) # 860 missing
medical_data$num_med_cat=ifelse(medical_data$num_med==0, "0",
                                ifelse(medical_data$num_med==1,"1",">1"))
medical_data$num_med_cat=factor(medical_data$num_med_cat, labels = c("0","1",">1"))
summary(medical_data$num_med_cat)

# Parental history
myids=c(20107,20110)
group1=c("Chronic bronchitis/emphysema")
names(group1)=c("COPD")
group1NA=c("Prefer not to answer (group 1)", "Do not know (group 1)")
group2=c("Breast cancer","Bowel cancer","Lung cancer","Prostate cancer")
names(group2)=c("breast","bowel","lung","prostate")
group2NA=c("Prefer not to answer (group 2)", "Do not know (group 2)")

# Make empty columns
diseases=c(group1,group2)
x=matrix(0,nrow=nrow(medical_data),ncol=length(c(group1,group2)))
colnames(x)=paste0("parent","_",names(diseases))
medical_data=cbind(medical_data,x)

# Find eids with parental history for each diseases
mygrep=c(1,grep(paste0(paste0("X",myids,"."),collapse = "|"), colnames(covar_med)))
parent_data=covar_med[covar_med$eid %in% case_control$eid,mygrep]
for (d in 1:length(diseases)){
  print(names(diseases)[d])
  disease=diseases[d]
  myeids=apply(parent_data,2,function(x) parent_data$eid[grep(disease, x)]) %>% unlist
  if (disease %in% group1){
    tomatch=group1NA
  }
  if (disease %in% group2){
    tomatch=group2NA
  }
  myNA=apply(parent_data,2,function(x) parent_data$eid[x %in% tomatch]) %>% unlist
  myeids=unique(myeids)
  mygrepNA=apply(parent_data,1,function(x) sum(!is.na(x))) == 0 %>% unlist
  missing=parent_data$eid[mygrepNA] # missing from baseline
  myeidsNA=unique(c(myNA, missing)) # total missing
  colindex=grep(names(diseases)[d], colnames(medical_data))
  medical_data[medical_data$eid %in% myeids,colindex]=1
  medical_data[medical_data$eid %in% myeidsNA,colindex]=NA
}

# Check, restructure and save
summary(medical_data)
str(medical_data)
medical_data=medical_data %>% mutate_at(vars(starts_with("parent")), as.factor)
str(medical_data)
summary(medical_data)
saveRDS(medical_data, "../Results/medical_data.rds")
