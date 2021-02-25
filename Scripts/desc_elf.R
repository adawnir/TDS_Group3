### TDS Project -- Recoding early life factors
### Programme created by Rin Wada on 18 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

# Load data sets
case_control=readRDS("../Results/case_control.rds")
recoded_covar=readRDS("../Results/recoded_covar.rds")

# Select medical data from covariates
elf_ids=as.character(c(1787,1677))

column_id=grep("eid", colnames(recoded_covar))
found_fieldids=NULL
for (i in 1:length(elf_ids)){
  mygrep=grep(paste0("X",elf_ids[i],"."), fixed=TRUE, colnames(recoded_covar))
  if (length(mygrep)>0){
    found_fieldids=c(found_fieldids, elf_ids[i])
  }
  column_id=c(column_id, mygrep)
}
setdiff(elf_ids,found_fieldids) # Field ids not found in dataset
covar_elf=recoded_covar[,column_id]
covar_elf=covar_elf %>% select(eid,ends_with(".0.0"))
colnames(covar_elf)=c("eid","maternal_smoking","breastfed")

# Merge data sets by eid
elf_data=left_join(case_control,covar_elf,by="eid")

# Resturcture and save
str(elf_data)
table(elf_data$maternal_smoking)
table(elf_data$breastfed)
elf_data$maternal_smoking=ifelse(elf_data$maternal_smoking=="Do not know"|
                                   elf_data$maternal_smoking=="Prefer not to answer",
                                 NA, elf_data$maternal_smoking)
elf_data$breastfed=ifelse(elf_data$breastfed=="Do not know"|
                            elf_data$breastfed=="Prefer not to answer",
                          NA, elf_data$breastfed)
elf_data=elf_data %>%
  mutate(maternal_smoking=factor(maternal_smoking, labels=c("No","Yes")),
         breastfed=factor(breastfed, labels=c("No","Yes")))
str(elf_data)
saveRDS(elf_data, "../Results/elf_data.rds")

### Descriptive analysis ----
colnames(elf_data)
t1=table(elf_data$maternal_smoking,elf_data$case_status)
t2=table(elf_data$breastfed,elf_data$case_status)
round(prop.table(t1,2)*100,1)
round(prop.table(t2,2)*100,1)
apply(elf_data,2,function(x) sum(!is.na(x)))
apply(elf_data,2,function(x) round(sum(is.na(x))/length(x)*100,1))
