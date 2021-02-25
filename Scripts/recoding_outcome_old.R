### TDS Project -- Recoding outcome
## Programme created by Rin Wada on 9 Feb 2021 reviewed on 15 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

### Extract data set---
# Loading the data
#data=data.frame(fread("../Data/ukb26390.csv", nrows=1))
#myfields=list("53","21022","20001")

# Extracting the column ids 
#column_id=grep("eid", colnames(data))
#for (k in 1:length(myfields)){
  #found_fieldids=NULL
  #mygrep=grep(paste0("X",myfields[k],"."), fixed=TRUE, colnames(data))
  #if (length(mygrep)>0){
    #found_fieldids=c(found_fieldids, myfields[k])
  #}
  #column_id=c(column_id, mygrep)
#}

# Extracting required columns from dataset
#extracted=data.frame(fread("../Data/ukb26390.csv", select=column_id))
#withdrawn=as.character(read.csv("../Data/w19266_20200204.csv")[,1])
#mydata=subset(extracted, !extracted$eid %in% withdrawn)
#saveRDS(mydata, "../Results/extract_recruitment.rds")

### Define study population----
mydata=readRDS("../Results/extract_recruitment.rds")
# Self-reported cancer
mygrep=grep("X20001.", fixed=TRUE, colnames(mydata))
self_list=as.character(c(1001,1027,1028,1080, # lung
                         1035)) # bladder

self_eid=apply(mydata[,mygrep],
               2,
               function(x) as.character(mydata$eid[grepl(paste0(self_list,collapse="|"), x)])) %>%
  unlist()

# Lung and bladder ICD codes
icd10_lung=c(paste0("C",33:34), paste0("D02.",1:2)) # lung
icd10_bladder=c(paste0("C",67), paste0("D09.",0)) # bladder

# HES: Hospital Episode Statistics
#hesin_diag <- data.frame(fread("../Data/hesin_diag.txt"))
#hesin <- data.frame(fread("../Data/hesin.txt")) # Diagnosis data
#colnames(hesin)

# Select unique identifiers and episode start date
#hesin_sub=hesin %>% 
  #na_if("") %>%
  #mutate(epistart=coalesce(epistart, admidate)) %>%
  #select(eid,ins_index,epistart)

# Join by eid and ins_index
#hes=inner_join(hesin_diag, hesin_sub, by=c("eid","ins_index"))
#head(hes)
#sum(is.na(hes$epistart)) # Successfully replaced missing values
#saveRDS(hes, "../Results/joined_hes.rds")

# Format date
hes=readRDS("../Results/joined_hes.rds")
hes=hes %>% mutate(epistart=as.Date(epistart,"%d/%m/%Y"))

# Extract date and age at baseline
sum(is.na(mydata$X53.0.0)) # No missing
sum(is.na(mydata$X21022.0.0)) # No missing
baseline=mydata %>%
  select(eid, X53.0.0, X21022.0.0) %>%
  mutate(X53.0.0=as.Date(X53.0.0,"%Y-%m-%d"))
colnames(baseline)=c("eid","date_baseline","age_baseline")

# Recode outcome
myhes=hes %>% select(eid,ins_index,level,diag_icd10,epistart) %>%
  mutate(lung=grepl(paste0(paste0("^", icd10_lung),collapse="|"), hes$diag_icd10),
         bladder=grepl(paste0(paste0("^", icd10_bladder),collapse="|"), hes$diag_icd10))
# Save hes table with all lung/bladder cancer instances
saveRDS(myhes, "../Results/hes_outcome.rds")

# Filter first instances
unique_hes=myhes %>%
  filter(lung==T | bladder==T) %>%
  group_by(eid) %>%
  filter(epistart == min(epistart)) %>%
  distinct(eid,.keep_all = T)

table(unique_hes$lung,unique_hes$bladder)
table(unique_hes$level)

# Join hes table and baseline information by eid
cancer_outcome=inner_join(unique_hes, baseline, by="eid")

# Calculate time to diagnosis and age at diagnosis
cancer_outcome$time_diag_days=difftime(cancer_outcome$epistart,
                                       cancer_outcome$date_baseline,
                                       units = "days") %>% as.numeric()
cancer_outcome$age_diag=cancer_outcome$age_baseline+cancer_outcome$time_diag_days/365.25

# Prevalent cases
icd10_eid=cancer_outcome$eid[cancer_outcome$time_diag_days <= 0]
remove_eid=unique(c(self_eid,icd10_eid))
length(remove_eid)
case_control=mydata %>% select(eid) %>% filter(!(eid %in% remove_eid))
nrow(case_control)

# Filter out patients with both cancer
lung_eid=myhes$eid[myhes$lung==T] %>% unique
bladder_eid=myhes$eid[myhes$bladder==T] %>% unique
remove_eid=intersect(lung_eid,bladder_eid)

case_control=filter(case_control,!(eid %in% remove_eid))
nrow(case_control)

# Join case and control
case_control=case_control %>% left_join(cancer_outcome, by="eid")
colnames(case_control)
table(case_control$lung)
table(case_control$bladder)

# Make dataframe
# Make case/control status column
case_control$case_status[case_control$bladder==T]=2
case_control$case_status[case_control$lung==T]=1
case_control$case_status[is.na(case_control$lung) & is.na(case_control$bladder)]=0
table(case_control$case_status)

str(case_control)
case_control=case_control %>%
  select(eid, date_baseline, age_baseline, case_status, ins_index, level, diag_icd10, epistart,
         time_diag_days, age_diag) %>%
  mutate(case_status=factor(case_status, labels=c("control","lung","bladder")),
         level=as.factor(level),
         time_diag_days=as.integer(time_diag_days))
rownames(case_control)=case_control[,1]
str(case_control)
# Check
table(case_control$case_status)

saveRDS(case_control, "../Results/case_control.rds")
