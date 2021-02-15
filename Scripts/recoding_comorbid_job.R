### TDS Project -- Recoding comorbidities
## Programme created by Rin Wada on 9 Feb 2021 reviewed on 15 Feb 2021

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(data.table)
library(tidyverse)

mydata=readRDS("../Results/extract_comorbid.rds")

### Define diseases----
diseases=c("cardiovascular", "hypertension", "diabetes", "respiratory", "autoimmune")
names(diseases)=diseases

# Create dataset
case_control=readRDS("../Results/case_control.rds")
x=matrix(0,nrow=nrow(case_control),ncol=length(diseases))
colnames(x)=names(diseases)
comorbid=cbind(case_control, x)

## Disease definition - ICD10
# Cardiovascular (all circulatory except hypertension)
icd10_cardiovascular=c(paste0("I0",0:2), # acute rheumatic fever
                       paste0("I0",5:9), # chronic rheumatic
                       paste0("I",20:25), # ischemic heart
                       paste0("I",26:28), # pulmonary
                       paste0("I",30:52), # other heart disease
                       paste0("I",60:69), # cerebrovascular
                       paste0("I",70:76), # arteries -- excluding I77.6 (in autoimmune)
                       paste0("I77", 0:5),
                       paste0("I77", 7:9),
                       paste0("I",78:79), 
                       paste0("I",81:82), # vein thrombosis
                       paste0("I",95:99)) # other circulatory

# Hypertension
icd10_hypertension=c(paste0("I", 10:15))

# Diabetes
icd10_diabetes=c("E11", "E12", "E13", "E14")

# Respiratory (all)
icd10_respiratory=c(paste0("J", 30:39), # other upper
                    paste0("J", 40:47), # chronic lower
                    paste0("J", 60:70), # lung 
                    paste0("J", 80:84), # other interstitium
                    paste0("J", 85:86), # lower
                    paste0("J", 90:94), # other pleura
                    paste0("J", 95:99)) # other respiratory

# Autoimmune
icd10_autoimmune=readLines("../Dictionaries/icd10_autoimmune.txt")

## Self-reported illness codes
# Self-reported illness
mygrep=grep("X20002.", fixed=TRUE, colnames(mydata))

# Cardiovascular (all circulatory except hypertension)
self_cardiovascular=as.character(c(1066:1068,
                                   1074:1083,
                                   1086:1088,
                                   1093:1094))

# Hypertension
self_hypertension=as.character(c(1065,1072))

# Diabetes
self_diabetes=as.character(c(1220,1223))

# Respiratory (all)
self_respiratory=as.character(c(1111:1115,
                                1117,
                                1120:1126))

# Autoimmune
self_autoimmune=readLines("../Dictionaries/self_report_autoimmune.txt")

# HES: Hospital Episode Statistics
hes=readRDS("../Results/joined_hes.rds")

# HES table with all instances for defined study population
myhes=readRDS("../Results/myhes.rds")

# Case diagnosis
case_diag=case_control %>% select(eid, epistart)
# Recode comorbidities
for (d in 1:length(diseases)){
  print(names(diseases)[d])
  disease=diseases[d]
  icd10_list=eval(parse(text=paste0("icd10_",disease)))
  self_list=eval(parse(text=paste0("self_",disease)))
  myeids=NULL
  
  # ICD10 codes
  tmp=myhes[grepl(paste0(paste0("^", icd10_list),collapse="|"), myhes$diag_icd10),]
  mytime=case_diag[which(tmp$eid %in% case_diag$eid),"epistart"]
  for (i in 1:length(tmp$epistart)){
    if (is.na(mytime[i]) | tmp$epistart[i] > mytime[i]){
      myeids=c(myeids,as.character(tmp$eid[i]))
    } 
  }
  # Self-reported codes
  self_eids=apply(mydata[,mygrep],
                  2,
                  function(x)mydata$eid[grepl(paste0(self_list,collapse="|"), x)]) %>%
    unlist()
  myeids=c(self_eids,myeids) %>% unique %>% as.character
  print(table(comorbid[myeids,names(diseases)[d]]))
  comorbid[myeids,names(diseases)[d]]=1
  print(table(comorbid[,names(diseases)[d]]))
  cat("\n")
}

## Cancers besides cases
# Cases to exclude -- by case/control definition they should be excluded
#icd10_case=c(paste0("C",33:34), paste0("D02.",1:2), paste0("C",67), paste0("D09.",0))
#self_case=as.character(c(1001,1027,1028,1080,1035))

# All cancers
icd10_cancer=c(c(paste0("C0", 0:9), paste0("C", 10:14)), # lip, oral cavity, larynx
               paste0("C", 15:26), # digestive
               paste0("C", 30:39), # respiratory
               paste0("C", 40:41), # bone, cartilage
               paste0("C", 43:44), # skin
               paste0("C", 45:49), # mesothelial
               paste0("C", 50), # breast
               paste0("C", 51:58), # female genital
               paste0("C", 60:63), # male genital
               paste0("C", 64:68), # urinary
               paste0("C", 69:72), # nervous
               paste0("C", 73:75), # endocrin
               paste0("C", 76:80), # other sites
               paste0("C", 81:96), # lymphoid
               paste0("C", 97), # multiple
               paste0("D0", 0:9), # in situ
               paste0("D", 37:48)) # uncertain behaviour
# Self-reported cancer
mygrep=grep("X20001.", fixed=TRUE, colnames(mydata))
# ICD10 codes
tmp=myhes[grepl(paste0(paste0("^", icd10_cancer),collapse="|"), myhes$diag_icd10),]
mytime=case_diag[which(tmp$eid %in% case_diag$eid),"epistart"]
for (i in 1:length(tmp$epistart)){
  if (is.na(mytime[i]) | tmp$epistart[i] > mytime[i]){
    myeids=c(myeids,as.character(tmp$eid[i]))
  } 
}
# Self-reported codes
self_eids=mydata$eid[rowSums(is.na(mydata[,mygrep]))!=length(mygrep)]
myeids=c(self_eids,myeids) %>% unique %>% as.character
comorbid$cancer=ifelse(comorbid$eid %in% myeids,1,0)
table(comorbid$cancer)

saveRDS(comorbid, "../Results/comorbid.rds")