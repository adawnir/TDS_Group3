### TDS Project -- Recoding comorbidities
## Programme created by Rin Wada on 9 Feb 2021

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(data.table)
library(tidyverse)

### Extract data set---
data=data.frame(fread("../Data/ukb26390.csv", nrows=1))
myfields=list("20002")

# Extracting the column ids 
column_id=grep("eid", colnames(data))
found_fieldids=NULL
for (k in 1:length(myfields)){
  mygrep=grep(paste0("X",myfields[k],"."), fixed=TRUE, colnames(data))
  if (length(mygrep)>0){
    found_fieldids=c(found_fieldids, myfields[k])
  }
  column_id=c(column_id, mygrep)
}

# Extracting required columns from dataset
extracted=data.frame(fread("../Data/ukb26390.csv", select=column_id))
withdrawn=as.character(read.csv("../Data/w19266_20200204.csv")[,1])
mydata=subset(extracted, !extracted$eid %in% withdrawn)
saveRDS(mydata, "../Results/extract_comorbid.rds")

### Define diseases----
# Creating the dataset
comorbid=select(mydata, eid)
rownames(comorbid)=comorbid[,1]

diseases=c("cardiovascular", "hypertension", "diabetes", "respiratory", "autoimmune")
names(diseases)=diseases

x=matrix(0,nrow=nrow(comorbid),ncol=length(diseases))
colnames(x)=names(diseases)
comorbid=cbind(comorbid, x)

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
hes=data.frame(fread("../Data/hesin_diag.txt"))

# Self-reported illness
mygrep=grep("X20002.", fixed=TRUE, colnames(mydata))

for (d in 1:length(diseases)){
  print(names(diseases)[d])
  disease=diseases[d]
  icd10_list=eval(parse(text=paste0("icd10_",disease)))
  self_list=eval(parse(text=paste0("self_",disease)))
  myeids=NULL
  
  # ICD10 codes
  pb=txtProgressBar(style=3)
  for (k in 1:length(icd10_list)){
    setTxtProgressBar(pb, k/length(icd10_list))
    tmp=as.character(hes$eid[grepl(paste0("^", icd10_list[k]), hes$diag_icd10)])
    myeids=c(myeids, tmp)
  }
  myeids=unique(myeids)
  cat("\n")
  print(length(myeids))
  
  # Self-reported codes
  pb=txtProgressBar(style=3)
  for (k in 1:length(self_list)){
    setTxtProgressBar(pb, k/length(self_list) )
    tmp=as.character(mydata$eid[grepl(paste0("X", self_list[k],"."), mydata[,mygrep])])
    myeids=c(myeids, tmp)
  }
  myeids=unique(myeids)
  cat("\n")
  print(length(myeids))
  
  print(table(comorbid[myeids,names(diseases)[d]]))
  comorbid[myeids,names(diseases)[d]]=1
  print(table(comorbid[,names(diseases)[d]]))
  cat("\n")
}
saveRDS(comorbid, "../Results/comorbid.rds")
