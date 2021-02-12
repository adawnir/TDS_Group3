### TDS Project -- Descriptive analysis (outcome)
## Programme created by Rin Wada on 9 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

### Extract data set---
# Loading the data
data=data.frame(fread("../Data/ukb26390.csv", nrows=1))
myfields=list("53","20001")

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
saveRDS(mydata, "../Results/extract_53_20001.rds")

### Define cases----
# Creating the dataset
mydata=readRDS("../Results/extract_53_20001.rds")
cancer_outcome=select(mydata, eid)
rownames(cancer_outcome)=cancer_outcome[,1]

outcome=c("lung", "bladder")
names(outcome)=outcome

x <- matrix(0,nrow=nrow(cancer_outcome),ncol=length(outcome)*2)
colnames(x) <- c(names(outcome), paste0(names(outcome), "_", "time"))
cancer_outcome <- cbind(cancer_outcome,x)

# Lung and bladder
icd10_codes <- list(c(paste0("C",33:34), paste0("D02.",1:2)), # lung cancer
                    c(paste0("C",67), paste0("D09.",0))) # bladder cancer

# HES: Hospital Episode Statistics
hesin_diag <- data.frame(fread("../Data/hesin_diag.txt"))
hesin <- data.frame(fread("../Data/hesin.txt")) # Diagnosis data
colnames(hesin)
hesin_sub=hesin %>% select(eid,ins_index,epistart,admidate)
# Merge by eid and ins_index
hes=inner_join(hesin_diag, hesin_sub, by=c("eid","ins_index"))
sum(is.na(hes$epistart)) # No missing values
table(table(hes$eid[grepl(paste0("^", icd10_codes[[1]][1]), hes$diag_icd10)]))
# must select first instance per eid
# Format date
hes=hes %>% mutate(epistart=as.Date(epistart,"%d/%m/%Y"))
mydata=mydata %>% na_if("") %>% mutate_at(vars(matches(paste0("X53."))), as.Date, "%Y-%m-%d")
sum(is.na(mydata$X53.0.0)) # No missing values
for (o in 1:length(outcome)){
  print(names(outcome)[o])
  # ICD10 codes
  icd10=icd10_codes[[o]]
  mygrep=grep(paste0(paste0("^", icd10),collapse="|"), hes$diag_icd10)
  myhes=hes[mygrep,c("eid","ins_index","epistart")] %>%
    group_by(eid) %>%
    filter(epistart == min(epistart)) %>%
    distinct(eid,.keep_all = T)
  unique_eids=unique(myhes$eid)
  start=mydata[mydata$eid %in% unique_eids,"X53.0.0"]
  stop=myhes$epistart
  mytime=difftime(stop,start, units = "days") %>% as.numeric()
  cat("\n")
  print(length(unique_eids))
  print(table(cancer_outcome[unique_eids,names(outcome)[o]]))
  cancer_outcome[names(outcome)[o]]=ifelse(cancer_outcome$eid %in% as.character(unique_eids),
                                           1,0)
  cancer_outcome[as.character(unique_eids),paste0(names(outcome)[o],"_","time")]=mytime
  print(table(cancer_outcome[,names(outcome)[o]]))
  cat("\n")
}
saveRDS(cancer_outcome, "../Results/cancer_outcome.rds")

## Filter negative time to diagnosis
cancer_outcome=readRDS("../Results/cancer_outcome.rds")
cancer_outcome[cancer_outcome$lung_time < 0,"lung"]=0
cancer_outcome[cancer_outcome$bladder_time < 0,"bladder"]=0
# Check
table(cancer_outcome$lung)
table(cancer_outcome$bladder)

### Define controls----
# Self-reported cancer
mygrep=grep("X20001.", fixed=TRUE, colnames(mydata))
self_eid <- mydata$eid[rowSums(is.na(mydata[,mygrep]))!=length(mygrep)]

# Diagnosed cancer (all except benign)
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
tomatch=sapply(icd10_cancer, function(x) paste0("^", x))
diag_eid=hesin_diag$eid[grepl(paste0(tomatch,collapse="|"), hesin_diag$diag_icd10)]
myeids=unique(c(self_eid,diag_eid))
cancer_outcome$control=ifelse(mydata$eid %in% myeids,0,1)

# Descriptive analysis: Case/Control
table(cancer_outcome$lung)
table(cancer_outcome$bladder)
table(cancer_outcome$control)

saveRDS(cancer_outcome, "../Results/case_control.rds")

table(cancer_outcome$lung, cancer_outcome$bladder)
table(cancer_outcome$lung, cancer_outcome$control)
table(cancer_outcome$bladder, cancer_outcome$control)
