### TDS Project -- Recoding outcome Exclude all prevante cancer cases from study
## Programme created by Rin Wada on 23 Feb 2021

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
# Load baseline data
mydata=readRDS("../Results/extract_recruitment.rds")

# HES: Hospital Episode Statistics
#hesin_diag <- data.frame(fread("../Data/hesin_diag.txt"))
#hesin <- data.frame(fread("../Data/hesin.txt")) # Diagnosis data
#colnames(hesin)

# Select unique identifiers and episode start date
#hesin_sub=hesin %>% na_if("") %>%
#mutate(epistart=coalesce(epistart, admidate)) %>%
#select(eid,ins_index,epistart)

# Join by eid and ins_index
#hes=inner_join(hesin_diag, hesin_sub, by=c("eid","ins_index"))
#head(hes)
#sum(is.na(hes$epistart)) # Successfully replaced missing values
#saveRDS(hes, "../Results/joined_hes.rds")

# Format date
#hes=readRDS("../Results/joined_hes.rds")
#hes=hes %>% mutate(epistart=as.Date(epistart,"%d/%m/%Y"))

## All cancer
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

icd9_cancer=c(paste0(140:149), # lip, oral cavity, larynx
              paste0(150:159), # digestive
              paste0(160:165), # respiratory
              paste0(170:176), # bone, cartilage
              paste0(179:189), # genital
              paste0(190:199), # unspecified
              paste0(200:209), # lymphoid
              paste0(230:234), # in situ
              paste0(235:238), # uncertain 
              paste0(239)) # unspecified

# Subset hes table by all cancer diagnosis only
#myhes=hes %>% select(eid,ins_index,level,diag_icd9, diag_icd10,epistart) %>%
#filter(grepl(paste0(paste0("^", icd9_cancer),collapse="|"), diag_icd9) |
#grepl(paste0(paste0("^", icd10_cancer),collapse="|"), diag_icd10))
# Save hes table with all cancer instances
#saveRDS(myhes, "../Results/hes_all_cancer.rds")

### Prepare data ----
myhes=readRDS("../Results/hes_all_cancer.rds")

# ICD codes for cases
icd10_lung=c(paste0("C",33:34), paste0("D02.",1:2)) # lung
icd10_bladder=c(paste0("C",67), paste0("D09.",0)) # bladder
icd9_lung=c("162","231.1","231.2") # lung
icd9_bladder=c("188","233.7") # bladder

# Extract date and age at baseline
sum(is.na(mydata$X53.0.0)) # No missing
sum(is.na(mydata$X21022.0.0)) # No missing
baseline=mydata %>%
  select(eid, X53.0.0, X21022.0.0) %>%
  mutate(X53.0.0=as.Date(X53.0.0,"%Y-%m-%d"))
colnames(baseline)=c("eid","date_baseline","age_baseline")

# Find first instance of lung cancer
lung=myhes %>%
  filter(grepl(paste0(paste0("^", icd9_lung),collapse="|"), diag_icd9)|
           grepl(paste0(paste0("^", icd10_lung),collapse="|"), diag_icd10)) %>%
  group_by(eid) %>%
  filter(epistart==min(epistart)) %>%
  distinct(eid, .keep_all=T) %>%
  select(eid, diag_icd9, diag_icd10, epistart)
colnames(lung)=c("eid",paste0(c("diag_icd9", "diag_icd10", "epistart"),"_lung"))
# Add to data set
outcome=left_join(baseline,lung,by="eid")
colnames(outcome)

# Find first instance of bladder cancer
bladder=myhes %>%
  filter(grepl(paste0(paste0("^", icd9_bladder),collapse="|"), diag_icd9)|
           grepl(paste0(paste0("^", icd10_bladder),collapse="|"), diag_icd10)) %>%
  group_by(eid) %>%
  filter(epistart==min(epistart)) %>%
  distinct(eid, .keep_all=T) %>%
  select(eid, diag_icd9, diag_icd10, epistart)
colnames(bladder)=c("eid",paste0(c("diag_icd9", "diag_icd10", "epistart"),"_bladder"))
# Add to data set
outcome=left_join(outcome,bladder,by="eid")
colnames(outcome)

# Find first instance of any other cancer
other=myhes[-which(grepl(paste0(paste0("^",c(icd9_lung,icd9_bladder)),collapse="|"),
                         myhes$diag_icd9) |
                     grepl(paste0(paste0("^",c(icd10_lung,icd10_bladder)),collapse="|"),
                           myhes$diag_icd10)),] %>%
  group_by(eid) %>%
  filter(epistart==min(epistart)) %>%
  distinct(eid, .keep_all=T) %>%
  select(eid, diag_icd9, diag_icd10, epistart)
colnames(other)=c("eid",paste0(c("diag_icd9", "diag_icd10", "epistart"),"_other"))
# Add to data set
outcome=left_join(outcome,other,by="eid")
colnames(outcome)

# Self-reported cancers
mygrep=grep("X20001.0.", fixed=TRUE, colnames(mydata))
self_ins=mydata[,c(1,mygrep)]
colnames(self_ins)=c("eid",paste0("self",1:6))
# Add to data set
outcome=inner_join(outcome,self_ins,by="eid")
colnames(outcome)

### Find prevalent cases ---
### If a participant was diagnosed with any cancer before baseline
### OR self-reported any cancer at baseline, they were excluded.

# Find cases with any self-reported cancer at baseline
self_any=outcome$eid[rowSums(!is.na(outcome[,paste0("self",1:6)]))>0]

# Remove all prevalent cancers
def_pop=outcome[-which(outcome$date_baseline >= outcome$epistart_lung |
                         outcome$date_baseline >= outcome$epistart_bladder |
                         outcome$date_baseline >= outcome$epistart_other |
                         outcome$eid %in% self_any),]
nrow(outcome)-nrow(def_pop)

### Find cases with other cancer before diagnosis ---
###	If a participant was diagnosed with any other cancer before and
### was diagnosed with lung/bladder cancer after, they were excluded.
# Remove
def_pop2=def_pop[-which(def_pop$epistart_other <= def_pop$epistart_lung|
                          def_pop$epistart_other <= def_pop$epistart_bladder),]
nrow(def_pop)-nrow(def_pop2)

# Check for co-occurring lung and bladder cancer
tmp=def_pop2[which(!is.na(def_pop2$epistart_bladder)&!is.na(def_pop2$epistart_lung)),]
nrow(tmp) # 18 co-occurring cases
# Remove diagnosed on same day
def_pop3=def_pop2[-which(def_pop2$epistart_lung==def_pop2$epistart_bladder),]
nrow(def_pop2)-nrow(def_pop3)
nrow(def_pop3)
# Only keep first diagnosis
def_pop3[which(def_pop3$epistart_bladder>def_pop3$epistart_lung),
         c("diag_icd9_lung","diag_icd10_lung","epistart_lung")]=NA
def_pop3[which(def_pop3$epistart_lung>def_pop3$epistart_bladder),
         c("diag_icd9_bladder","diag_icd10_bladder","epistart_bladder")]=NA

## Case/control status
def_pop3$case_status=0
def_pop3$case_status[!is.na(def_pop3$epistart_bladder)]=2
def_pop3$case_status[!is.na(def_pop3$epistart_lung)]=1
table(def_pop3$case_status)

def_pop4=def_pop3 %>%
  mutate(diag_icd9=coalesce(diag_icd9_lung,diag_icd9_bladder,diag_icd9_other),
         diag_icd10=coalesce(diag_icd10_lung,diag_icd10_bladder,diag_icd10_other),
         epistart=coalesce(epistart_lung,epistart_bladder,epistart_other))
str(def_pop4)
# Calculate time to diagnosis and age at diagnosis
def_pop4$time_diag_days=difftime(def_pop4$epistart,
                                 def_pop4$date_baseline,
                                 units = "days") %>% as.numeric()
def_pop4$time_diag_years=def_pop4$time_diag_days/365.25
def_pop4$age_diag=def_pop4$age_baseline+def_pop4$time_diag_years
### Restructure ---
case_control=def_pop4 %>% select(eid:age_baseline,case_status:age_diag) %>%
  mutate(case_status=factor(case_status,levels = c(0:2),labels = c("control","lung","bladder")))
str(case_control)
table(case_control$case_status)

saveRDS(case_control, "../Results/case_control.rds")
