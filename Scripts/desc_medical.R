### TDS Project -- Descriptive analysis of risk factors (Medical)
## Programme created by Rin Wada on 10 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

# Load data sets
comorbid_data=readRDS("../Results/comorbid.rds")
case_control=readRDS("../Results/case_control.rds")
recoded_covar=readRDS("../Results/recoded_covar.rds")

# Select medical data from covariates
medical_ids=as.character(c(21058,137,20107,20110,20111))

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
# If NA, fill in with value from later columns
id=137
mygrep=grep(paste0("X",id,"."), fixed=TRUE, colnames(covar_med))
medical_data=data.frame(eid = covar_med[,1])
medical_data$num_meds=coalesce(!!!covar_med[,mygrep])
summary(covar_med$X137.0.0)
summary(medical_data$num_meds)

# Family history (Father, Mother and Siblings)
myids =c(20107,20110,20111)
prefix=c("Father", "Mother", "Siblings")
group1=c("Chronic bronchitis/emphysema")
names(group1)=c("COPD")
group1NA=c("Prefer not to answer (group 1)", "Do not know (group 1)")
group2=c("Breast cancer","Bowel cancer","Lung cancer","Prostate cancer")
names(group2)=c("Breast cancer","Bowel cancer","Lung cancer","Prostate cancer")
group2NA=c("Prefer not to answer (group 2)", "Do not know (group 2)")

diseases=c(group1,group2)
names(diseases)=names(c(group1,group2))
x=matrix(0,nrow=nrow(medical_data),ncol=length(c(group1,group2))*3)
colnames(x)=sapply(prefix, function(x) paste(x, names(diseases)))
medical_data=cbind(medical_data,x)

for (i in 1:length(myids)){
  print(prefix[i])
  mygrep=grep(paste0("X",myids[i],"."), fixed=TRUE, colnames(covar_med))
  subset=covar_med[,mygrep]
  for (d in 1:length(diseases)){
    print(names(diseases)[d])
    disease=diseases[d]
    myeids=apply(subset,2,function(x) covar_med$eid[grep(disease, x)]) %>% unlist
    if (disease %in% group1){
      tomatch=group1NA
    }
    if (disease %in% group2){
      tomatch=group2NA
    }
    myNA=apply(subset,2,function(x) covar_med$eid[x %in% tomatch]) %>% unlist
    myeids=unique(myeids)
    mygrepNA=apply(subset,1,function(x) sum(!is.na(x))) == 0 %>% unlist
    missing=covar_med$eid[mygrepNA] # missing from baseline
    myeidsNA=unique(c(myNA, missing)) # total missing
    
    colindex=grep(paste(prefix[i], names(diseases)[d]), colnames(medical_data))
    medical_data[medical_data$eid %in% myeids,colindex]=1
    medical_data[medical_data$eid %in% myeidsNA,colindex]=NA
  }
}
summary(medical_data)

# Remove gender specific cancers
medical_data$`Father Breast cancer`=NULL
medical_data$`Mother Prostate cancer`=NULL

# Merge data sets by eid
mydata=merge(merge(case_control,comorbid_data,by="eid",all=TRUE),medical_data,by="eid",all=TRUE)
str(mydata)
mydata=mydata %>% mutate_at(vars(-eid, -num_meds), as.factor)
str(mydata)
saveRDS(mydata, "../Results/medical_data.rds")

# Descciptive analysis
mydata=readRDS("../Results/medical_data.rds")
lung=mydata[mydata$lung==1,-c(2:4)]
bladder=mydata[mydata$bladder==1,-c(2:4)]
control=mydata[mydata$control==1,-c(2:4)]

mean(lung$num_meds, na.rm = T)
sd(lung$num_meds, na.rm = T)
mean(bladder$num_meds, na.rm = T)
sd(bladder$num_meds, na.rm = T)
mean(control$num_meds, na.rm = T)
sd(control$num_meds, na.rm = T)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(216,238,192, max = 255, alpha = 80, names = "lt.green")

hgL <- hist(lung$num_meds, plot = FALSE) # Save first histogram data
hgB <- hist(bladder$num_meds, plot = FALSE) # Save 2nd histogram data
plot(hgL, col = c1,
     main = "Histogram of number of medications", xlab = "Number of medications")
plot(hgB, col = c2, add = TRUE) 
hist(control$num_meds, col = c3,
     main = "Histogram of number of medications", xlab = "Number of medications")

summary(lung)
sum(is.na(lung$num_meds))/length(lung$num_meds)*100
apply(lung[,-c(1,7)], 2, function(x) round(prop.table(table(x, useNA = "ifany"))[3]*100,1))
apply(lung[,-c(1,7)], 2, function(x) round(prop.table(table(x))*100,1))

summary(bladder)
sum(is.na(bladder$num_meds))/length(bladder$num_meds)*100
apply(bladder[,-c(1,7)], 2, function(x) round(prop.table(table(x, useNA = "ifany"))[3]*100,1))
apply(bladder[,-c(1,7)], 2, function(x) round(prop.table(table(x))*100,1))

summary(control)
sum(is.na(control$num_meds))/length(control$num_meds)*100
apply(control[,-c(1,7)], 2, function(x) round(prop.table(table(x, useNA = "ifany"))[3]*100,1))
apply(control[,-c(1,7)], 2, function(x) round(prop.table(table(x))*100,1))

