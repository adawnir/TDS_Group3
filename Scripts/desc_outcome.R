### TDS Project -- Descriptive analysis of outcomes
## Programme created by Rin Wada on 12 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

# Load data sets
cancer_outcome=readRDS("../Results/cancer_outcome.rds")
# Filter out negative time to diagnosis
cancer_outcome[cancer_outcome$lung_time < 0,"lung"]=0
cancer_outcome[cancer_outcome$bladder_time < 0,"bladder"]=0
# Time to diagnosis is in days; Convert days to years
cancer_outcome$lung_time_year=cancer_outcome$lung_time/365.25
cancer_outcome$bladder_time_year=cancer_outcome$bladder_time/365.25

# Extract age at assessment
data=data.frame(fread("../Data/ukb26390.csv", nrows=1))
myfields=list("21003")

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
age_data=subset(extracted, !extracted$eid %in% withdrawn)

# Merge into one dataframe
mydata=cbind(cancer_outcome, age_data[,2])
colnames(mydata)=c(colnames(cancer_outcome), "age_start")

# Calculate age at diagnosis
mydata$lung_age_diag=mydata$lung_time_year + mydata$age_start
mydata$bladder_age_diag= mydata$bladder_time_year + mydata$age_start

lung=mydata[mydata$lung==1,c("eid","lung_time_year","lung_age_diag")]
bladder=mydata[mydata$bladder==1,c("eid","bladder_time_year","bladder_age_diag")]

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
hist(lung$lung_time_year, main="Lung vs Bladder cancer", xlab="Time to diagnosis (years)", col=c1)
plot(hist(bladder$bladder_time_year, plot=F),
     add=T, col=c2)
legend("topright",legend=c("Lung cancer", "Bladder cancer"), fill=c(c1,c2), cex=0.8)
mean(lung$lung_time_year)
sd(lung$lung_time_year)
mean(bladder$bladder_time_year)
sd(bladder$bladder_time_year)

hist(lung$lung_age_diag, main="Lung vs Bladder cancer", xlab="Age at diagnosis (years)", col=c1)
plot(hist(bladder$bladder_age_diag, plot=F),
     add=T, col=c2)
legend("topright",legend=c("Lung cancer", "Bladder cancer"), fill=c(c1,c2), cex=0.75)
mean(lung$lung_age_diag)
sd(lung$lung_age_diag)
mean(bladder$bladder_age_diag)
sd(bladder$bladder_age_diag)

