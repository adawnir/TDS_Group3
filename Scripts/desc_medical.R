### TDS Project -- Descriptive analysis of risk factors (Medical)
## Programme created by Rin Wada on 10 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

### Descriptive analysis----
# Load data sets
comorbid_data=readRDS("../Results/comorbid.rds")
medical_data=readRDS("../Results/medical_data.rds")

mydata=inner_join(medical_data,comorbid_data)

tables=apply(mydata[,12:23],2,function(x) table(x,mydata$case_status))
prop.tables=sapply(tables,function(x) round(prop.table(x,2)*100,1))
total=apply(mydata[,12:23],2,function(x) sum(!is.na(x)))
apply(mydata[,12:23],2,function(x) table(is.na(x),mydata$case_status))
apply(mydata[,12:23],2,function(x) round(sum(is.na(x))/length(x)*100,1))

