### TDS Project -- Descriptive analysis (outcome)
## Programme created by Rin Wada on 9 Feb 2021 reviewed on 15 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

data=data.frame(fread("../Data/ukb26390.csv", nrows=1))
myfields=list("816")

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
found_fieldids
