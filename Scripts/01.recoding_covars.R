### TDS Project -- Recoding covariates
### Programme created by Ines; reviewed by Rin on 10 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# Load packages
library(openxlsx)
library(data.table)

# Load annotation
covars=read.csv("../Dictionaries/covar_annot.csv")
head(covars)

# Extract dataset
data=data.frame(fread("../Data/ukb26390.csv", nrows=1))
myfields=covars$field_id
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
nrow(extracted)
withdrawn=as.character(read.csv("../Data/w19266_20200204.csv")[,1])
mydata=subset(extracted, !extracted$eid %in% withdrawn)
nrow(mydata)
nrow(extracted) - nrow(mydata)
saveRDS(mydata, "../Results/extract_covar.rds")

# Load extracted data
mydata=readRDS("../Results/extract_covar.rds")
# Load coding ids
mycoding=read.csv("../Dictionaries/Codings_Showcase.csv")
head(mycoding)

# Make copy of data
recoded_covar=mydata

### Iterate ----
for (i in 1:nrow(covars)){
  if (!is.na(covars$coding_id[i])){
    myid=covars$coding_id[i]
    
    mycoding_field=mycoding[which(mycoding[,1]==myid),]
    mycoding_field=mycoding_field[,-1]
    rownames(mycoding_field) <- mycoding_field[,1]
    
    # Recoding categories
    mygrep=grep(paste0("X",covars$field_id[i],"."), fixed=TRUE, colnames(recoded_covar))
    for (g in 1:length(mygrep)){
      recoded_covar[,mygrep[g]]=ifelse(
        mydata[,mygrep[g]] %in% rownames(mycoding_field),
        as.character(mycoding_field[as.character(mydata[,mygrep[g]]),"Meaning"]),
        mydata[,mygrep[g]])
    }
  }
}
# structure of original data
str(mydata)
# structure of recoded data
str(recoded_covar)
# check missing values for first instances
recoded_covar %>% select(contains(".0.")) %>% apply(., 2, function(x) sum(is.na(x)))

saveRDS(recoded_covar,"../Results/recoded_covar.rds")
