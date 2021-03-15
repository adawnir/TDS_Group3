### TDS Project -- Extract biomarkers
## Programme created by Vivan copy made by Rin on 14 March

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(tidyverse)
library(openxlsx)

##########################       extraction      ##########################

### Biomarker measurements are stored in a different dataset:
bmk=data.frame(fread("../Data/ukb27725.csv",nrows=1))
dim(bmk)
colnames(bmk)

### You can use the "Biomarker_annotation.xlsx" file to identify the biomarkers based on
### their field IDs:
annot=read.xlsx("../Dictionaries/Biomarker_annotation.xlsx")
head(annot)


### Loading the id file
myfields=unname(unlist(read.table("../Dictionaries/biomarker_id.txt", header=FALSE)))

### extract biomarkers
## Extracting the column ids 
column_id=grep("eid", colnames(bmk))
found_fieldids=NULL
for (k in 1:length(myfields)){
  mygrep=grep(paste0("X",myfields[k],"."), fixed=TRUE, colnames(bmk))
  if (length(mygrep)>0){
    found_fieldids=c(found_fieldids, myfields[k])
  }
  column_id=c(column_id, mygrep)
}

## Extracting required columns from dataset
biomarker_extracted=data.frame(fread("../Data/ukb27725.csv", select=column_id)) # Path to change!
write.csv(biomarker_extracted, "../Data/biomarkers.csv")


### reload the biomarkers and check
biomk=read.csv("../Data/biomarkers.csv")
class(biomk)
biomk=biomk[, -1]
dim(biomk)
View(biomk)


### remove people in withdrawn
withdrawn=as.character(read.csv("../Data/w19266_20200204.csv")[,1])
length(withdrawn)  #135
print(withdrawn)

head(biomk$eid)

sum(as.character(biomk$eid) %in% withdrawn)  #30
biomk=biomk[!(as.character(biomk$eid) %in% withdrawn), ]
nrow(biomk)  # 502506
dim(biomk)
View(biomk)


### check missing values, X30600.0.0:X30890.1.0

na_fre=select(biomk, X30620.0.0:X30890.1.0) %>%
  lapply(is.na) %>%
  sapply(function(x)sum(x)/nrow(biomk),simplify=TRUE)
na_fre
sum(na_fre <= 0.15)


### extract biomarkers measured first time
biomk_F=biomk[, c(1,seq(2,60, by=2))]
dim(biomk_F)
View(biomk_F)

### check na again
na_biomk_F=select(biomk_F, X30620.0.0:X30890.0.0) %>%
  lapply(is.na) %>%
  sapply(function(x)sum(x)/nrow(biomk_F),simplify=TRUE)
na_biomk_F

sum(na_biomk_F <= 0.15)
max(na_biomk_F)
sort(na_biomk_F)


### recode names
biomk_names=annot$Biomarker.name
colnames(biomk_F)[2:31]=biomk_names
View(biomk_F)
write.csv(biomk_F, "../Data/biomarkers_cleaned.csv")
biomarker=readRDS("../Results/extract_biomarkers.rds")
saveRDS(biomk_F, "../Results/extract_biomarkers.rds")
