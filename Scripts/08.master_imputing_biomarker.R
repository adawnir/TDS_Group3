### TDS Project -- Biomarker data imputation (RUN ON HPC)
## Programme created by Vivian and reviewed by Rin Wada on 24 Feb 2021

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

# Load packages
library(imputeLCMD)

# Load data sets
biomarker=readRDS("../Results/biomarker_master.rds")

### Pre-processing for imputation ---
# Remove individuals with 8 or more biomarkers missing
biomarker=biomarker[-which(rowSums(is.na(biomarker[-1]))>=8),]

### Imputation---
## impute.QRILC
set.seed(7)
biomarker_log_t=t(log2(biomarker[,-1]))
biomarker_imp=impute.QRILC(biomarker_log_t)
biomarker_imp=data.frame(t(biomarker_imp[[1]]))
summary(biomarker_imp)
biomarker_imp=cbind(biomarker$eid, biomarker_imp)
colnames(biomarker_imp)=c("eid", colnames(biomarker_imp)[-1])
str(biomarker_imp)

## Save
saveRDS(biomarker_imp, "../Results/biomarker_imp_master.rds")