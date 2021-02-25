### TDS Project -- Biomarker data processing
## Programme created by Vivian and reviewed by Rin Wada on 24 and 25 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)

### Processing ----
# Load data sets
biomarker=readRDS("../Results/extract_biomarkers.rds")
case_control=readRDS("../Results/case_control.rds")

## Select eid of study population 
biomarker=biomarker[which(biomarker$eid %in% case_control$eid),]

## Remove two biomarkers with high missing rate (Oestradiol/Rheumatoid_factor)
biomarker=biomarker %>% select(-Oestradiol, -`Rheumatoid factor`)
str(biomarker)
saveRDS(biomarker,"../Results/biomarker_master.rds")
