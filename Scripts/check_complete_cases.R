### TDS Project -- Check complete cases
## Programme created by Rin Wada on 22 Feb 2021 reviewed on 24 Feb

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

covar=readRDS("../Results/covar_master.rds")
biomarker_imp=readRDS("../Results/biomarker_imp_master.rds")
case_control=readRDS("../Results/case_control.rds")

case_control=case_control %>% select(eid,case_status)
multivar_data=inner_join(case_control,covar,by="eid") %>%
  right_join(biomarker_imp, by="eid") %>% na.omit()
summary(multivar_data)
table(multivar_data$case_status)

saveRDS(multivar_data, "../Results/multivar_data.rds")


