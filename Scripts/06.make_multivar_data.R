### TDS Project -- Check complete cases
## Programme created by Rin Wada on 22 Feb 2021 reviewed on 24 Feb

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)

covar=readRDS("../Results/covar_models.rds")
biomarker_imp=readRDS("../Results/biomarker_imp_master.rds")
case_control=readRDS("../Results/case_control.rds")

sort(apply(covar, 2, function(x) sum(is.na(x))))

covar = covar %>%
  select((eid:sex), bmi,smoking,(ethnic:hh_income),(phys_PC1:alcohol),
         (mat_smoke:autoimmune))

case_control=case_control %>% select(eid,case_status)
multivar_data=inner_join(case_control,covar,by="eid") %>%
  right_join(biomarker_imp, by="eid") %>%
  na.omit()

rownames(multivar_data)=multivar_data$eid
multivar_data=select(multivar_data,-eid)
summary(multivar_data)
table(multivar_data$case_status)

# Separate by case
lung=multivar_data[multivar_data$case_status!="bladder",]
bladder=multivar_data[multivar_data$case_status!="lung",]

saveRDS(multivar_data, "../Results/multivar_data_full.rds")
saveRDS(lung, "../Results/multivar_data_lung.rds")
saveRDS(bladder, "../Results/multivar_data_bladder.rds")

### Data sets for Never smoker sensitivity analyses----
myeids=rownames(multivar_data) # extract eids from main data
covar=readRDS("../Results/never_smoker/covar_never_smoker.rds")

sort(apply(covar, 2, function(x) sum(is.na(x))))

covar = covar %>%
  select((eid:sex), bmi,(ethnic:hh_income),(phys_PC1:autoimmune))

case_control=case_control %>% select(eid,case_status)
multivar_data=inner_join(case_control,covar,by="eid") %>%
  right_join(biomarker_imp, by="eid")

rownames(multivar_data)=multivar_data$eid
multivar_data=select(multivar_data,-eid)
multivar_data=multivar_data[myeids,] # Consistent eids with main data set

summary(multivar_data)
table(multivar_data$case_status)

# Separate by case
lung=multivar_data[multivar_data$case_status!="bladder",]
bladder=multivar_data[multivar_data$case_status!="lung",]

ifelse(dir.exists("../Results/never_smoker"),"",dir.create("../Results/never_smoker"))
saveRDS(multivar_data, "../Results/never_smoker/multivar_data_full.rds")
saveRDS(lung, "../Results/never_smoker/multivar_data_lung.rds")
saveRDS(bladder, "../Results/never_smoker/multivar_data_bladder.rds")
