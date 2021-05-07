### TDS Project -- Stratify
### Programme created by Rin Wada on 2 May

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(tidyverse)
library(parallel)

# Load new datasets
biomarkers=readRDS("../Results/biomarker_imp_master.rds")
covar=readRDS("../Results/covar_models.rds")
cc=readRDS("../Results/case_control.rds")

# Restructure data - case/control status in first column
colnames(covar)
covar = covar %>%
  select((eid:sex), bmi,smoking,(ethnic:hh_income),(phys_PC1:alcohol),
         (mat_smoke:autoimmune))

mydata=cc %>% select(eid, case_status) %>%
  left_join(covar, by="eid") %>%
  left_join(biomarkers, by="eid") %>%
  select(-eid)

colnames(mydata)
# Scale biomarkers and continuous environmental variables
# (all continuous variables that are not composite scores)
mydata[c(27:32,49:76)]=scale(mydata[c(27:32,49:76)])

summary(mydata) # check mean = 0

str(mydata) # Ensure categorical variables are coded as factors and continuous numerical

# Separate by case
lung_data=mydata[mydata$case_status!="bladder",]
bladder_data=mydata[mydata$case_status!="lung",]
# Recode case/control status
lung_data$case_status = as.factor(ifelse(as.character(lung_data$case_status) == "control",0,1))
bladder_data$case_status = as.factor(ifelse(as.character(bladder_data$case_status) == "control",0,1))

med_age_diag = median(cc$age_diag, na.rm=TRUE)
q_age_diag = quantile(cc$age_diag, c(0.25,0.75), na.rm=TRUE)

# Stratification AaD
tmp = cc$age_diag[which(rownames(cc) %in% rownames(lung_data))]
all(rownames(tmp)==rownames(lung_data))
lung_aad_mx=lung_data[which(tmp < med_age_diag | is.na(tmp)),]
lung_aad_my=lung_data[which(tmp > med_age_diag | is.na(tmp)),]
lung_aad_qx=lung_data[which(tmp < q_age_diag[1] | is.na(tmp)),]
lung_aad_qy=lung_data[which(tmp > q_age_diag[2] | is.na(tmp)),]

tmp = cc$age_diag[which(rownames(cc) %in% rownames(bladder_data))]
all(rownames(tmp)==rownames(bladder_data))
bladder_aad_mx=bladder_data[which(tmp < med_age_diag | is.na(tmp)),]
bladder_aad_my=bladder_data[which(tmp > med_age_diag | is.na(tmp)),]
bladder_aad_qx=bladder_data[which(tmp < q_age_diag[1] | is.na(tmp)),]
bladder_aad_qy=bladder_data[which(tmp > q_age_diag[2] | is.na(tmp)),]

med_time2diag = median(cc$time_diag_days, na.rm=TRUE)
q_time2diag = quantile(cc$time_diag_days, c(0.25,0.75), na.rm=TRUE)

# Stratification TtD
tmp = cc$time_diag_days[which(rownames(cc) %in% rownames(lung_data))]
all(rownames(tmp)==rownames(lung_data))
lung_ttd_mx=lung_data[which(tmp < med_time2diag | is.na(tmp)),]
lung_ttd_my=lung_data[which(tmp > med_time2diag | is.na(tmp)),]
lung_ttd_qx=lung_data[which(tmp < q_time2diag[1] | is.na(tmp)),]
lung_ttd_qy=lung_data[which(tmp > q_time2diag[2] | is.na(tmp)),]

tmp = cc$time_diag_days[which(rownames(cc) %in% rownames(bladder_data))]
all(rownames(tmp)==rownames(bladder_data))
bladder_ttd_mx=bladder_data[which(tmp < med_time2diag | is.na(tmp)),]
bladder_ttd_my=bladder_data[which(tmp > med_time2diag | is.na(tmp)),]
bladder_ttd_qx=bladder_data[which(tmp < q_time2diag[1] | is.na(tmp)),]
bladder_ttd_qy=bladder_data[which(tmp > q_time2diag[2] | is.na(tmp)),]

arr = c("lung_aad_mx","lung_aad_my","bladder_aad_mx","bladder_aad_my",
        "lung_aad_qx","lung_aad_qy","bladder_aad_qx","bladder_aad_qy",
        "lung_ttd_mx","lung_ttd_my","bladder_ttd_mx","bladder_ttd_my",
        "lung_ttd_qx","lung_ttd_qy","bladder_ttd_qx","bladder_ttd_qy")

ifelse(dir.exists("../Results/TtD_AaD_univ"),"",dir.create("../Results/TtD_AaD_univ"))
for (i in 1:length(arr)){
  saveRDS(eval(parse(text=arr[i])),paste0("../Results/TtD_AaD_univ/",arr[i],".rds"))
}
