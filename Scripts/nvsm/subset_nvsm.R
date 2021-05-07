# subset nvsms without household smokers

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

# just want neversmoking data + household smokers
mydata=readRDS("../Results/nvsm/covar_never_smoker.rds")

nvsm <- mydata %>% select(eid,smoking,smoke_hh)

nvsm$smoking=ifelse(nvsm$smoking %in% c("NA","Prefer not to answer"),NA,
                                             nvsm$smoking)
nvsm$smoking <- as.factor(nvsm$smoking)
summary(nvsm$smoking)

nvsm$smoke_hh <- as.factor(nvsm$smoke_hh)
summary(nvsm$smoke_hh)

saveRDS(nvsm, "../Results/nvsm/smoking_status.rds")


rm(list=ls())

status <- readRDS("../Results/nvsm/smoking_status.rds")
lung <- readRDS("../Results/nvsm/multivar_data_lung.rds") # 808 lung cases, total 231765 obs
# has 190476 no's to smoking in household, 19308 yes, 21981 current smokers in household
bladder <- readRDS("../Results/nvsm/multivar_data_bladder.rds") 
# 921 bladder cases, 230957 controls
# 190733 no, 19330 yes, 21815 current smokers in household
table(status$smoking, status$smoke_hh)
# again, issue is that current smokers in household has been recoded to include current smokers
# we want to look at NEVER smokers, but that can include smokers in household

# EXACTLY THE SAME DATASETS AS USED FOR THE OTHER ONES

status<- as.data.frame(status)
table(status$smoking, status$smoke_hh)
status <- subset(status, status$smoking=="Never") # gives us 249195 never smokers
status <- subset(status, status$smoke_hh=="No") # gives us 225539 never smokers with no household smokers
ids <- status$eid

lung <- subset(lung, rownames(lung) %in% ids) # yields 224 lung cancer cases, 193902 never smoking controls
bladder<-subset(bladder, rownames(bladder) %in% ids) # yields 474 bladder cases, 193902 never smoking controls


# let's drop the unused levels

bladder <- droplevels(bladder)
lung <- droplevels(lung)
bladder<-subset(bladder,select=-c(smoke_hh, smoking))
lung<-subset(lung,select=-c(smoke_hh, smoking))
saveRDS(lung, "../Results/nvsm/multivar_data_lung_nvsm.rds")
saveRDS(bladder, "../Results/nvsm/multivar_data_bladder_nvsm.rds")