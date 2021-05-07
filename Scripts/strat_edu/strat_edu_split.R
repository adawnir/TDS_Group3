### TDS Project -- Split data for stability selection PLS
## Programme created by Rin on 8 April

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(caret)

for(m in 1:8){
  arr=paste0(rep(c("lung","bladder"),each=4),".",rep(c("h","l"),each=2),".",1:2)[m] 
  dat=readRDS(paste0("../Results/strat_edu_denoised/",arr,"_denoised.rds"))
  y=dat$case_status
  x=dat[,-1]
  set.seed(100)
  train=createDataPartition(y,p=0.8,list=F)
  saveRDS(train, paste0("../Results/strat_edu_denoised/",arr,"_split.rds"))
}

