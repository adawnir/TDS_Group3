#SES stratified models
#Fergal

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

#Load data
bladder.1_denoised <- readRDS('../Results/denoised/bladder.1_denoised.rds')
bladder.2_denoised <- readRDS('../Results/denoised/bladder.2_denoised.rds')
lung.1_denoised <- readRDS('../Results/denoised/lung.1_denoised.rds')
lung.2_denoised <- readRDS('../Results/denoised/lung.2_denoised.rds')


#Stratify denoised data by Townsend deprivation index (<Q1, IQR, >Q3 - 3 groups)

bladder1cases <- bladder.1_denoised[bladder.1_denoised$case_status == 1,]
bladder2cases <- bladder.2_denoised[bladder.2_denoised$case_status == 1,]
lung1cases <- lung.1_denoised[lung.1_denoised$case_status == 1,]
lung2cases <- lung.2_denoised[lung.2_denoised$case_status == 1,]


#Calculate the quartiles
bladder1_Q1 <- quantile(bladder1cases$townsend,0.25, na.rm = TRUE)
bladder1_Q3 <- quantile(bladder1cases$townsend,0.75, na.rm = TRUE)

bladder2_Q1 <- quantile(bladder2cases$townsend,0.25, na.rm = TRUE)
bladder2_Q3 <- quantile(bladder2cases$townsend,0.75, na.rm = TRUE)

lung1_Q1 <- quantile(lung1cases$townsend,0.25, na.rm = TRUE)
lung1_Q3 <- quantile(lung1cases$townsend,0.75, na.rm = TRUE)

lung2_Q1 <- quantile(lung2cases$townsend,0.25, na.rm = TRUE)
lung2_Q3 <- quantile(lung2cases$townsend,0.75, na.rm = TRUE)

ifelse(dir.exists("../Results/strat_townsend_denoised"),"",dir.create("../Results/strat_townsend_denoised")) 

#Split based on the quartiles, then remove townsend variable before analysis
bladder1_low <- bladder.1_denoised[bladder.1_denoised$townsend < bladder1_Q1,]
bladder1_low <- bladder1_low[-7]
bladder1_high <- bladder.1_denoised[bladder.1_denoised$townsend > bladder1_Q3,]
bladder1_high <- bladder1_high[-7]
saveRDS(bladder1_low, '../Results/strat_townsend_denoised/bladder.l.1_denoised.rds')
saveRDS(bladder1_high, '../Results/strat_townsend_denoised/bladder.h.1_denoised.rds')


bladder2_low <- bladder.2_denoised[bladder.2_denoised$townsend < bladder2_Q1,]
bladder2_low <- bladder2_low[-3]
bladder2_high <- bladder.2_denoised[bladder.2_denoised$townsend > bladder2_Q3,]
bladder2_high <- bladder2_high[-3]
saveRDS(bladder2_low, '../Results/strat_townsend_denoised/bladder.l.2_denoised.rds')
saveRDS(bladder2_high, '../Results/strat_townsend_denoised/bladder.h.2_denoised.rds')


lung1_low <- lung.1_denoised[lung.1_denoised$townsend < lung1_Q1,]
lung1_low <- lung1_low[-7]
lung1_high <- lung.1_denoised[lung.1_denoised$townsend > lung1_Q3,]
lung1_high <- lung1_high[-7]
saveRDS(lung1_low, '../Results/strat_townsend_denoised/lung.l.1_denoised.rds')
saveRDS(lung1_high, '../Results/strat_townsend_denoised/lung.h.1_denoised.rds')


lung2_low <- lung.2_denoised[lung.2_denoised$townsend < lung2_Q1,]
lung2_low <- lung2_low[-3]
lung2_high <- lung.2_denoised[lung.2_denoised$townsend > lung2_Q3,]
lung2_high <- lung2_high[-3]
saveRDS(lung2_low, '../Results/strat_townsend_denoised/lung.l.2_denoised.rds')
saveRDS(lung2_high, '../Results/strat_townsend_denoised/lung.h.2_denoised.rds')


