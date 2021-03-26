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

#Calculate the quartiles
bladder1_Q1 <- quantile(bladder.1_denoised$townsend,0.25, na.rm = TRUE)
bladder1_Q3 <- quantile(bladder.1_denoised$townsend,0.75, na.rm = TRUE)

bladder2_Q1 <- quantile(bladder.2_denoised$townsend,0.25, na.rm = TRUE)
bladder2_Q3 <- quantile(bladder.2_denoised$townsend,0.75, na.rm = TRUE)

lung1_Q1 <- quantile(lung.1_denoised$townsend,0.25, na.rm = TRUE)
lung1_Q3 <- quantile(lung.1_denoised$townsend,0.75, na.rm = TRUE)

lung2_Q1 <- quantile(lung.2_denoised$townsend,0.25, na.rm = TRUE)
lung2_Q3 <- quantile(lung.2_denoised$townsend,0.75, na.rm = TRUE)


#Split based on the quartiles
bladder1_low <- bladder.1_denoised[bladder.1_denoised$townsend < bladder1_Q1,]
bladder1_mid <- bladder.1_denoised[bladder.1_denoised$townsend >= bladder1_Q1
                                   & bladder.1_denoised$townsend <= bladder1_Q3,]
bladder1_high <- bladder.1_denoised[bladder.1_denoised$townsend > bladder1_Q3,]
saveRDS(bladder1_low, '../Results/denoised/townsend_strat/bladderlow.1_denoised.rds')
saveRDS(bladder1_mid, '../Results/denoised/townsend_strat/bladdermid.1_denoised.rds')
saveRDS(bladder1_high, '../Results/denoised/townsend_strat/bladderhigh.1_denoised.rds')


bladder2_low <- bladder.2_denoised[bladder.2_denoised$townsend < bladder2_Q1,]
bladder2_mid <- bladder.2_denoised[bladder.2_denoised$townsend >= bladder2_Q1
                                   & bladder.2_denoised$townsend <= bladder2_Q3,]
bladder2_high <- bladder.2_denoised[bladder.2_denoised$townsend > bladder2_Q3,]
saveRDS(bladder2_low, '../Results/denoised/townsend_strat/bladderlow.2_denoised.rds')
saveRDS(bladder2_mid, '../Results/denoised/townsend_strat/bladdermid.2_denoised.rds')
saveRDS(bladder2_high, '../Results/denoised/townsend_strat/bladderhigh.2_denoised.rds')


lung1_low <- lung.1_denoised[lung.1_denoised$townsend < lung1_Q1,]
lung1_mid <- lung.1_denoised[lung.1_denoised$townsend >= lung1_Q1
                             & lung.1_denoised$townsend <= lung1_Q3,]
lung1_high <- lung.1_denoised[lung.1_denoised$townsend > lung1_Q3,]
saveRDS(lung1_low, '../Results/denoised/townsend_strat/lunglow.1_denoised.rds')
saveRDS(lung1_mid, '../Results/denoised/townsend_strat/lungmid.1_denoised.rds')
saveRDS(lung1_high, '../Results/denoised/townsend_strat/lunghigh.1_denoised.rds')


lung2_low <- lung.2_denoised[lung.2_denoised$townsend < lung2_Q1,]
lung2_mid <- lung.2_denoised[lung.2_denoised$townsend >= lung2_Q1
                             & lung.2_denoised$townsend <= lung2_Q3,]
lung2_high <- lung.2_denoised[lung.2_denoised$townsend > lung2_Q3,]
saveRDS(lung2_low, '../Results/denoised/townsend_strat/lunglow.2_denoised.rds')
saveRDS(lung2_mid, '../Results/denoised/townsend_strat/lungmid.2_denoised.rds')
saveRDS(lung2_high, '../Results/denoised/townsend_strat/lunghigh.2_denoised.rds')


