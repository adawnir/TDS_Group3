
### TDS Project -- sPLS calibration

project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/PLS"
setwd(project_path)

library(mixOmics)
suppressPackageStartupMessages(library(sgPLS))
source("../pls_functions.R")

##### dataset

bladder.1 <- readRDS("../../Results/denoised/bladder.1_denoised.rds")
bladder.2 <- readRDS("../../Results/denoised/bladder.2_denoised.rds")



no_var_bladder.2 <- NULL
for (i in 1:100) {
  ## balanced data
  set.seed(i)
  bladder_r_control <- bladder.2[which(bladder.2$case_status == 0), ]
  bladder_r_control <- bladder_r_control[sample(nrow(bladder_r_control), 1200), ]
  bladder_random.2 <- rbind(bladder_r_control, bladder.2[which(bladder.2$case_status == 1),])
  
  ## calibration:10
  X_B.2 <- bladder_random.2[,-1]
  Y_B.2 <- bladder_random.2[,1]
  set.seed(1)
  bladder_splsda.2 = CalibratesPLSDA(dataX = X_B.2, dataY = Y_B.2, ncomp = 1, Nrepeat = 100)
  no_var_bladder.2 <- c(no_var_bladder.2,  bladder_splsda.2$NVar)
}

saveRDS(no_var_bladder.2, "no_var_bladder.2.rds")

