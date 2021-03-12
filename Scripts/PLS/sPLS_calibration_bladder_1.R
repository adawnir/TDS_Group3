
### TDS Project -- sPLS calibration

project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/PLS"
setwd(project_path)

library(mixOmics)
suppressPackageStartupMessages(library(sgPLS))
source("../pls_functions.R")

##### dataset

bladder.1 <- readRDS("../../Results/denoised/bladder.1_denoised.rds")
#bladder.2 <- readRDS("../../Results/denoised/bladder.2_denoised.rds")


no_var_bladder.1 <- NULL
for (i in 1:100) {
  ## balanced data
  set.seed(i)
  bladder_random_control <- bladder.1[which(bladder.1$case_status == 0), ]
  bladder_random_control <- bladder_random_control[sample(nrow(bladder_random_control), 1200), ]
  bladder_random.1 <- rbind(bladder_random_control, bladder.1[which(bladder.1$case_status == 1),])
  
  ## calibration:28
  X_B.1 <- bladder_random.1[,-1]
  Y_B.1 <- bladder_random.1[,1]
  set.seed(1)
  bladder_splsda.1 = CalibratesPLSDA(dataX = X_B.1, dataY = Y_B.1, ncomp = 1, Nrepeat = 100)
  no_var_bladder.1 <- c(no_var_bladder.1,  bladder_splsda.1$NVar)
}


saveRDS(no_var_bladder.1, "no_var_bladder.1.rds")


