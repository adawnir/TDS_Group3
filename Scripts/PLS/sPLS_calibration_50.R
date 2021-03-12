
### TDS Project -- sPLS calibration

project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/PLS"
setwd(project_path)

library(mixOmics)
suppressPackageStartupMessages(library(sgPLS))
source("../pls_functions.R")

##### dataset
lung.1 <- readRDS("../../Results/denoised/lung.1_denoised.rds")
#lung.2 <- readRDS("../../Results/denoised/lung.2_denoised.rds")


##### subset---

set.seed(7)
r_control_lung.1 <- lung.1[which(lung.1$case_status == 0), ]
r_control_lung.1 <- lung.1[sample(nrow(lung.1), 800), ]
r_lung.1 <- rbind(r_control_lung.1, lung.1[which(lung.1$case_status == 1),])
  
#set.seed(7)
#r_control_lung.2 <- lung.2[which(lung.2$case_status == 0), ]
#r_control_lung.2 <- lung.2[sample(nrow(lung.2), 800), ]
#r_lung.2 <- rbind(r_control_lung.2, lung.2[which(lung.2$case_status == 1),])


##### run calibration---

X_pooled.1 <- r_lung.1[,-1]
Y_pooled.1 <- r_lung.1[,1]
set.seed(1)
lund_splsda.1 = CalibratesPLSDA(dataX = X_pooled.1, dataY = Y_pooled.1, ncomp = 1, Nrepeat = 50)

png(filename = "Claibration_lung_1.png")
PlotCalib(res = lund_splsda.1)
dev.off()

saveRDS(lund_splsda.1, "lund_splsda.1.rds")


#X_pooled.2 <- r_lung.2[,-1]
#Y_pooled.2 <- r_lung.2[,1]
#set.seed(1)
#lund_splsda.2 = CalibratesPLSDA(dataX = X_pooled.2, dataY = Y_pooled.2, ncomp = 1, Nrepeat = 100)

#png(filename = "Claibration_lung_2.png")
#PlotCalib(res = lund_splsda.2)
#dev.off()
#saveRDS(lund_splsda.2, "lund_splsda.2.rds")





