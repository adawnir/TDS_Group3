### TDS Project -- Stability selection sPLS splitting train/test dataset 
## Programme created by Vivian on 24 March 


rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

lung.1 <- readRDS("../Results/denoised/lung.1_denoised.rds")
lung.2 <- readRDS("../Results/denoised/lung.2_denoised.rds")
bladder.1 <- readRDS("../Results/denoised/bladder.1_denoised.rds")
bladder.2 <- readRDS("../Results/denoised/bladder.2_denoised.rds")

foo.1 <- function(dat){
  y <- dat$case_status
  x <- dat[,-1]
  x <- x[, c(5:46, 1:4, 47:98)]
  set.seed(100)
  ids=caret::createDataPartition(y,p=0.8,list=F) # Directly call caret do not load package
  y_train=y[ids]
  x_train=x[ids,]
  train <- data.frame(cbind(y_train, x_train))
  y_test=y[-ids]
  x_test=x[-ids, ]
  test <- data.frame(cbind(y_test, x_test))
  df <- list(training = train, 
             testing = test)
}

lung_TT.1 <- foo.1(lung.1)
bladder_TT.1 <- foo.1(bladder.1)

foo.2 <- function(dat){
  y <- dat$case_status
  x <- dat[,-1]
  set.seed(100)
  ids=caret::createDataPartition(y,p=0.8,list=F) # Directly call caret do not load package
  y_train=y[ids]
  x_train=x[ids,]
  train <- data.frame(cbind(y_train, x_train))
  y_test=y[-ids]
  x_test=x[-ids, ]
  test <- data.frame(cbind(y_test, x_test))
  df <- list(training = train, 
             testing = test)
}


lung_TT.2 <- foo.2(lung.2)
bladder_TT.2 <- foo.2(bladder.2)


saveRDS(lung_TT.1, "../Results/PLS_319/train_test/lung_TT.1.rds")
saveRDS(lung_TT.2, "../Results/PLS_319/train_test/lung_TT.2.rds")
saveRDS(bladder_TT.1, "../Results/PLS_319/train_test/bladder_TT.1.rds")
saveRDS(bladder_TT.2, "../Results/PLS_319/train_test/bladder_TT.2.rds")

