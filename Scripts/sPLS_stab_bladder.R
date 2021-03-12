

project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

install.packages("pheatmap")
suppressPackageStartupMessages(library(pheatmap))
library(mixOmics)
suppressPackageStartupMessages(library(sgPLS))

source("pls_functions.R")

MyPal = brewer.pal("Paired", n = 12) 
MyPalPairsExt <- colorRampPalette(MyPal)(100)


##### dataset
bladder.1 <- readRDS("../Results/denoised/bladder.1_denoised.rds")
bladder.2 <- readRDS("../Results/denoised/bladder.2_denoised.rds")

##### 
## bladder: 28 from balanced dataset
x_bladder.1 <- bladder.1[,-1]
x_bladder.1 <- cbind(x_bladder.1[,5:47], x_bladder.1[,1:4], x_bladder.1[, 48:98])
y_bladder.1 <- bladder.1[,1]


##### 
## bladder: 10 from balanced dataset
x_bladder.2 <- bladder.2[,-1]
y_bladder.2 <- bladder.2[,1]


##### stability

set.seed(1)
Stab_bladder.1 = StabilityPlot(X = x_bladder.1, Y =  y_bladder.1, NIter = 100)
pheatmap(Stab_bladder.1, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE,
         height = 30, width = 60, fontsize = 20, filename = "stab_bladder_1.pdf")


set.seed(1)
Stab_bladder.2 = StabilityPlot(X = x_bladder.2, Y =  y_bladder.2, NIter = 100)
pheatmap(Stab_bladder.2, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE,
         height = 30, width = 60, fontsize = 20, filename = "stab_bladder_2.pdf")


