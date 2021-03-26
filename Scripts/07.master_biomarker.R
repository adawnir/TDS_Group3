### TDS Project -- Biomarker data processing
## Programme created by Vivian and reviewed by Rin Wada on 24 and 25 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)

### Processing ----
# Load data sets
biomarker=readRDS("../Results/extract_biomarkers.rds")
case_control=readRDS("../Results/case_control.rds")

## Select eid of study population 
biomarker=biomarker[which(biomarker$eid %in% case_control$eid),]

## Remove two biomarkers with high missing rate (Oestradiol/Rheumatoid_factor)
biomarker=biomarker %>% select(-Oestradiol, -`Rheumatoid factor`)
str(biomarker)

## Log 2 transformation
biomarker[-1]=log2(biomarker[-1])

# Density plots
tmp=biomarker[,-1]
pdf("../Figures/biomarker_distribution.pdf")
par(mfrow = c(4, 7), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 1:ncol(tmp [,-1])){
  xfull=density(tmp[,k], na.rm = T)
  plot(xfull, col="skyblue", xlab="", ylab="", xaxt="n", main=colnames(tmp)[k],
       cex.main = 0.5, cex.axis=0.5, tck=-0.05)
}
dev.off()

saveRDS(biomarker,"../Results/biomarker_master.rds")
