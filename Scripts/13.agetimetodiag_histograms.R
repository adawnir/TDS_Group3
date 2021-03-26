#Age at diagnosis and time to diagnosis analysis for both cancers
#Fergal

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

#Load data
mydata <- readRDS('../Results/case_control.rds')

library(ggplot2)
install.packages('ggpubr')
library(ggpubr)

lung <- mydata[mydata$case_status == 'lung',]
bladder <- mydata[mydata$case_status == 'bladder',]

median(lung$time_diag_days)
median(bladder$time_diag_days)
IQR(lung$time_diag_days)
IQR(bladder$time_diag_days)

cases <- mydata[mydata$case_status != 'control',]
names(cases)[names(cases) == "case_status"] <- "Case status"

p1<-ggplot(cases, aes(x=time_diag_years, fill=`Case status`, color=`Case status`)) +
  geom_histogram(position="dodge", alpha=0.5) + theme_bw() +
  xlab('Time to diagnosis (years)') + ylab('Count') +
  scale_fill_manual(values=c("darkturquoise", "hotpink")) +
  scale_color_manual(values=c("darkturquoise", "hotpink"))
p1

p2<-ggplot(cases, aes(x=age_diag, fill=`Case status`, color=`Case status`)) +
  geom_histogram(position="dodge", alpha=0.5) + theme_bw() +
  xlab('Age at diagnosis (years)') + ylab('Count') +
  scale_fill_manual(values=c("darkturquoise", "hotpink")) +
  scale_color_manual(values=c("darkturquoise", "hotpink"))
p2

p <- ggarrange(p1, p2, common.legend = TRUE, legend = "bottom")
p

pdf('../Figures/agetimetodiag_histograms.pdf', height = 7, width = 20)
p
dev.off()

