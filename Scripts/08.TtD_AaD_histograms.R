#Age at diagnosis and time to diagnosis analysis for both cancers
#Fergal

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

#Load data
mydata <- readRDS('../Results/case_control.rds')

library(ggplot2)
library(ggpubr)
library(plyr)

lung <- mydata[mydata$case_status == 'lung',]
lung$case_status <- 'Lung cancer'
bladder <- mydata[mydata$case_status == 'bladder',]
bladder$case_status <- 'Bladder cancer'

median(lung$time_diag_days)
median(bladder$time_diag_days)
IQR(lung$time_diag_days)
IQR(bladder$time_diag_days)

cases <- rbind(bladder, lung)
names(cases)[names(cases) == "case_status"] <- "Case status"

mu_ttd <- ddply(cases, "`Case status`", summarise, grp.median=median(time_diag_years))
head(mu_ttd)

p1<-ggplot(cases, aes(x=time_diag_years, fill=`Case status`, color=`Case status`)) +
  geom_histogram(position="dodge", alpha=0.5) + theme_bw() +
  xlab('Time to diagnosis (years)') + ylab('Frequency') +
  scale_fill_manual(values=c('hotpink', "darkturquoise")) +
  scale_color_manual(values=c('hotpink', "darkturquoise")) +
  geom_vline(data=mu_ttd, aes(xintercept=grp.median, color=`Case status`), linetype="dashed")

mu_aad <- ddply(cases, "`Case status`", summarise, grp.median=median(age_diag))
head(mu_aad)

p2<-ggplot(cases, aes(x=age_diag, fill=`Case status`, color=`Case status`)) +
  geom_histogram(position="dodge", alpha=0.5) + theme_bw() +
  xlab('Age at diagnosis (years)') + ylab('Frequency') +
  scale_fill_manual(values=c('hotpink', "darkturquoise")) +
  scale_color_manual(values=c('hotpink', "darkturquoise")) +
  geom_vline(data=mu_aad, aes(xintercept=grp.median, color=`Case status`), linetype="dashed")

p3 <- ggarrange(p1, p2, common.legend = TRUE, legend = "bottom")

pdf('../Figures/agetimetodiag_histograms.pdf', height = 5, width = 10)
p3
dev.off()

