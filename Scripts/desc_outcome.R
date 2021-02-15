### TDS Project -- Descriptive analysis of outcomes
## Programme created by Rin Wada on 12 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

# Load data set
case_control=readRDS("../Results/case_control.rds")

# Covert days to years
case_control$time_diag_years=case_control$time_diag_days/365.25
# Interleaved histograms
ggplot(case_control, aes(x=time_diag_years, color=case_status)) +
  geom_histogram(fill="white", position="dodge")+
  theme(legend.position="top")
# Add mean lines
mu=case_control %>%
  group_by(case_status) %>%
  filter(case_status!=0) %>%
  summarise(grp.mean=mean(time_diag_years))
p=ggplot(case_control, aes(x=time_diag_years, fill=case_status, color=case_status)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=case_status),
             linetype="dashed")+
  theme_minimal()
p



