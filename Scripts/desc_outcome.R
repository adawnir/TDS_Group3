### TDS Project -- Descriptive analysis of outcomes
## Programme created by Rin Wada on 12 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

# Load data set
case_control=readRDS("../Results/case_control.rds")

# Filter cases
cases=case_control %>% filter(case_status!="control")

# Covert days to years
cases$time_diag_years=cases$time_diag_days/365.25

# Histogram
# Mean time to diag
mu=cases %>%
  group_by(case_status) %>%
  summarise(grp.mean=mean(time_diag_years))
p=ggplot(cases, aes(x=time_diag_years, color=case_status)) +
  geom_histogram(fill="white", position="identity", bins=50)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=case_status), linetype="dashed")+
  labs(title = "Lung vs Bladder cancer: Time to diagnosis",
       x="Years",
       y="Frequency",
       color="Type of cancer") +
  theme_minimal()
p

# Mean age_diag
mu=cases %>%
  group_by(case_status) %>%
  summarise(grp.mean=mean(age_diag))
p=ggplot(cases, aes(x=age_diag, color=case_status)) +
  geom_histogram(fill="white", position="identity", bins=50)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=case_status), linetype="dashed")+
  labs(title = "Lung vs Bladder cancer: Age at diagnosis",
       x="Years",
       y="Frequency",
       color="Type of cancer") +
  theme_minimal()
p



