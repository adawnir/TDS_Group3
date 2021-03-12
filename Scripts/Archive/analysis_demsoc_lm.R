# looking at demographic + social variables
# written by Ines on 16th feb

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# Load packages
library(openxlsx)
library(data.table)
library(ggplot2)
library(tidyverse)
library(broom)
library(dplyr)

# for continuous - look at median & mean, distribution for anything weird
# for categorical - histograms! also proportions
# missing values per cases and controls

# read in the data
case_control <- readRDS("/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Results/case_control.rds")
dems_social <- readRDS("/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Results/dems_social.rds")

# join the tables
cc_dems <- left_join(x=case_control, y=dems_social, by="eid", all.x = TRUE)

# case_status control=0, lung=1, bladder=2
levels(cc_dems$case_status) <- c("control","lung","bladder")

# using the aggregate function, you can look at all individual covariates by case control status
table <- aggregate(cc_dems$householdincome, list(cc_dems$case_status),FUN=summary)
table


# produce: table of p values: once adjusted for age + sex, once not
# to do:
  # recode lung, bladder, control - maybe split data?
  # lm loop? store p values in a data frame

# one hot encoding of case/control status so we can do GLMs
# DONT DO FOR LOOPS THEY BETRAY YOU


# relevel factors
cc_dems$ethnic<-relevel(cc_dems$ethnic, ref="White")
cc_dems$accommodation<-relevel(cc_dems$accommodation, ref="House")
cc_dems$own.rent<-relevel(cc_dems$own.rent, ref="Own")
cc_dems$education<-relevel(cc_dems$education, ref="Intermediate")
cc_dems$householdincome<-relevel(cc_dems$householdincome, ref="18,000-30,999")



lhs <- c('lung', 'bladder', 'control')
rhs <- c('age', 'gender','BMI', 'ethnic','townsend','employment','accommodation','own.rent','education','householdincome')

models = list()
for (i in lhs){
  for (j in rhs){
    models[[paste(i, "vs", j)]] <- glm(as.formula(paste(i, "~", j)), family = binomial(link="logit"),data = cc_dems)
  }
}
summ = lapply(models, summary, simplify = F) # glance will give me list of coefficients
glms <- dplyr::bind_rows(summ, .id = "model")
glms

summaries <- lapply(models, summary)
summaries

# extracting coefficients with p values:
pvals <- lapply(summaries, function(x) x$coefficients[, c(1,4)])
pvals
