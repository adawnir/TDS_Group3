# joining datasets
# written by Ines on the 9th of February
# revisited on 16th Feb

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# Load packages
library(openxlsx)
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggcorrplot)

# for continuous - look at median & mean, distribution for anything weird
# for categorical - histograms! also proportions
# missing values per cases and controls

# read in the data
case_control <- readRDS("/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Results/case_control.rds")
recoded_covar <- readRDS("/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Results/recoded_covar.rds")

# join the tables
case_control_covars <- left_join(x=case_control, y=recoded_covar, by="eid", all.x = TRUE)
head(recoded_covar$eid)
head(case_control$eid)

# have to recode all characters as factors
case_control_covars <- as.data.frame(unclass(case_control_covars))

# using the aggregate function, you can look at all individual covariates by case control status
Table <- aggregate(case_control_covars$X738.0.0, list(case_control_covars$lung, case_control_covars$bladder, case_control_covars$control),FUN=summary)

# produce: table of p values: once adjusted for age + sex, once not

##### Trying to do summary table here - it didnt work -----
# do I have to combine lung, bladder, and control status?
# right now I will copy & paste that other code and not do that

summary_covars <-
  list("Age at recruitment" =
         list("min"       = ~ min(case_control_covars$X21022.0.0),
              "max"       = ~ max(case_control_covars$X21022.0.0),
              "mean (sd)" = ~ qwraps2::mean_sd(case_control_covars$X21022.0.0)),
       "Sex" =
         list("Female"  = ~ qwraps2::n_perc0(case_control_covars$X31.0.0 == "Female"),
              "Male"    = ~ qwraps2::n_perc0(case_control_covars$X31.0.0 == "Male")),
       "Townsend Deprivation Index" =
         list("min"       = ~ min(case_control_covars$X189.0.0),
              "max"       = ~ max(case_control_covars$X189.0.0),
              "median"    = ~ median(case_control_covars$X189.0.0),
              "mean (sd)" = ~ qwraps2::mean_sd(case_control_covars$X189.0.0)),
       "BMI" =
         list("min"       = ~ min(case_control_covars$X21001.0.0),
              "max"       = ~ max(case_control_covars$X21001.0.0),
              "median"    = ~ median(case_control_covars$X21001.0.0),
              "mean (sd)" = ~ qwraps2::mean_sd(case_control_covars$X21001.0.0)),
       "Current Employment Status" =
         list("Doing unpaid or voluntary work"  = ~ qwraps2::n_perc0(case_control_covars$X6142.0.0 == "Doing unpaid or voluntary work"),
              "Full or part-time student"  = ~ qwraps2::n_perc0(case_control_covars$X6142.0.0 == "Full or part-time student"),
              "In paid employment or self-employed"  = ~ qwraps2::n_perc0(case_control_covars$X6142.0.0 == "In paid employment or self-employed"),
              "Looking after home and/or family"  = ~ qwraps2::n_perc0(case_control_covars$X6142.0.0 == "Looking after home and/or family"),
              "None of the above"  = ~ qwraps2::n_perc0(case_control_covars$X6142.0.0 == "None of the above"),
              "Prefer not to answer"  = ~ qwraps2::n_perc0(case_control_covars$X6142.0.0 == "Prefer not to answer"),
              "Retired"  = ~ qwraps2::n_perc0(case_control_covars$X6142.0.0 == "Retired"),
              "Unable to work because of sickness or disability"  = ~ qwraps2::n_perc0(case_control_covars$X6142.0.0 == "Unable to work because of sickness or disability"),
              "Unemployed"  = ~ qwraps2::n_perc0(case_control_covars$X6142.0.0 == "Unemployed"),
              "NA's"  = ~ qwraps2::n_perc0(case_control_covars$X6142.0.0 == "NA's")),
       "Qualifications" =
         list("A levels/AS levels or equivalent"  = ~ qwraps2::n_perc0(case_control_covars$X6138.0.0 == "A levels/AS levels or equivalent"),
              "College or University degree"  = ~ qwraps2::n_perc0(case_control_covars$X6138.0.0 == "College or University degree"),
              "CSEs or equivalent"  = ~ qwraps2::n_perc0(case_control_covars$X6138.0.0 == "CSEs or equivalent"),
              "None of the above"  = ~ qwraps2::n_perc0(case_control_covars$X6138.0.0 == "None of the above"),
              "NVQ or HND or HNC or equivalent"  = ~ qwraps2::n_perc0(case_control_covars$X6138.0.0 == "NVQ or HND or HNC or equivalent"),
              "O levels/GCSEs or equivalent"  = ~ qwraps2::n_perc0(case_control_covars$X6138.0.0 == "O levels/GCSEs or equivalent"),
              "Other professional qualifications eg: nursing, teaching"  = ~ qwraps2::n_perc0(case_control_covars$X6138.0.0 == "Other professional qualifications eg: nursing, teaching"),
              "Prefer not to answer"  = ~ qwraps2::n_perc0(case_control_covars$X6138.0.0 == "Prefer not to answer"),
              "NA's"  = ~ qwraps2::n_perc0(case_control_covars$X6138.0.0 == "NA's")),
       "Type of accomodation lived in" =
         list("A flat, maisonette or apartment"  = ~ qwraps2::n_perc0(case_control_covars$X670.0.0 == "A flat, maisonette or apartment"),
              "A house or bungalow"  = ~ qwraps2::n_perc0(case_control_covars$X670.0.0 == "A house or bungalow"),
              "Care home"  = ~ qwraps2::n_perc0(case_control_covars$X670.0.0 == "Care home"),
              "Mobile or temporary structure (i.e. caravan)"  = ~ qwraps2::n_perc0(case_control_covars$X670.0.0 == "Mobile or temporary structure (i.e. caravan)"),
              "Sheltered accommodation"  = ~ qwraps2::n_perc0(case_control_covars$X670.0.0 == "Sheltered accommodation"),
              "None of the above"  = ~ qwraps2::n_perc0(case_control_covars$X670.0.0 == "None of the above"),
              "Prefer not to answer"  = ~ qwraps2::n_perc0(case_control_covars$X670.0.0 == "Prefer not to answer"),
              "NA's"  = ~ qwraps2::n_perc0(case_control_covars$X670.0.0 == "NA's")),
       "Own or Rent accomodation lived in" =
         list("Live in accommodation rent free"  = ~ qwraps2::n_perc0(case_control_covars$X680.0.0 == "Live in accommodation rent free"),
              "Own outright (by you or someone in your household)"  = ~ qwraps2::n_perc0(case_control_covars$X680.0.0 == "Own outright (by you or someone in your household)"),
              "Own with a mortgage"  = ~ qwraps2::n_perc0(case_control_covars$X680.0.0 == "Own with a mortgage"),
              "Pay part rent and part mortgage (shared ownership)"  = ~ qwraps2::n_perc0(case_control_covars$X680.0.0 == "Pay part rent and part mortgage (shared ownership)"),
              "Rent - from local authority, local council, housing association"  = ~ qwraps2::n_perc0(case_control_covars$X680.0.0 == "Rent - from local authority, local council, housing association"),
              "Rent - from private landlord or letting agency"  = ~ qwraps2::n_perc0(case_control_covars$X680.0.0 == "Rent - from private landlord or letting agency"),
              "None of the above"  = ~ qwraps2::n_perc0(case_control_covars$X680.0.0 == "None of the above"),
              "Prefer not to answer"  = ~ qwraps2::n_perc0(case_control_covars$X680.0.0 == "Prefer not to answer"),
              "NA's"  = ~ qwraps2::n_perc0(case_control_covars$X680.0.0 == "NA's")),       
       "Average Total Household Income Before Tax" =
         list("Less than 18,000"  = ~ qwraps2::n_perc0(case_control_covars$X738.0.0 == "Less than 18,000"),
              "18,000 to 30,999"  = ~ qwraps2::n_perc0(case_control_covars$X738.0.0 == "18,000 to 30,999"),
              "31,000 to 51,999"  = ~ qwraps2::n_perc0(case_control_covars$X738.0.0 == "31,000 to 51,999"),
              "52,000 to 100,000"  = ~ qwraps2::n_perc0(case_control_covars$X738.0.0 == "52,000 to 100,000"),
              "Greater than 100,000"  = ~ qwraps2::n_perc0(case_control_covars$X738.0.0 == "Greater than 100,000"),              
              "Do not know"  = ~ qwraps2::n_perc0(case_control_covars$X738.0.0 == "Do not know"),
              "Prefer not to answer"  = ~ qwraps2::n_perc0(case_control_covars$X738.0.0 == "Prefer not to answer"),
              "NA's"  = ~ qwraps2::n_perc0(case_control_covars$X738.0.0 == "NA's")),       
       "Maternal Smoking Around Birth" =
         list("Yes"  = ~ qwraps2::n_perc0(case_control_covars$X1787.0.0 == "Yes"),
              "No"  = ~ qwraps2::n_perc0(case_control_covars$X1787.0.0 == "No"),              
              "Do not know"  = ~ qwraps2::n_perc0(case_control_covars$X1787.0.0 == "Do not know"),
              "Prefer not to answer"  = ~ qwraps2::n_perc0(case_control_covars$X1787.0.0 == "Prefer not to answer"),
              "NA's"  = ~ qwraps2::n_perc0(case_control_covars$X1787.0.0 == "NA's")), 
       "Birth Weight" =
         list("min"       = ~ min(case_control_covars$X20022.0.0),
              "max"       = ~ max(case_control_covars$X189.0.0),
              "median"    = ~ median(case_control_covars$X189.0.0),
              "mean (sd)" = ~ qwraps2::mean_sd(case_control_covars$X189.0.0)),
       "Ethnic Background" =
         list("African"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "African"),
              "Any other Asian background"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Any other Asian background"),
              "Any other Black background"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Any other Black background"),
              "Any other mixed background"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Any other mixed background"),
              "Asian or Asian British"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Asian or Asian British"),
              "Bangladeshi"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Bangladeshi"),
              "Black or Black British"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Black or Black British"),
              "British"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "British"),
              "Caribbean"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Caribbean"),
              "Chinese"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Chinese"),
              "Indian"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Indian"),
              "Irish"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Irish"),
              "Mixed"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Mixed"),
              "Pakistani"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Pakistani"),
              "White"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "White"),
              "White and Asian"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "White and Asian"),
              "White and Black African"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "White and Black African"),
              "White and Black Caribbean"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "White and Black Caribbean"),
              "Other ethnic group"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Other ethnic group"),
              "Do not know"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Do not know"),
              "Prefer not to answer"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "Prefer not to answer"),
              "NA's"  = ~ qwraps2::n_perc0(case_control_covars$X21000.0.0 == "NA's")),
       "Summed MET minutes per week" =
         list("min"       = ~ min(case_control_covars$X22040.0.0),
              "max"       = ~ max(case_control_covars$X22040.0.0),
              "median"    = ~ median(case_control_covars$X22040.0.0),
              "mean (sd)" = ~ qwraps2::mean_sd(case_control_covars$X22040.0.0)),
       "Processed meat intake" =
         list("Female"  = ~ qwraps2::n_perc0(case_control_covars$X1349.0.0 == "Female"),
              "Male"    = ~ qwraps2::n_perc0(case_control_covars$X31.0.0 == "Male")),
       "Poultry intake" =
         list("Female"  = ~ qwraps2::n_perc0(case_control_covars$X1359.0.0 == "Female"),
              "Male"    = ~ qwraps2::n_perc0(case_control_covars$X31.0.0 == "Male")),
       "Beef intake" =
         list("Female"  = ~ qwraps2::n_perc0(case_control_covars$X1369.0.0 == "Female"),
              "Male"    = ~ qwraps2::n_perc0(case_control_covars$X31.0.0 == "Male")),
       "Lamb/mutton intake" =
         list("Female"  = ~ qwraps2::n_perc0(case_control_covars$X1379.0.0 == "Female"),
              "Male"    = ~ qwraps2::n_perc0(case_control_covars$X31.0.0 == "Male")),
       "Pork intake" =
         list("Female"  = ~ qwraps2::n_perc0(case_control_covars$X1389.0.0 == "Female"),
              "Male"    = ~ qwraps2::n_perc0(case_control_covars$X31.0.0 == "Male")),
       "Coffee intake" =
         list("Female"  = ~ qwraps2::n_perc0(case_control_covars$X1498.0.0 == "Female"),
              "Male"    = ~ qwraps2::n_perc0(case_control_covars$X31.0.0 == "Male")),
       "Smoking status" =
         list("Female"  = ~ qwraps2::n_perc0(case_control_covars$X20116.0.0 == "Female"),
              "Male"    = ~ qwraps2::n_perc0(case_control_covars$X31.0.0 == "Male")),
       "Pack years of smoking" =
         list("min"       = ~ min(case_control_covars$X20161.0.0),
              "max"       = ~ max(case_control_covars$X20161.0.0),
              "median"    = ~ median(case_control_covars$X20161.0.0),
              "mean (sd)" = ~ qwraps2::mean_sd(case_control_covars$X20161.0.0)),
       "Exposure to tobacco smoke at home" =
         list("Female"  = ~ qwraps2::n_perc0(case_control_covars$X1269.0.0 == "Female"),
              "Male"    = ~ qwraps2::n_perc0(case_control_covars$X31.0.0 == "Male")),
       "Exposure to tobacco smoke outside home" =
         list("Female"  = ~ qwraps2::n_perc0(case_control_covars$X1279.0.0 == "Female"),
              "Male"    = ~ qwraps2::n_perc0(case_control_covars$X31.0.0 == "Male")),
       "Alcohol drinker status" =
         list("Female"  = ~ qwraps2::n_perc0(case_control_covars$X20117.0.0 == "Female"),
              "Male"    = ~ qwraps2::n_perc0(case_control_covars$X31.0.0 == "Male")),
       "Alcohol intake frequency" =
         list("Female"  = ~ qwraps2::n_perc0(case_control_covars$X1558.0.0 == "Female"),
              "Male"    = ~ qwraps2::n_perc0(case_control_covars$X31.0.0 == "Male")),
  )


### By case control status (hopefully this works)
by_cc_status <- summary_table(dplyr::group_by(case_control_covars, c("lung", "bladder", "control")), summary_covars)
by_cc_status