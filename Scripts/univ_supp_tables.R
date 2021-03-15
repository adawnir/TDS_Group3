#Creating supplementary table of ORs and p values for the univariate models
#Ferg 10/3/21

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

#Load in the data
mhdata <- readRDS("../Results/manhattan_plot.rds")
covars <- readRDS('../Results/covar_table1.rds')
fpdata <- readRDS('../Results/forest_plot.rds')

library(gt)
library(dplyr)
library(tidyverse)
library(data.table)

mhdata <- as.data.frame(mhdata)

row.names(mhdata)[1:44] <- c('Smoking status', 'Ethnicity',
                          'Townsend deprivation index', 'Current employment status',
                          'Education', 'Type of accomodation lived in',
                          'Own or rent accomodation lived in', 'Number of people in ousehold',
                          'Average total household income', 
                          'Physical activity score', 'Sleep per 24 hours (hours)',
                          'Total meat intake score', 'White meat intake score',
                          'Red meat intake score', 'Processed meat intake',
                          'Total fruit and vegetable intake score',
                          'Salt added to food', 'Tea intake per day (cups)',
                          'Coffee intake per day (cups)', 'Water intake per day (glasses)',
                          'Alcohol intake frequency', 
                          'Maternal smoking around birth','NO2 (??g/m3)',
                          'NOx (??g/m3)', 'PM10 (??g/m3)', 'PM2.5 (absorbance/m)',
                          'PM2.5 (??g/m3)', 'PM2.5-10??m (??g/m3)',
                          'Close to major road', 'Number of medications',
                          'Parental history of COPD', 'Parental history of diabetes',
                          'Parental history of hypertension',
                          'Parental history of stroke', 'Parental history of heart disease',
                          'Parental history of breast cancer',
                          'Parental history of bowel cancer',
                          'Parental history of lung cancer',
                          'Parental history of prostate cancer',
                          'Cardiovascular disease', 'Hypertension',
                          'Diabetes', 'Respiratory disease',
                          'Autoimmune disease')

colnames(mhdata) <- c('Lung model 1', 'Lung model 2', 'Bladder model 1', 'Bladder model 2')

mhdata <- tibble::rownames_to_column(mhdata, 'Variable')

test_tbl <- gt(data = mhdata)
test_tbl

