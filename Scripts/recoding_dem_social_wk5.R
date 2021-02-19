### TDS Project -- Recoding  demographic + social factors (BMI, RACE, CURRENT EMPLOYMENT)
## created by Ines Gerard-Ursin on Monday 15 February

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(data.table)
library(tidyverse)

recoded_covar <- readRDS("/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Results/recoded_covar.rds")
dems <- recoded_covar %>% # demographics
                    select(eid, X21022.0.0, X31.0.0, X189.0.0, X21001.0.0, X6142.0.0, X21000.0.0, X670.0.0, X680.0.0, X6138.0.0, X738.0.0) 
# age: 21022
# sex: 31
# townsend: 189
# BMI: 21001
# current employment : 6142
# ethnic group: 21000
# type of accommodation: 670
# own/rent accommodation : 680
# education/qualifications : 6138
# avg total household income? : 738
# for everyone: Prefer not to answer, NA, Do not know -> NA

# my job is to recode demographics:
#### recode BMI by bands ----

dems$BMI[dems$X21001.0.0 < 18.5]  <- "Underweight (<18.5)"
dems$BMI[dems$X21001.0.0 >= 18.5 & dems$X21001.0.0 <25]  <- "Normal Weight (18.5-24.9)"
dems$BMI[dems$X21001.0.0 >= 25 & dems$X21001.0.0 <30]  <- "Pre-obesity (25-29.9)"
dems$BMI[dems$X21001.0.0 >= 30 & dems$X21001.0.0 <35]  <- "Obesity class I (30-34.9)"
dems$BMI[dems$X21001.0.0 >= 35 & dems$X21001.0.0 <40]  <- "Obesity class II (35-39.9)"
dems$BMI[dems$X21001.0.0 >= 40]  <- "Obesity class III (Above 40)"
dems$BMI[is.na(dems$X21001.0.0)] <- NA

dems$BMI <- factor(dems$BMI)

#### recode race: white, black, asian, mixed/other, na -----

dems$ethnic[dems$X21000.0.0 == "White" 
            | dems$X21000.0.0 == "British" 
            | dems$X21000.0.0 == "Irish"
            | dems$X21000.0.0 == "Any other white background"]  <- "White"
dems$ethnic[dems$X21000.0.0 == "NA" 
            | dems$X21000.0.0 == "Do not know"
            | is.na(dems$X21000.0.0)
            | dems$X21000.0.0 == "Prefer not to answer"]  <- NA
dems$ethnic[dems$X21000.0.0 == "Asian or Asian British" 
              | dems$X21000.0.0 == "Indian" 
              | dems$X21000.0.0 == "Pakistani" 
              | dems$X21000.0.0 == "Any other Asian background" 
              | dems$X21000.0.0 == "Chinese"] <- "Asian"
dems$ethnic[dems$X21000.0.0 == "Black or Black British" 
            | dems$X21000.0.0 == "Caribbean" 
            | dems$X21000.0.0 == "African" 
            | dems$X21000.0.0 == "Any other Black background"] <- "Black"
dems$ethnic[dems$X21000.0.0 == "White and Black Caribbean" 
            | dems$X21000.0.0 == "White and Black African" 
            | dems$X21000.0.0 == "White and Asian" 
            | dems$X21000.0.0 == "Any other mixed background" 
            | dems$X21000.0.0 == "Other ethnic group"] <- "Mixed/Other"


dems$ethnic <- factor(dems$ethnic)

#### recode current employment status: active, unemployed, retired, NA -----

dems$employment[dems$X6142.0.0 == "In paid employment or self-employed" 
                | dems$X6142.0.0 == "Looking after home and/or family"
                | dems$X6142.0.0 == "Doing unpaid or voluntary work"
                | dems$X6142.0.0 == "Full or part-time student"] <- "Active"
dems$employment[dems$X6142.0.0 == "Retired"] <- "Retired"
dems$employment[dems$X6142.0.0 == "Unemployed"
                | dems$X6142.0.0 == "Unable to work because of sickness or disability"] <- "Unemployed"
dems$employment[dems$X6142.0.0 == "None of the above"
                | dems$X6142.0.0 == "Prefer not to answer"
                | dems$X6142.0.0 == "NA"
                | is.na(dems$X6142.0.0)] <- NA

dems$employment <- factor(dems$employment)

### type of accommodation: 670 ----

dems$accommodation[dems$X670.0.0 == "A flat, maisonette or apartment"] <- "Flat"
dems$accommodation[dems$X670.0.0 == "A house or bungalow"] <- "House"
dems$accommodation[dems$X670.0.0 == "Mobile or temporary structure (i.e.caravan)"
                   | dems$X670.0.0 == "Sheltered accommodation"
                   | dems$X670.0.0 == "Care home"] <- "Other"
dems$accommodation[dems$X670.0.0 == "None of the above"
                   | dems$X670.0.0 == "Prefer not to answer"
                   | dems$X670.0.0 == "NA"
                   | is.na(dems$X670.0.0)] <- NA


dems$accommodation <- factor(dems$accommodation)

### own/rent accommodation : 680 ----
dems$own.rent[dems$X680.0.0 == "Own outright (by you or someone in your household"
              | dems$X680.0.0 == "Own with a mortgage"] <- "Own"
dems$own.rent[dems$X680.0.0 == "Rent - from local authority, local council, housing association"
              | dems$X680.0.0 == "Rent - from private landlord or letting agency"] <- "Rent"
dems$own.rent[dems$X680.0.0 == "Pay part rent and part mortgage (shared ownership)"
                   | dems$X680.0.0 == "Live in accommodation rent free"] <- "Other"
dems$own.rent[dems$X680.0.0 == "None of the above"
                   | dems$X680.0.0 == "Prefer not to answer"
                   | dems$X680.0.0 == "NA"
                   | is.na(dems$X680.0.0)] <- NA

dems$own.rent <- factor(dems$own.rent)



### education/qualifications : 6138 ---- high, intermediate, low
dems$education[dems$X6138.0.0 == "College or University degree"] <- "High"
dems$education[dems$X6138.0.0 == "A levels/AS levels or equivalent"
               | dems$X6138.0.0 == "O levels/GCSEs or equivalent"
               | dems$X6138.0.0 == "CSEs or equivalent"
               | dems$X6138.0.0 == "NVQ or HND or HNC or equivalent"
               | dems$X6138.0.0 == "Other professional qualifications eg: nursing, teaching"] <- "Intermediate"
dems$education[dems$X6138.0.0 == "None of the above"] <- "Low"
dems$education[dems$X680.0.0 == "Prefer not to answer"
              | dems$X680.0.0 == "NA"
              | dems$X680.0.0 == "Do not know"
              | is.na(dems$X680.0.0)] <- NA

dems$education <- factor(dems$education)


### avg total household income? : 738 ---- >52000 grouped together
dems$householdincome[dems$X738.0.0 == "Less than 18,000"] <- "<18,000" 
dems$householdincome[dems$X738.0.0 == "18,000 to 30,999"] <- "18,000-30,999" 
dems$householdincome[dems$X738.0.0 == "31,000 to 51,999"] <- "31,000-51,999" 
dems$householdincome[dems$X738.0.0 == "52,000 to 100,000"
                    | dems$X738.0.0 == "Greater than 100,000"] <- ">52,000"
dems$householdincome[dems$X738.0.0 == "Prefer not to answer"
               | dems$X738.0.0 == "NA"
               | dems$X738.0.0 == "Do not know"
               | is.na(dems$X738.0.0)] <- NA

dems$householdincome <- factor(dems$householdincome)


## lets rename things so they have names that make sense
dems$gender <- factor(dems$X31.0.0)
dems$age <- dems$X21022.0.0
dems$townsend <- dems$X189.0.0

### save recoded demographic + social factor----
demographics_social=dems %>% select(eid, BMI, ethnic, employment, gender, age, townsend, householdincome, own.rent, accommodation, education)

saveRDS(demographics_social, "../Results/dems_social.rds")
