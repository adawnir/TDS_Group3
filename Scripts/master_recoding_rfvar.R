### TDS Project -- MASTER Recoding risk factor variables
### Programme created by Rin Wada on 22 Feb 2021 reviewed on 24 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

case_control=readRDS("../Results/case_control.rds")
recode_covar=readRDS("../Results/recoded_covar.rds")

# Filter study population
recode_covar=recode_covar[which(recode_covar$eid %in% case_control$eid),]

### Recoding variables with multiple records ---
# check for missing values for first instances
recode_covar %>% select(contains(".0.")) %>% apply(., 2, function(x) sum(is.na(x)))

# Variables with multiple instances
# Current employment status: 6142
# Education: 6138
# Illness of father and mother: 20107 and 20110

# Extract first instances
mydata=recode_covar %>% select(eid,contains(".0."))

# Employment
mydata %>% select(matches("X6142.")) %>% unlist %>% table
table(mydata$X6142.0.0) # Need to check using all rest of columns
tmp=mydata %>% select(eid,matches("X6142."))
eidEmployed=apply(tmp[,-1], 2,
                  function(x) tmp$eid[grep("In paid employment or self-employed",x)]) %>% unlist
eidActive=apply(tmp[,-1], 2,
                function(x) tmp$eid[grep("Doing unpaid or voluntary work|Full or part-time student|Looking after home and/or family",
                               x)]) %>% unlist
eidRetired=apply(tmp[,-1], 2,
                function(x) tmp$eid[grep("Retired",x)]) %>% unlist
eidUnemployed=apply(tmp[,-1], 2,
                 function(x) tmp$eid[grep("Unemployed",x)]) %>% unlist
eidUnable=apply(tmp[,-1], 2,
                    function(x) tmp$eid[grep("Unable to work because of sickness or disability",
                                             x)]) %>% unlist
# RULE: if paid work = Employed
# RULE: if unpaid work, student or caretaker = Active
# RULE: if retired and not active = Retired
# RULE if unemployed and not retired or active = Unemployed
# RULE if not active and unable to work = Unable to work
# RULE if Prefer not to answer, None of the above or NA = NA
mydata$X6142.0.0=ifelse(mydata$eid %in% eidEmployed, "Employed",
                        ifelse(mydata$eid %in% eidActive, "Active",
                               ifelse(mydata$eid %in% eidRetired, "Retired",
                                      ifelse(mydata$eid %in% eidUnemployed, "Unemployed",
                                             ifelse(mydata$eid %in% eidUnable, "Unable to work",
                                                    NA)))))
barplot(table(mydata$X6142.0.0, useNA = "ifany"))

# Parental history
mydata %>% select(matches("X20107.|X20110.")) %>% unlist %>% table

group1=c("Chronic bronchitis/emphysema","Diabetes","High blood pressure","Stroke","Heart disease")
names(group1)=c("COPD", "diabetes","hypertension","stroke","CHD")
group1NA=c("Prefer not to answer (group 1)", "Do not know (group 1)")
group2=c("Breast cancer","Bowel cancer","Lung cancer","Prostate cancer")
names(group2)=c("breast","bowel","lung","prostate")
group2NA=c("Prefer not to answer (group 2)", "Do not know (group 2)")

# Make empty columns
tmp=mydata %>% select(eid)
diseases=c(group1,group2)
x=matrix(0,nrow=nrow(tmp),ncol=length(diseases))
colnames(x)=paste0("parent","_",names(diseases))
tmp=cbind(tmp,x)

# Subset columns with information
parent_data=mydata %>% select(eid,matches("X20107.|X20110."))
missing=parent_data$eid[rowSums(is.na(parent_data)) == ncol(parent_data)] # No missing information

# Find eids with parental history for each diseases
for (d in 1:length(diseases)){
  print(names(diseases)[d])
  disease=diseases[d]
  myeids=apply(parent_data[,-1],2,function(x) parent_data$eid[grep(disease, x)]) %>% unlist
  if (disease %in% group1){
    tomatch=group1NA
  }
  if (disease %in% group2){
    tomatch=group2NA
  }
  myNA_father=apply(parent_data[,2:11],2,function(x) parent_data$eid[x %in% tomatch]) %>% unlist
  myNA_mother=apply(parent_data[12:22],2,function(x) parent_data$eid[x %in% tomatch]) %>% unlist
  myNA=intersect(myNA_father,myNA_mother)
  myeids=unique(myeids)
  colindex=grep(names(diseases)[d], colnames(tmp))
  tmp[tmp$eid %in% myeids,colindex]=1
  tmp[tmp$eid %in% myNA,colindex]=NA
}

# Check and add to mydata
summary(tmp) # Missing data: 28766 for Group 1; 26339 for Group 2 diseases
mydata=inner_join(mydata,tmp,by="eid")

### Select and rename columns----
str(mydata)
mydata=mydata %>% select(eid,ends_with(".0.0"),starts_with("parent"),-matches("X20107.|X20110."))
str(mydata)
colnames(mydata)=c("eid","age_baseline", "sex","ethnic","townsend","employment","education",
                   "type_accom","own_rent","num_hh","hh_income","bmi","o_fish","no_fish",
                   "pro_meat","poultry","beef","lamb","pork","cook_veg","salad","fresh_fru",
                   "dried_fru","salt","tea","coffee","type_coffee","water","mat_smoke","smoking",
                   "curr_smoke","smoke_hh","alcohol","num_walk","num_mod","num_vig","sleep",
                   "no2","nox","pm10","pm2.5_absorb","pm2.5","pm2.5_10","close_major_rd",
                   "num_med",colnames(x))
str(mydata)
saveRDS(mydata, "../Results/recoded_covar_1col.rds")

### Load first instances of recoded covariates ----
mydata=readRDS("../Results/recoded_covar_ins1.rds")
# Make copy
covar=mydata

### Recode ethnicity: White, non-white ----
table(mydata$ethnic, useNA = "ifany")
covar$ethnic[mydata$ethnic %in% c("White","British","Irish","Any other white background")]="White"
covar$ethnic[mydata$ethnic %in% c("Do not know","Prefer not to answer")]=NA
covar$ethnic=ifelse(covar$ethnic=="White",covar$ethnic,
                     ifelse(is.na(covar$ethnic),covar$ethnic,"Non-white"))
table(covar$ethnic, useNA = "ifany")

### Recode education: High, intermediate, low ----
table(mydata$education, useNA = "ifany")
covar$education[mydata$education=="College or University degree"]="High"
covar$education[mydata$education %in% c("A levels/AS levels or equivalent",
                                        "O levels/GCSEs or equivalent",
                                        "CSEs or equivalent",
                                        "NVQ or HND or HNC or equivalent",
                                        "Other professional qualifications eg: nursing, teaching")]="Intermediate"
covar$education[mydata$education=="None of the above"]="Low"
covar$education[mydata$education=="Prefer not to answer"]=NA
table(covar$education, useNA = "ifany")

### Recode Type of accommodation: House, Flat, Other ----
table(mydata$type_accom, useNA = "ifany")
covar$type_accom[mydata$type_accom=="A flat, maisonette or apartment"]="Flat"
covar$type_accom[mydata$type_accom=="A house or bungalow"]="House"
covar$type_accom[mydata$type_accom %in% c("Mobile or temporary structure (i.e. caravan)",
                                          "Sheltered accommodation",
                                          "Care home","None of the above")]="Other"
covar$type_accom[mydata$type_accom=="Prefer not to answer"]=NA
table(covar$type_accom, useNA = "ifany")

### Recode Own/rent accommodation: Own, Rent, Other ----
table(mydata$own_rent, useNA = "ifany") # -1 is likely to be "Do not know"
covar$own_rent[mydata$own_rent %in% c("Own outright (by you or someone in your household)",
                                       "Own with a mortgage")]="Own"
covar$own_rent[mydata$own_rent %in% c("Rent - from local authority, local council, housing association",
                                       "Rent - from private landlord or letting agency")]="Rent"
covar$own_rent[mydata$own_rent %in% c("Pay part rent and part mortgage (shared ownership)",
                                       "Live in accommodation rent free",
                                       "None of the above")]="Other"
covar$own_rent[mydata$own_rent %in% c("Prefer not to answer", "-1")]=NA
table(covar$own_rent, useNA = "ifany")

### Recode Avgerage total household income before tax ----
table(mydata$hh_income, useNA = "ifany")
covar$hh_income[mydata$hh_income == "Less than 18,000"]="<18,000" 
covar$hh_income[mydata$hh_income == "18,000 to 30,999"]="18,000-30,999" 
covar$hh_income[mydata$hh_income == "31,000 to 51,999"]="31,000-51,999" 
covar$hh_income[mydata$hh_income %in% c("52,000 to 100,000",
                                         "Greater than 100,000")]=">52,000"
covar$hh_income[mydata$hh_income %in% c("Prefer not to answer",
                                             "Do not know")]=NA
table(covar$hh_income, useNA = "ifany")

### Recode BMI by bands ----
summary(mydata$bmi)
covar$bmi=ifelse(is.na(covar$bmi),NA,
                 ifelse(covar$bmi < 18.5,"Underweight",
                        ifelse(covar$bmi >= 18.5 & covar$bmi <25, "Normal",
                               ifelse(covar$bmi >= 25 & covar$bmi <30, "Pre-obesity",
                                      ifelse(covar$bmi >= 30 & covar$bmi <35, "Obesity class I",
                                             ifelse(covar$bmi >= 35 & covar$bmi <40, "Obesity class II",
                                                    "Obesity class III"))))))
table(covar$bmi,useNA = "ifany")

### Composite Scores Meat ----

# Total meat: Poultry, Oily fish, Non-oily fish, Beef, Pork, Lamb/Mutton, Processed meat
table(mydata$o_fish,useNA = "ifany")
table(mydata$no_fish,useNA = "ifany")
table(mydata$poultry,useNA = "ifany")
table(mydata$pro_meat,useNA = "ifany")
table(mydata$beef,useNA = "ifany")
table(mydata$lamb,useNA = "ifany")
table(mydata$pork,useNA = "ifany")

# Replace "Do not know"
tmp=covar %>% select(o_fish,no_fish,poultry,pro_meat,beef,lamb,pork) %>%
  na_if("Do not know") %>% na_if("Prefer not to answer")
covar[,c("o_fish","no_fish","poultry","pro_meat","beef","lamb","pork")]=tmp

table(covar$o_fish,useNA = "ifany")
table(covar$no_fish,useNA = "ifany")
table(covar$poultry,useNA = "ifany")
table(covar$pro_meat,useNA = "ifany")
table(covar$beef,useNA = "ifany")
table(covar$lamb,useNA = "ifany")
table(covar$pork,useNA = "ifany")

# Recode to numeric
level_key <- c("Never"=0,
               "Less than once a week"=1,
               "Once a week"=2,
               "2-4 times a week"=3,
               "5-6 times a week"=4,
               "Once or more daily"=5)
tmp=covar %>% select(o_fish, no_fish, poultry,pro_meat,beef,lamb,pork)
rownames(tmp)=covar$eid                             
tmp=tmp %>% mutate_all(~recode(., !!!level_key,.default = NaN))
summary(tmp)

# Scale (z-score)
tmp=scale(tmp,center=T,scale=T)

# Total meat
total_meat=as.data.frame(rowMeans(tmp[,c("o_fish","no_fish","poultry","pro_meat","beef","lamb","pork")]))
tmp=cbind(tmp,total_meat)
colnames(tmp)[ncol(tmp)]="total_meat"
summary(tmp)

# White meat
white_meat=as.data.frame(rowMeans(tmp[,c("o_fish","no_fish","poultry")]))
tmp=cbind(tmp,white_meat)
colnames(tmp)[ncol(tmp)]="white_meat"
summary(tmp)

# Red meat
red_meat=as.data.frame(rowMeans(tmp[,c("pro_meat","beef","lamb","pork")]))
tmp=cbind(tmp,red_meat)
colnames(tmp)[ncol(tmp)]="red_meat"
summary(tmp)

# Add back to main data set
covar=cbind(covar, tmp[,c("total_meat","white_meat","red_meat")])

### Recode Processed meat intake: Never, Less than once a week, Once a week, More than once a week
table(covar$pro_meat,useNA = "ifany")
covar$pro_meat=ifelse(is.na(covar$pro_meat)|covar$pro_meat%in%c("Never",
                                                                "Less than once a week",
                                                                "Once a week"),
                      covar$pro_meat,
                      "More than once a week")
table(covar$pro_meat,useNA = "ifany")

### Composite Scores Fruit/Vegetable ----
# Total fruit/vegetable intake
table(mydata$cook_veg,useNA = "ifany")
table(mydata$salad,useNA = "ifany")
table(mydata$fresh_fru,useNA = "ifany")
table(mydata$dried_fru,useNA = "ifany")

# Replace "Do not know" and "Prefer not to answer" and Code less than one as zero
tmp=covar %>% select(cook_veg,salad,fresh_fru,dried_fru) %>%
  na_if("Do not know") %>% na_if("Prefer not to answer") %>%
  mutate(across(c(cook_veg,salad,fresh_fru,dried_fru), recode, "Less than one"="0")) %>%
  mutate(across(c(cook_veg,salad,fresh_fru,dried_fru), as.numeric))

covar[,c("cook_veg","salad","fresh_fru","dried_fru")]=tmp
summary(covar[,c("cook_veg","salad","fresh_fru","dried_fru")])

# Scale (z-scores)
tmp=covar %>% select(cook_veg,salad,fresh_fru,dried_fru)
rownames(tmp)=covar$eid                             
summary(tmp)
tmp=scale(tmp,center=T,scale=T)

# Total score
total_fru_veg=as.data.frame(rowMeans(tmp[,c("cook_veg","salad","fresh_fru","dried_fru")]))
summary(total_fru_veg)

# Add back to main data set
covar=cbind(covar, total_fru_veg)
colnames(covar)[ncol(covar)]="total_fru_veg"

### Recode Salt intake: Never/rarely, Sometimes, Usually, Always
table(mydata$salt,useNA = "ifany")
covar$salt[mydata$salt=="Prefer not to answer"]=NA
table(covar$salt,useNA = "ifany")

### Recode Coffee intake: None, ≤1, 2-3, ≥4 ----
table(mydata$coffee,useNA = "ifany")
covar$coffee=ifelse(is.na(covar$coffee)|covar$coffee %in% c("Don't know",
                                                            "Prefer not to answer"), NA,
                     ifelse(covar$coffee==0,"0",
                            ifelse(covar$coffee %in% c("Less than one", "1"),"≥1",
                                   ifelse(covar$coffee %in% c("2","3"), "2-3", "4 or more"))))
table(covar$coffee,useNA = "ifany")

### Recode Tea intake: Same as coffee ----
table(mydata$tea,useNA = "ifany")
covar$tea=ifelse(is.na(covar$tea)|covar$tea %in% c("Don't know","Prefer not to answer"), NA,
                 ifelse(covar$tea==0,"0",
                        ifelse(covar$tea %in% c("Less than one", "1"),"≥1",
                               ifelse(covar$tea %in% c("2","3"), "2-3", "4 or more"))))
table(covar$tea,useNA = "ifany")

### Recode Water intake: Same as coffee ----
table(mydata$water,useNA = "ifany")
covar$water=ifelse(is.na(covar$water)|covar$water %in% c("Don't know", "Prefer not to answer"), NA,
                   ifelse(covar$water==0,"0",
                          ifelse(covar$water %in% c("Less than one", "1"),"≥1",
                                 ifelse(covar$water %in% c("2","3"), "2-3", "4 or more"))))
table(covar$water,useNA = "ifany")

### Recoding Maternal smoking: No/Yes ----
table(mydata$mat_smoke, useNA = "ifany")
covar$mat_smoke=ifelse(covar$mat_smoke %in% c("Do not know","Prefer not to answer"),NA,
                       covar$mat_smoke)
table(covar$mat_smoke, useNA = "ifany")

### Recoding Smoking status: Never, Previous Current ----
table(mydata$smoking, useNA = "ifany")
covar$smoking[mydata$smoking == "Prefer not to answer"]=NA
table(covar$smoking, useNA = "ifany")

### Recoding Smoking in household: Current smoker, 1 household member, >1 household member, No ----
table(mydata$smoke_hh, useNA = "ifany")
table(mydata$curr_smoke, useNA = "ifany")
covar$smoke_hh[mydata$smoke_hh == "Prefer not to answer"]=NA
covar$smoke_hh[mydata$curr_smoke == "Yes, on most or all days"]="Current smoker"
covar$smoke_hh[mydata$smoke_hh == "Yes, one household member smokes"]="1 household member"
covar$smoke_hh[mydata$smoke_hh == "Yes, more than one household member smokes"]=">1 household member"
table(covar$smoke_hh, useNA = "ifany")

### Recode Alcohol intake frequency ----
table(mydata$alcohol, useNA = "ifany")
covar$alcohol[mydata$alcohol == "Prefer not to answer"]=NA
covar$alcohol[mydata$alcohol %in% c("Daily or almost daily",
                                    "Three or four times a week")]='Regularly'
covar$alcohol[mydata$alcohol %in% c("One to three times a month",
                                    "Once or twice a week")]='Occassionally'
covar$alcohol[mydata$alcohol %in% c("Special occasions only",
                                    "Never")]='Never/Rarely'
table(covar$alcohol, useNA = "ifany")

### Recoding physical activity ----
table(mydata$num_walk, useNA = "ifany")
table(mydata$num_mod, useNA = "ifany")
table(mydata$num_vig, useNA = "ifany")

tmp=covar %>% select(num_walk,num_mod,num_vig) %>%
  na_if("Do not know") %>% na_if("Prefer not to answer") %>%
  mutate(num_walk=recode(num_walk,"Unable to walk"="0")) %>%
  mutate(across(c(num_walk,num_mod,num_vig), as.numeric))

covar[,c("num_walk","num_mod","num_vig")]=tmp
summary(covar[,c("num_walk","num_mod","num_vig")])

### Recoding Sleep duration (hours) per 24 hours
table(mydata$sleep, useNA = "ifany")
tmp=covar %>% select(sleep) %>%
  na_if("Do not know") %>% na_if("Prefer not to answer") %>%
  mutate(sleep=as.numeric(sleep))
summary(tmp$sleep)
covar$sleep=tmp

### Recoding Number of medications: 0, 1, >1 ----
summary(covar$num_med) # 860 missing
covar$num_med_cat=ifelse(is.na(covar$num_med), NA,
                         ifelse(covar$num_med==0, "0",
                                ifelse(covar$num_med==1,"1",">1")))
table(covar$num_med_cat, useNA = "ifany")

### Restructuring data
str(covar)
covar2=covar %>% select(eid,age_baseline,sex,ethnic,townsend,employment,education,type_accom,
                        own_rent,num_hh,hh_income,bmi,total_meat,white_meat,red_meat,pro_meat,
                        total_fru_veg,salt,tea,coffee,water,alcohol,smoking,mat_smoke,smoke_hh,
                        num_walk,num_mod,num_vig,sleep,no2,nox,pm10,pm2.5_absorb,pm2.5,pm2.5_10,
                        close_major_rd,num_med,starts_with("parent_")) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(vars(starts_with("parent_")), as.factor)
str(covar2)
saveRDS(covar2, "../Results/covar_without_comorbid.rds")
  
### Combine with comorbid data
covar2=readRDS("../Results/covar_without_comorbid.rds")
comorbid=readRDS("../Results/comorbid.rds")

covar3=inner_join(covar2,comorbid,by="eid")
str(covar3)
saveRDS(covar3, "../Results/covar_master.rds")
