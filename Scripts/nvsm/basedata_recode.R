### TDS Project - redoing base analysis with hh smoke & smoking status separated
# by ines on april 15, modification of Rin's recoding master

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(tidyverse)
library(dplyr)

### Load data with 1 column per variable ----
mydata=readRDS("../Results/recoded_covar_1col.rds")
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

### Recode Number in household: ----
summary(mydata$num_hh)
# Recode -3 and -1 to NA
covar$num_hh=ifelse(covar$num_hh<0,NA,covar$num_hh)
summary(covar$num_hh)
# Covert to category
covar$num_hh=ifelse(is.na(covar$num_hh),NA,
                    ifelse(covar$num_hh==1, "1",
                           ifelse(covar$num_hh==2, "2",
                                  ifelse(covar$num_hh<=4, "3-4",">=5"))))
table(covar$num_hh)

### Recode Average total household income before tax ----
table(mydata$hh_income, useNA = "ifany")
covar$hh_income[mydata$hh_income == "Less than 18,000"]="<18,000" 
covar$hh_income[mydata$hh_income == "18,000 to 30,999"]="18,000-30,999" 
covar$hh_income[mydata$hh_income == "31,000 to 51,999"]="31,000-51,999" 
covar$hh_income[mydata$hh_income %in% c("52,000 to 100,000",
                                        "Greater than 100,000")]=">=52,000"
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

### Recoding Physical activity ----
table(mydata$num_walk, useNA = "ifany")
table(mydata$num_mod, useNA = "ifany")
table(mydata$num_vig, useNA = "ifany")

tmp <- mydata %>% select(eid, num_walk,num_mod,num_vig) %>%
  na_if("Do not know") %>% na_if("Prefer not to answer") %>%
  mutate(num_walk=recode(num_walk,"Unable to walk"="0")) %>%
  mutate(across(c(num_walk,num_mod,num_vig), as.numeric))

covar[,c("num_walk","num_mod","num_vig")]=tmp[,c("num_walk","num_mod","num_vig")]
summary(covar[,c("num_walk","num_mod","num_vig")])

rownames(tmp)=tmp$eid
tmp$eid=NULL

# Run PCA
mypca <- prcomp(~., data=tmp, scale.=T, na.action = na.omit)
out=summary(mypca)
ev=out$importance[2,]
ev


# Loadings
mycor=out$rotation

res=mypca$x # scores
dim(res)

# Add composite score to data
covar$phys_PC1=res[match(covar$eid,rownames(res)),1]
covar$phys_PC2=res[match(covar$eid,rownames(res)),2]

summary(covar$phys_PC1)
summary(covar$phys_PC2)

### Recoding Sleep duration (hours) per 24 hours ----
table(mydata$sleep, useNA = "ifany")
tmp=covar %>% select(sleep) %>%
  na_if("Do not know") %>% na_if("Prefer not to answer") %>%
  mutate(sleep=as.numeric(sleep))
summary(tmp$sleep)
tmp$sleep=ifelse(tmp$sleep <= 6, "<=6",
                 ifelse(tmp$sleep >=9, ">=9", "7-8"))
table(tmp$sleep)
covar$sleep=tmp$sleep
table(covar$sleep)

### Recode meat intake: ----
# Replace "Do not know" and "Prefer not to answer"
covar$o_fish=ifelse(is.na(mydata$o_fish)|mydata$o_fish %in% c("Do not know","Prefer not to answer"),NA,
                    mydata$o_fish) %>% as.factor
covar$no_fish=ifelse(is.na(mydata$no_fish)|mydata$no_fish %in% c("Do not know","Prefer not to answer"),NA,
                     mydata$no_fish) %>% as.factor
covar$poultry=ifelse(is.na(mydata$poultry)|mydata$poultry %in% c("Do not know","Prefer not to answer"),NA,
                     mydata$poultry) %>% as.factor
covar$pro_meat=ifelse(is.na(mydata$pro_meat)|mydata$pro_meat %in% c("Do not know","Prefer not to answer"),NA,
                      mydata$pro_meat) %>% as.factor
covar$beef=ifelse(is.na(mydata$beef)|mydata$beef %in% c("Do not know","Prefer not to answer"),NA,
                  mydata$beef) %>% as.factor
covar$lamb=ifelse(is.na(mydata$lamb)|mydata$lamb %in% c("Do not know","Prefer not to answer"),NA,
                  mydata$lamb) %>% as.factor
covar$pork=ifelse(is.na(mydata$pork)|mydata$pork %in% c("Do not know","Prefer not to answer"),NA,
                  mydata$pork) %>% as.factor
summary(covar[,c("o_fish","no_fish","poultry","pro_meat","beef","lamb","pork")])

### Fruit/vegetable intake ----
# Replace "Do not know" and "Prefer not to answer" and Code less than one as zero
covar$cook_veg=ifelse(is.na(mydata$cook_veg)|mydata$cook_veg %in% c("Do not know","Prefer not to answer"),NA,
                      ifelse(mydata$cook_veg=="Less than one", 0, mydata$cook_veg)) %>% as.numeric
covar$salad=ifelse(is.na(mydata$salad)|mydata$salad %in% c("Do not know","Prefer not to answer"),NA,
                   ifelse(mydata$salad=="Less than one", 0, mydata$salad)) %>% as.numeric
covar$fresh_fru=ifelse(is.na(mydata$fresh_fru)|mydata$fresh_fru %in% c("Do not know","Prefer not to answer"),NA,
                       ifelse(mydata$fresh_fru=="Less than one", 0, mydata$fresh_fru)) %>% as.numeric
covar$dried_fru=ifelse(is.na(mydata$dried_fru)|mydata$dried_fru %in% c("Do not know","Prefer not to answer"),NA,
                       ifelse(mydata$dried_fru=="Less than one", 0, mydata$dried_fru)) %>% as.numeric
summary(covar[,c("cook_veg","salad","fresh_fru","dried_fru")])

### Recode Coffee intake: ----
covar$coffee=ifelse(is.na(mydata$coffee)|mydata$coffee %in% c("Do not know","Prefer not to answer"),NA,
                    ifelse(mydata$coffee=="Less than one", 0, mydata$coffee)) %>% as.numeric
summary(covar$coffee)

### Recode Tea intake: Same as coffee ----
covar$tea=ifelse(is.na(mydata$tea)|mydata$tea %in% c("Do not know","Prefer not to answer"), NA,
                 ifelse(mydata$tea=="Less than one", 0, mydata$tea)) %>% as.numeric
summary(covar$tea)

### Recode Water intake: Same as coffee ----
covar$water=ifelse(is.na(mydata$water)|mydata$water %in% c("Do not know", "Prefer not to answer"), NA,
                   ifelse(mydata$water=="Less than one", 0, mydata$water)) %>% as.numeric
summary(covar$water)

### Composite dietary intake ----
diet=covar %>% select(eid, o_fish,no_fish,poultry,pro_meat,beef,lamb,pork,
                      cook_veg, salad, fresh_fru) %>%
  na.omit %>%
  mutate_at(c("o_fish","no_fish","poultry","pro_meat","beef","lamb","pork"),
            ~ factor(., levels=c("Never","Less than once a week",
                                 "Once a week","2-4 times a week",
                                 "5-6 times a week",
                                 "Once or more daily"))) %>%
  mutate_all(as.numeric)
rownames(diet)=diet$eid
diet$eid=NULL

# Run PCA
mypca <- prcomp(~., data=diet, scale.=T, na.action = na.omit)
out=summary(mypca)
ev=out$importance[2,]
ev



# Loadings
mycor=out$rotation

res=mypca$x # scores
dim(res)

covar$diet_PC1=res[match(covar$eid,rownames(res)),1]
covar$diet_PC2=res[match(covar$eid,rownames(res)),2]
covar$diet_PC3=res[match(covar$eid,rownames(res)),3]
covar$diet_PC4=res[match(covar$eid,rownames(res)),4]
covar$diet_PC5=res[match(covar$eid,rownames(res)),5]

summary(covar[c("diet_PC1","diet_PC2","diet_PC3","diet_PC4","diet_PC5")])

### Composite beverage ----
bev=covar %>% select(eid, coffee, tea, water) %>%
  na.omit
rownames(bev)=bev$eid
bev$eid=NULL

# Run PCA
mypca <- prcomp(~., data=bev, scale.=T, na.action = na.omit)
out=summary(mypca)
ev=out$importance[2,]
ev

# Loadings
mycor=out$rotation

res=mypca$x # scores
dim(res)

covar$bev_PC1=res[match(covar$eid,rownames(res)),1]
covar$bev_PC2=res[match(covar$eid,rownames(res)),2]
summary(covar[c("bev_PC1","bev_PC2")])

### Recode Salt intake: ----
covar$salt[mydata$salt=="Prefer not to answer"]=NA
covar$salt=as.factor(covar$salt)
table(covar$salt,useNA = "ifany")

### Recode Alcohol intake frequency ----
table(mydata$alcohol, useNA = "ifany")
covar$alcohol[mydata$alcohol == "Prefer not to answer"]=NA
covar$alcohol[mydata$alcohol %in% c("Daily or almost daily",
                                    "Three or four times a week")]='Regularly'
covar$alcohol[mydata$alcohol %in% c("One to three times a month",
                                    "Once or twice a week")]='Occasionally'
covar$alcohol[mydata$alcohol %in% c("Special occasions only",
                                    "Never")]='Never/rarely'
table(covar$alcohol, useNA = "ifany")

### Recoding Smoking status:----
covar$smoking[covar$smoking=="Prefer not to answer"]=NA

### Recoding smoking in HH (for sensitivity analysis)----
covar$smoke_hh[covar$smoke_hh=="Prefer not to answer"]=NA
covar$smoke_hh[grepl("Yes",covar$smoke_hh)]="Yes"
table(covar$smoke_hh, useNA = "ifany")

### Recoding Maternal smoking: No/Yes ----
table(mydata$mat_smoke, useNA = "ifany")
covar$mat_smoke=ifelse(covar$mat_smoke %in% c("Do not know","Prefer not to answer"),NA,
                       covar$mat_smoke)
table(covar$mat_smoke, useNA = "ifany")


### Recoding Number of medications: 0, 1, >1 ----
summary(mydata$num_med) # 860 missing
covar$num_med=ifelse(is.na(covar$num_med), NA,
                     ifelse(covar$num_med==0, "0",
                            ifelse(covar$num_med==1,"1",">1")))
table(covar$num_med, useNA = "ifany")

### Restructuring data ----
# Combine with comorbid
comorbid=readRDS("../Results/comorbid.rds")
covar2=inner_join(covar,comorbid,by="eid")

str(covar2)

### Relevel covar
covar3=covar2 %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(vars(starts_with("parent_")),
            function(x) factor(x, levels=c("0","1"),labels=c("No","Yes"))) %>%
  mutate(sex=relevel(sex, "Female"),
         ethnic=relevel(ethnic, "White"),
         employment=relevel(employment, "Employed"),
         education=factor(education,levels=c("Intermediate","Low","High")),
         type_accom=relevel(type_accom,"House"),
         own_rent=factor(own_rent,levels=c("Own","Rent","Other")),
         num_hh=factor(num_hh,levels=c("2","1","3-4",">=5")),
         hh_income=factor(hh_income,
                          levels=c("18,000-30,999","<18,000","31,000-51,999",">=52,000")),
         bmi=factor(bmi,
                    levels=c("Normal","Underweight","Pre-obesity","Obesity class I",
                             "Obesity class II", "Obesity class III")),
         sleep=factor(sleep,levels=c("7-8","<=6",">=9")),
         salt=factor(salt,levels=c("Never/rarely","Sometimes","Usually","Always")),
         alcohol=factor(alcohol,levels=c("Never/rarely","Occasionally","Regularly")),
         smoking=factor(smoking,levels=c("Current",
                                         "Never",
                                         "Previous")),
         smoke_hh=factor(smoke_hh,levels=c("No",
                                           "Yes")),
         num_med=factor(num_med,levels=c("0","1",">1"))) %>%
  mutate_at(vars((o_fish:pork)),
            function(x) factor(x,levels=c("Never","Less than once a week","Once a week",
                                          "2-4 times a week","5-6 times a week",
                                          "Once or more daily")))
str(covar3)
levels(covar3$cardiovascular)=c("No","Yes")
levels(covar3$hypertension)=c("No","Yes")
levels(covar3$diabetes)=c("No","Yes")
levels(covar3$respiratory)=c("No","Yes")
levels(covar3$autoimmune)=c("No","Yes")
str(covar3)

sort(apply(covar3, 2, function(x) sum(is.na(x))))

saveRDS(covar3, "../Results/nvsm/covar_raw.rds")


# For never smoker
forNeverSmoker=covar3 %>% 
  select((eid:bmi),phys_PC1,phys_PC2,sleep,(diet_PC1:diet_PC5),salt,
         bev_PC1,bev_PC2,alcohol,smoking,smoke_hh,mat_smoke,no2,nox,pm10,pm2.5_absorb,pm2.5,
         pm2.5_10,close_major_rd,num_med,starts_with("parent_"),cardiovascular:autoimmune)
str(forNeverSmoker)

saveRDS(forNeverSmoker, "../Results/nvsm/covar_never_smoker.rds")

rm(list=ls())

covar=readRDS("../Results/nvsm/covar_never_smoker.rds")
biomarker_imp=readRDS("../Results/biomarker_imp_master.rds")
case_control=readRDS("../Results/case_control.rds")

sort(apply(covar, 2, function(x) sum(is.na(x))))


### Data sets for Never smoker sensitivity analyses----

covar = covar %>%
  select((eid:sex), bmi,(ethnic:hh_income),(phys_PC1:autoimmune))

case_control=case_control %>% select(eid,case_status)
multivar_data=inner_join(case_control,covar,by="eid") %>%
  right_join(biomarker_imp, by="eid")

#myeids=rownames(multivar_data) # extract eids from main data
rownames(multivar_data)=multivar_data$eid
multivar_data=select(multivar_data,-eid)
#multivar_data=multivar_data[myeids,] # Consistent eids with main data set

summary(multivar_data)
table(multivar_data$case_status)

# Separate by case
lung=multivar_data[multivar_data$case_status!="bladder",]
bladder=multivar_data[multivar_data$case_status!="lung",]

ifelse(dir.exists("../Results/nvsm"),"",dir.create("../Results/nvsm"))
saveRDS(multivar_data, "../Results/nvsm/multivar_data_full.rds")
saveRDS(lung, "../Results/nvsm/multivar_data_lung.rds")
saveRDS(bladder, "../Results/nvsm/multivar_data_bladder.rds")

