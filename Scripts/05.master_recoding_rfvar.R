### TDS Project -- MASTER Recoding risk factor variables
### Programme created by Rin Wada on 22 Feb 2021 reviewed on 27 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)

# ### Make one column for each variable of interest ----
# # Load data set
# case_control=readRDS("../Results/case_control.rds")
# recode_covar=readRDS("../Results/recoded_covar.rds")
# 
# # Filter study population
# recode_covar=recode_covar[which(recode_covar$eid %in% case_control$eid),]
# 
# # check for missing values for first instances
# recode_covar %>% select(contains(".0.")) %>% apply(., 2, function(x) sum(is.na(x)))
# 
# # Variables with multiple instances
# # Current employment status: 6142
# # Education: 6138
# # Illness of father and mother: 20107 and 20110
# 
# # Extract first instances
# mydata=recode_covar %>% select(eid,contains(".0."))
# 
# # Employment
# mydata %>% select(matches("X6142.")) %>% unlist %>% table
# table(mydata$X6142.0.0) # Need to check using all rest of columns
# tmp=mydata %>% select(eid,matches("X6142."))
# eidEmployed=apply(tmp[,-1], 2,
#                   function(x) tmp$eid[grep("In paid employment or self-employed",x)]) %>% unlist
# eidActive=apply(tmp[,-1], 2,
#                 function(x) tmp$eid[grep("Doing unpaid or voluntary work|Full or part-time student|Looking after home and/or family",
#                                x)]) %>% unlist
# eidRetired=apply(tmp[,-1], 2,
#                 function(x) tmp$eid[grep("Retired",x)]) %>% unlist
# eidUnemployed=apply(tmp[,-1], 2,
#                  function(x) tmp$eid[grep("Unemployed",x)]) %>% unlist
# eidUnable=apply(tmp[,-1], 2,
#                     function(x) tmp$eid[grep("Unable to work because of sickness or disability",
#                                              x)]) %>% unlist
# # RULE: if paid work = Employed
# # RULE: if unpaid work, student or caretaker = Active
# # RULE: if retired and not active = Retired
# # RULE if unemployed and not retired or active = Unemployed
# # RULE if not active and unable to work = Unable to work
# # RULE if Prefer not to answer, None of the above or NA = NA
# mydata$X6142.0.0=ifelse(mydata$eid %in% eidEmployed, "Employed",
#                         ifelse(mydata$eid %in% eidActive, "Active",
#                                ifelse(mydata$eid %in% eidRetired, "Retired",
#                                       ifelse(mydata$eid %in% eidUnemployed, "Unemployed",
#                                              ifelse(mydata$eid %in% eidUnable, "Unable to work",
#                                                     NA)))))
# barplot(table(mydata$X6142.0.0, useNA = "ifany"))
# 
# # Parental history
# mydata %>% select(matches("X20107.|X20110.")) %>% unlist %>% table
# 
# group1=c("Chronic bronchitis/emphysema","Diabetes","High blood pressure","Stroke","Heart disease")
# names(group1)=c("COPD", "diabetes","hypertension","stroke","CHD")
# group1NA=c("Prefer not to answer (group 1)", "Do not know (group 1)")
# group2=c("Breast cancer","Bowel cancer","Lung cancer","Prostate cancer")
# names(group2)=c("breast","bowel","lung","prostate")
# group2NA=c("Prefer not to answer (group 2)", "Do not know (group 2)")
# 
# # Make empty columns
# tmp=mydata %>% select(eid)
# diseases=c(group1,group2)
# x=matrix(0,nrow=nrow(tmp),ncol=length(diseases))
# colnames(x)=paste0("parent","_",names(diseases))
# tmp=cbind(tmp,x)
# 
# # Subset columns with information
# parent_data=mydata %>% select(eid,matches("X20107.|X20110."))
# missing=parent_data$eid[rowSums(is.na(parent_data)) == ncol(parent_data)] # No missing information
# 
# # Find eids with parental history for each diseases
# for (d in 1:length(diseases)){
#   print(names(diseases)[d])
#   disease=diseases[d]
#   myeids=apply(parent_data[,-1],2,function(x) parent_data$eid[grep(disease, x)]) %>% unlist
#   if (disease %in% group1){
#     tomatch=group1NA
#   }
#   if (disease %in% group2){
#     tomatch=group2NA
#   }
#   myNA_father=apply(parent_data[,2:11],2,function(x) parent_data$eid[x %in% tomatch]) %>% unlist
#   myNA_mother=apply(parent_data[12:22],2,function(x) parent_data$eid[x %in% tomatch]) %>% unlist
#   myNA=intersect(myNA_father,myNA_mother)
#   myeids=unique(myeids)
#   colindex=grep(names(diseases)[d], colnames(tmp))
#   tmp[tmp$eid %in% myeids,colindex]=1
#   tmp[tmp$eid %in% myNA,colindex]=NA
# }
# 
# # Check and add to mydata
# summary(tmp) # Missing data: 28766 for Group 1; 26339 for Group 2 diseases
# mydata=inner_join(mydata,tmp,by="eid")
# 
# # Select and rename columns
# str(mydata)
# mydata=mydata %>% select(eid,ends_with(".0.0"),starts_with("parent"),-matches("X20107.|X20110."))
# str(mydata)
# colnames(mydata)=c("eid","age_baseline", "sex","ethnic","townsend","employment","education",
#                    "type_accom","own_rent","num_hh","hh_income","bmi","o_fish","no_fish",
#                    "pro_meat","poultry","beef","lamb","pork","cook_veg","salad","fresh_fru",
#                    "dried_fru","salt","tea","coffee","type_coffee","water","mat_smoke","smoking",
#                    "curr_smoke","smoke_hh","alcohol","num_walk","num_mod","num_vig","sleep",
#                    "no2","nox","pm10","pm2.5_absorb","pm2.5","pm2.5_10","close_major_rd",
#                    "num_med",colnames(x))
# str(mydata)
# saveRDS(mydata, "../Results/recoded_covar_1col.rds")

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
                                  ifelse(covar$num_hh<=4, "3-4","≥5"))))
table(covar$num_hh)

### Recode Average total household income before tax ----
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

### Recoding Physical activity ----
table(mydata$num_walk, useNA = "ifany")
table(mydata$num_mod, useNA = "ifany")
table(mydata$num_vig, useNA = "ifany")

tmp=mydata %>% select(eid, num_walk,num_mod,num_vig) %>%
  na_if("Do not know") %>% na_if("Prefer not to answer") %>%
  mutate(num_walk=recode(num_walk,"Unable to walk"="0")) %>%
  mutate(across(c(num_walk,num_mod,num_vig), as.numeric))

covar[,c("num_walk","num_mod","num_vig")]=tmp[,c("num_walk","num_mod","num_vig")]
summary(covar[,c("num_walk","num_mod","num_vig")])

# Run PCA
mypca <- prcomp(~., data=tmp[,-1], scale.=T, na.action = na.omit)
# Scree plot
out=summary(mypca)
ev=out$importance[2,]
ev
cum_ev=out$importance[3,]
dir.create("../Figures/composite")
pdf("../Figures/composite/phys_score_ev.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
plot(cum_ev, pch=19, col="navy", type="b", ylim=c(0,1),
     xaxt="n", yaxt="n",
     ylab="Percentage of explained variance", xlab="Number of components")
axis(1, at=c(1:3),labels=c(1:3))
axis(2, at=seq(0,1,0.2),labels=seq(0,100,20))
points(ev, pch=19, col="tomato", type="b")
dev.off()
res=mypca$x # scores
dim(res)
# Make complete case data to find eids
addNAback=tmp[complete.cases(tmp[,c("num_walk","num_mod","num_vig")]),] %>% cbind(.,res)
summary(addNAback)
# Add NA back
addNAback=mydata %>% select(eid) %>% left_join(addNAback, by="eid")
summary(addNAback)
# Add composite score to data
covar$phys_score=addNAback$PC1

### Recoding Sleep duration (hours) per 24 hours ----
table(mydata$sleep, useNA = "ifany")
tmp=covar %>% select(sleep) %>%
  na_if("Do not know") %>% na_if("Prefer not to answer") %>%
  mutate(sleep=as.numeric(sleep))
summary(tmp$sleep)
tmp$sleep=ifelse(tmp$sleep <= 6, "≤6",
                 ifelse(tmp$sleep >=9, "≥9", "7-8"))
table(tmp$sleep)
covar$sleep=tmp$sleep
table(covar$sleep)

### Recode meat intake: Never, Less than once a week, Once a week, More than once a week ----
# Total meat: Poultry, Oily fish, Non-oily fish, Beef, Pork, Lamb/Mutton, Processed meat
table(mydata$o_fish,useNA = "ifany")
table(mydata$no_fish,useNA = "ifany")
table(mydata$poultry,useNA = "ifany")
table(mydata$pro_meat,useNA = "ifany")
table(mydata$beef,useNA = "ifany")
table(mydata$lamb,useNA = "ifany")
table(mydata$pork,useNA = "ifany")

# Replace "Do not know"
tmp=mydata %>% select(eid, o_fish,no_fish,poultry,pro_meat,beef,lamb,pork) %>%
  na_if("Do not know") %>% na_if("Prefer not to answer") %>%
  mutate_at(vars(-eid),
            function(x) as.factor(ifelse(is.na(x)|x%in%c("Never","Less than once a week","Once a week"),
                               x,"More than once a week")))
summary(tmp)
covar[,c("o_fish","no_fish","poultry","pro_meat","beef","lamb","pork")]=tmp[,c("o_fish","no_fish","poultry","pro_meat","beef","lamb","pork")]

table(covar$o_fish,useNA = "ifany")
table(covar$no_fish,useNA = "ifany")
table(covar$poultry,useNA = "ifany")
table(covar$pro_meat,useNA = "ifany")
table(covar$beef,useNA = "ifany")
table(covar$lamb,useNA = "ifany")
table(covar$pork,useNA = "ifany")

### Composite Scores Meat ----
## Total meat
# Load data from MCA
total_meat.eig=readRDS("../Results/MCA/total_meat.eig.rds")
total_meat.ind=readRDS("../Results/MCA/total_meat.ind.rds")
# Show variance explained by component
head(total_meat.eig)
# Scree plot
ev=total_meat.eig[,2]
ev
cum_ev=total_meat.eig[,3]
pdf("../Figures/composite/tot_meat_ev.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
plot(cum_ev, pch=19, col="navy", type="b", ylim=c(0,100),
     xaxt="n", yaxt="n",
     ylab="Percentage of explained variance", xlab="Number of components")
axis(1, at=c(1:35),labels=c(1:35))
axis(2, at=seq(0,100,20),labels=seq(0,100,20))
points(ev, pch=19, col="tomato", type="b")
dev.off()
# Show individual scores
res=total_meat.ind$coord[,1]
summary(res)
length(res)
# Make complete case data to find eids
addNAback=tmp[complete.cases(tmp[,c("o_fish","no_fish","poultry",
                                    "pro_meat","beef","lamb","pork")]),] %>%
  cbind(.,res)
summary(addNAback)
# Add NA back
addNAback=mydata %>% select(eid) %>% left_join(addNAback, by="eid")
summary(addNAback)
# Add composite score to data
covar$total_meat=addNAback$res
summary(covar$total_meat)

## White meat
# Load data from MCA
white_meat.eig=readRDS("../Results/MCA/white_meat.eig.rds")
white_meat.ind=readRDS("../Results/MCA/white_meat.ind.rds")
# Show variance explained by component
head(white_meat.eig)
# Scree plot
ev=white_meat.eig[,2]
ev
cum_ev=white_meat.eig[,3]
pdf("../Figures/composite/whi_meat_ev.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
plot(cum_ev, pch=19, col="navy", type="b", ylim=c(0,100),
     xaxt="n", yaxt="n",
     ylab="Percentage of explained variance", xlab="Number of components")
axis(1, at=c(1:15),labels=c(1:15))
axis(2, at=seq(0,100,20),labels=seq(0,100,20))
points(ev, pch=19, col="tomato", type="b")
dev.off()
# Show individual scores
res=white_meat.ind$coord[,1]
summary(res)
length(res)
# Make complete case data to find eids
addNAback=tmp[complete.cases(tmp[,c("o_fish","no_fish","poultry")]),] %>%
  cbind(.,res)
summary(addNAback)
# Add NA back
addNAback=mydata %>% select(eid) %>% left_join(addNAback, by="eid")
summary(addNAback)
# Add composite score to data
covar$white_meat=addNAback$res

## Red meat
# Load data from MCA
red_meat.eig=readRDS("../Results/MCA/red_meat.eig.rds")
red_meat.ind=readRDS("../Results/MCA/red_meat.ind.rds")
# Show variance explained by component
head(red_meat.eig)
# Scree plot
ev=red_meat.eig[,2]
ev
cum_ev=red_meat.eig[,3]
pdf("../Figures/composite/red_meat_ev.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
plot(cum_ev, pch=19, col="navy", type="b", ylim=c(0,100),
     xaxt="n", yaxt="n",
     ylab="Percentage of explained variance", xlab="Number of components")
axis(1, at=c(1:15),labels=c(1:15))
axis(2, at=seq(0,100,20),labels=seq(0,100,20))
points(ev, pch=19, col="tomato", type="b")
dev.off()
# Show individual scores
res=red_meat.ind$coord[,1]
summary(res)
length(res)
# Make complete case data to find eids
addNAback=tmp[complete.cases(tmp[,c("beef","lamb","pork")]),] %>%
  cbind(.,res)
summary(addNAback)
# Add NA back
addNAback=mydata %>% select(eid) %>% left_join(addNAback, by="eid")
summary(addNAback)
# Add composite score to data
covar$red_meat=addNAback$res

### Composite Scores Fruit/Vegetable ----
# Total fruit/vegetable intake
table(mydata$cook_veg,useNA = "ifany")
table(mydata$salad,useNA = "ifany")
table(mydata$fresh_fru,useNA = "ifany")
table(mydata$dried_fru,useNA = "ifany")

# Replace "Do not know" and "Prefer not to answer" and Code less than one as zero
tmp=mydata %>% select(eid,cook_veg,salad,fresh_fru,dried_fru) %>%
  na_if("Do not know") %>% na_if("Prefer not to answer") %>%
  mutate(across(c(cook_veg,salad,fresh_fru,dried_fru), recode, "Less than one"="0")) %>%
  mutate(across(c(cook_veg,salad,fresh_fru,dried_fru), as.numeric))

covar[,c("cook_veg","salad","fresh_fru","dried_fru")]=tmp[,c("cook_veg","salad","fresh_fru","dried_fru")]
summary(covar[,c("cook_veg","salad","fresh_fru","dried_fru")])

# Run PCA
mypca <- prcomp(~., data=tmp[,-1], scale.=T, na.action = na.omit)
# Scree plot
out=summary(mypca)
ev=out$importance[2,]
ev
cum_ev=out$importance[3,]
pdf("../Figures/composite/tot_fru_veg_ev.pdf", width=7, height=7)
par(mar=c(5,5,1,1))
plot(cum_ev, pch=19, col="navy", type="b", ylim=c(0,1),
     xaxt="n", yaxt="n",
     ylab="Percentage of explained variance", xlab="Number of components")
axis(1, at=c(1:4),labels=c(1:4))
axis(2, at=seq(0,1,0.2),labels=seq(0,100,20))
points(ev, pch=19, col="tomato", type="b")
dev.off()
res=mypca$x # scores
dim(res)
# Make complete case data to find eids
addNAback=tmp[complete.cases(tmp[,c("cook_veg","salad","fresh_fru","dried_fru")]),] %>%
  cbind(.,res)
summary(addNAback)
# Add NA back
addNAback=mydata %>% select(eid) %>% left_join(addNAback, by="eid")
summary(addNAback)
# Add composite score to data
covar$total_fru_veg=addNAback$PC1

### Recode Salt intake: Never/rarely, Sometimes, Usually, Always ----
table(mydata$salt,useNA = "ifany")
covar$salt[mydata$salt=="Prefer not to answer"]=NA
table(covar$salt,useNA = "ifany")

### Recode Coffee intake: None, ≤1, 2-3, ≥4 ----
table(mydata$coffee,useNA = "ifany")
covar$coffee=ifelse(is.na(covar$coffee)|covar$coffee %in% c("Don't know",
                                                            "Prefer not to answer"), NA,
                     ifelse(covar$coffee==0,"0",
                            ifelse(covar$coffee %in% c("Less than one", "1"),"≥1",
                                   ifelse(covar$coffee %in% c("2","3"), "2-3", "≥4"))))
table(covar$coffee,useNA = "ifany")

### Recode Tea intake: Same as coffee ----
table(mydata$tea,useNA = "ifany")
covar$tea=ifelse(is.na(covar$tea)|covar$tea %in% c("Don't know","Prefer not to answer"), NA,
                 ifelse(covar$tea==0,"0",
                        ifelse(covar$tea %in% c("Less than one", "1"),"≥1",
                               ifelse(covar$tea %in% c("2","3"), "2-3", "≥4"))))
table(covar$tea,useNA = "ifany")

### Recode Water intake: Same as coffee ----
table(mydata$water,useNA = "ifany")
covar$water=ifelse(is.na(covar$water)|covar$water %in% c("Don't know", "Prefer not to answer"), NA,
                   ifelse(covar$water==0,"0",
                          ifelse(covar$water %in% c("Less than one", "1"),"≥1",
                                 ifelse(covar$water %in% c("2","3"), "2-3", "≥4"))))
table(covar$water,useNA = "ifany")

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

### Recoding Smoking status: Combine Smoking status and Smoking/smoker in household ----
table(mydata$smoking, useNA = "ifany")
table(mydata$smoke_hh, useNA = "ifany")
table(mydata$curr_smoke, useNA = "ifany")
tmp=mydata %>% select(eid, smoking, smoke_hh, curr_smoke)
tmp$smoking_new=ifelse(is.na(mydata$smoking)|mydata$smoking == "Prefer not to answer",NA,
                       ifelse(mydata$smoking=="Current", "Current smoker",
                              ifelse(mydata$smoking=="Previous" &
                                       mydata$smoke_hh %in% c("Yes, one household member smokes",
                                                              "Yes, more than one household member smokes"),
                                     "Previous smoker - Yes, smoker in household",
                                     ifelse(mydata$smoking=="Previous" & mydata$smoke_hh == "No",
                                            "Previous smoker - No smoker in household",
                                            ifelse(mydata$smoking=="Never" &
                                                     mydata$smoke_hh %in% c("Yes, one household member smokes",
                                                                            "Yes, more than one household member smokes"),
                                                   "Never smoker - Yes, smoker in household",
                                                   ifelse(mydata$smoking=="Never" & mydata$smoke_hh == "No",
                                                          "Never smoker - No smoker in household",
                                                          NA))))))
# Check NAs
test = tmp[is.na(tmp$smoking_new),]
table(test$smoking,test$smoke_hh)
table(tmp$smoking_new, useNA = "ifany")

# Add back to main data set
covar$smoking=tmp$smoking_new

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
         num_hh=factor(num_hh,levels=c("2","1","3-4","≥5")),
         hh_income=factor(hh_income,
                          levels=c("18,000-30,999","<18,000","31,000-51,999",">52,000")),
         bmi=factor(bmi,
                    levels=c("Normal","Underweight","Pre-obesity","Obesity class I",
                             "Obesity class II", "Obesity class III")),
         sleep=factor(sleep,levels=c("7-8","≤6","≥9")),
         salt=factor(salt,levels=c("Never/rarely","Sometimes","Usually","Always")),
         alcohol=factor(alcohol,levels=c("Never/rarely","Occasionally","Regularly")),
         smoking=factor(smoking,levels=c("Never smoker - No smoker in household",
                                         "Never smoker - Yes, smoker in household",
                                         "Previous smoker - No smoker in household",
                                         "Previous smoker - Yes, smoker in household",
                                         "Current smoker")),
         num_med=factor(num_med,levels=c("0","1",">1"))) %>%
  mutate_at(vars((o_fish:pork)),
            function(x) factor(x,levels=c("Never","Less than once a week","Once a week",
                                          "More than once a week"))) %>%
  mutate_at(vars(tea,coffee,water),
            function(x) factor(x,levels=c("0","≥1","2-3","≥4")))
str(covar3)
levels(covar3$cardiovascular)=c("No","Yes")
levels(covar3$hypertension)=c("No","Yes")
levels(covar3$diabetes)=c("No","Yes")
levels(covar3$respiratory)=c("No","Yes")
levels(covar3$autoimmune)=c("No","Yes")
str(covar3)

sort(apply(covar3, 2, function(x) sum(is.na(x))))

saveRDS(covar3, "../Results/covar_raw.rds")

# For Table 1
forTable1=covar3 %>%
  select((eid:bmi),phys_score,sleep,(num_walk:num_vig),total_meat,white_meat,
         red_meat,pro_meat,(o_fish:no_fish),poultry,pork,(beef:lamb),pro_meat,
         total_fru_veg,cook_veg:dried_fru,(salt:coffee), water,alcohol,smoking,
         mat_smoke,no2,nox,pm10,pm2.5_absorb,pm2.5,pm2.5_10,close_major_rd,
         num_med,starts_with("parent_"),cardiovascular:autoimmune)
# For univariate analysis
forModels=covar3 %>% 
  select((eid:bmi),phys_score,sleep,total_meat,white_meat,red_meat,pro_meat,
         total_fru_veg,(salt:coffee), water,alcohol,smoking,
         mat_smoke,no2,nox,pm10,pm2.5_absorb,pm2.5,pm2.5_10,close_major_rd,
         num_med,starts_with("parent_"),cardiovascular:autoimmune)
str(forTable1)
str(forModels)

saveRDS(forTable1, "../Results/covar_table1.rds")
saveRDS(forModels, "../Results/covar_models.rds")
