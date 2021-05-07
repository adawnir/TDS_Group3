### TDS Project -- MASTER Recoding risk factor variables
### Programme created by Rin Wada on 30 March 2021

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

tmp=mydata %>% select(eid, num_walk,num_mod,num_vig) %>%
  na_if("Do not know") %>% na_if("Prefer not to answer") %>%
  mutate(num_walk=recode(num_walk,"Unable to walk"="0")) %>%
  mutate(across(c(num_walk,num_mod,num_vig), as.numeric))

covar[,c("num_walk","num_mod","num_vig")]=tmp[,c("num_walk","num_mod","num_vig")]
summary(covar[,c("num_walk","num_mod","num_vig")])

rownames(tmp)=tmp$eid
tmp$eid=NULL

# Run PCA
mypca <- prcomp(~., data=tmp, scale.=T, na.action = na.omit)
loadings = mypca$rotation
saveRDS(loadings, "../Results/phys_act_loadings.rds")
out=summary(mypca)
ev=out$importance[2,]
saveRDS(ev, "../Results/phys_act_ev.rds")
ev

# Scree plot
dir.create("../Figures/composite")
pdf("../Figures/composite/phys_score_ev.pdf", width=5, height=5)
par(mar=c(5,5,1,1))
plot(ev, pch=19, col="navy", las=1, type="b", ylim=c(0,1),
     ylab="Proportion of explained variance", xlab="PCs", xaxt="n")
points(cumsum(ev), pch=19, col="red", type="b")
axis(1, at = 1:3)
legend("right", pch=19, col=c("navy", "red"),
       legend=c("Proportion of e.v.", "Cumulative proportion of e.v."))
dev.off()

# Loadings
mycor=out$rotation
pdf("../Figures/composite/phys_score_cor.2.pdf", width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,1:2], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[2]*" ("*a*"% e.v.)", list(a=round(ev[2]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,2], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,2]+sign(mycor[,2])*0.1, cex = 0.7,
     labels=c("Walking","Moderate activity","Vigorous activity"), col="navy")
dev.off()

pdf("../Figures/composite/phys_score_cor.3.pdf", width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,c(1,3)], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[3]*" ("*a*"% e.v.)", list(a=round(ev[3]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,3], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,3]+sign(mycor[,3])*0.1, cex = 0.7,
     labels=c("Walking","Moderate activity","Vigorous activity"), col="navy")
dev.off()

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
loadings = mypca$rotation
saveRDS(loadings, "../Results/diet_loadings.rds")
out=summary(mypca)
ev=out$importance[2,]
ev
saveRDS(ev, "../Results/diet_ev.rds")


# Scree plot
pdf("../Figures/composite/diet_score_ev.pdf", width=5, height=5)
par(mar=c(5,5,1,1))
plot(ev, pch=19, col="navy", las=1, type="b", ylim=c(0,1),
     ylab="Proportion of explained variance", xlab="PCs")
points(cumsum(ev), pch=19, col="red", type="b")
legend("right", pch=19, col=c("navy", "red"),
       legend=c("Proportion of e.v.", "Cumulative proportion of e.v."))
dev.off()

# Loadings
mycor=out$rotation
pdf("../Figures/composite/diet_score_cor.2.pdf", width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,1:2], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[2]*" ("*a*"% e.v.)", list(a=round(ev[2]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,2], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,2]+sign(mycor[,2])*0.1, cex = 0.7,
     labels=1:10, col="navy")
legend("topright", cex=0.5, text.col="navy", bg="white",
       legend=c(paste(1:10, "-",c("Oily fish","Non-oily fish", "Poultry",
                                  "Processed meat", "Beef", "Lamb", "Pork",
                                  "Cooked vegetable","Salad/raw vegetable","Fresh fruit"))))
dev.off()

pdf("../Figures/composite/diet_score_cor.3.pdf", width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,c(1,3)], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[3]*" ("*a*"% e.v.)", list(a=round(ev[3]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,3], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,3]+sign(mycor[,3])*0.1, cex = 0.7,
     labels=1:10, col="navy")
legend("topright", cex=0.5, text.col="navy", bg="white",
       legend=c(paste(1:10, "-",c("Oily fish","Non-oily fish", "Poultry",
                                  "Processed meat", "Beef", "Lamb", "Pork",
                                  "Cooked vegetable","Salad/raw vegetable","Fresh fruit"))))
dev.off()

pdf("../Figures/composite/diet_score_cor.4.pdf", width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,c(1,4)], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[4]*" ("*a*"% e.v.)", list(a=round(ev[4]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,4], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,4]+sign(mycor[,4])*0.1, cex = 0.7,
     labels=1:10, col="navy")
legend("topleft", cex=0.5, text.col="navy", bg="white",
       legend=c(paste(1:10, "-",c("Oily fish","Non-oily fish", "Poultry",
                                  "Processed meat", "Beef", "Lamb", "Pork",
                                  "Cooked vegetable","Salad/raw vegetable","Fresh fruit"))))
dev.off()

pdf("../Figures/composite/diet_score_cor.5.pdf", width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,c(1,5)], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[5]*" ("*a*"% e.v.)", list(a=round(ev[5]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,5], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,5]+sign(mycor[,5])*0.1, cex = 0.7,
     labels=1:10, col="navy")
legend("topright", cex=0.5, text.col="navy", bg="white",
       legend=c(paste(1:10, "-",c("Oily fish","Non-oily fish", "Poultry",
                                  "Processed meat", "Beef", "Lamb", "Pork",
                                  "Cooked vegetable","Salad/raw vegetable","Fresh fruit"))))
dev.off()

pdf("../Figures/composite/diet_score_cor.6.pdf", width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,c(1,6)], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[6]*" ("*a*"% e.v.)", list(a=round(ev[6]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,6], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,6]+sign(mycor[,6])*0.1, cex = 0.7,
     labels=1:10, col="navy")
legend("topleft", cex=0.5, text.col="navy", bg="white",
       legend=c(paste(1:10, "-",c("Oily fish","Non-oily fish", "Poultry",
                                  "Processed meat", "Beef", "Lamb", "Pork",
                                  "Cooked vegetable","Salad/raw vegetable","Fresh fruit"))))
dev.off()


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
loadings = mypca$rotation
saveRDS(loadings, "../Results/bev_loadings.rds")
out=summary(mypca)
ev=out$importance[2,]
saveRDS(ev, "../Results/bev_ev.rds")

# Scree plot
pdf("../Figures/composite/bev_score_ev.pdf", width=5, height=5)
par(mar=c(5,5,1,1))
plot(ev, pch=19, col="navy", las=1, type="b", ylim=c(0,1),
     ylab="Proportion of explained variance", xlab="PCs")
points(cumsum(ev), pch=19, col="red", type="b")
legend("right", pch=19, col=c("navy", "red"),
       legend=c("Proportion of e.v.", "Cumulative proportion of e.v."))
dev.off()

# Loadings
mycor=out$rotation
pdf("../Figures/composite/bev_score_cor.2.pdf", width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,1:2], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[2]*" ("*a*"% e.v.)", list(a=round(ev[2]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,2], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,2]+sign(mycor[,2])*0.1, cex = 0.7,
     labels=c("Coffee","Tea","Water"), col="navy")
dev.off()

pdf("../Figures/composite/bev_score_cor.3.pdf", width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,c(1,3)], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[3]*" ("*a*"% e.v.)", list(a=round(ev[3]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,3], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,3]+sign(mycor[,3])*0.1, cex = 0.7,
     labels=c("Coffee","Tea","Water"), col="navy")
dev.off()

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

### Recoding Smoking status: Combine Smoking status and Smoking/smoker in household ----
table(mydata$smoking, useNA = "ifany")
table(mydata$smoke_hh, useNA = "ifany")
table(mydata$curr_smoke, useNA = "ifany")
tmp=mydata %>% select(eid, smoking, smoke_hh, curr_smoke)
tmp$smoking_new=ifelse(is.na(mydata$smoking)|mydata$smoking == "Prefer not to answer",NA,
                       ifelse(mydata$smoking=="Current", "Current",
                              ifelse(mydata$smoking=="Previous" &
                                       mydata$smoke_hh %in% c("Yes, one household member smokes",
                                                              "Yes, more than one household member smokes"),
                                     "Previous - Yes, smoker in HH",
                                     ifelse(mydata$smoking=="Previous" & mydata$smoke_hh == "No",
                                            "Previous - No smoker in HH",
                                            ifelse(mydata$smoking=="Never" &
                                                     mydata$smoke_hh %in% c("Yes, one household member smokes",
                                                                            "Yes, more than one household member smokes"),
                                                   "Never - Yes, smoker in HH",
                                                   ifelse(mydata$smoking=="Never" & mydata$smoke_hh == "No",
                                                          "Never - No smoker in HH",
                                                          NA))))))
# Check NAs
test = tmp[is.na(tmp$smoking_new),]
table(test$smoking,test$smoke_hh)
table(tmp$smoking_new, useNA = "ifany")

# Add back to main data set
covar$smoking=tmp$smoking_new

### Recoding smoking in HH (for sensitivity analysis)----
covar$smoke_hh=ifelse(mydata$smoking=="Current", "Current", mydata$smoke_hh)
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
         smoking=factor(smoking,levels=c("Never - No smoker in HH",
                                         "Never - Yes, smoker in HH",
                                         "Previous - No smoker in HH",
                                         "Previous - Yes, smoker in HH",
                                         "Current")),
         smoke_hh=factor(smoke_hh,levels=c("No",
                                           "Yes",
                                           "Current")),
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

saveRDS(covar3, "../Results/covar_raw.rds")

# For Table 1
forTable1=covar3 %>%
  select((eid:bmi),(num_walk:num_vig),phys_PC1,phys_PC2,sleep,
         o_fish,no_fish,poultry,pro_meat,(beef:pork),
         cook_veg:fresh_fru,(diet_PC1:diet_PC5),(salt:coffee),water,bev_PC1,bev_PC2,
         alcohol,smoking,mat_smoke,no2,nox,pm10,pm2.5_absorb,pm2.5,pm2.5_10,
         close_major_rd,num_med,starts_with("parent_"),cardiovascular:autoimmune)
# For univariate analysis
forModels=covar3 %>% 
  select((eid:bmi),phys_PC1,phys_PC2,sleep,(diet_PC1:diet_PC5),salt,
         bev_PC1,bev_PC2,alcohol,smoking,mat_smoke,no2,nox,pm10,pm2.5_absorb,pm2.5,
         pm2.5_10,close_major_rd,num_med,starts_with("parent_"),cardiovascular:autoimmune)
str(forTable1)
str(forModels)

# For never smoker
forNeverSmoker=covar3 %>% 
  select((eid:bmi),phys_PC1,phys_PC2,sleep,(diet_PC1:diet_PC5),salt,
         bev_PC1,bev_PC2,alcohol,smoke_hh,mat_smoke,no2,nox,pm10,pm2.5_absorb,pm2.5,
         pm2.5_10,close_major_rd,num_med,starts_with("parent_"),cardiovascular:autoimmune)
str(forNeverSmoker)

saveRDS(forTable1, "../Results/covar_table1.rds")
saveRDS(forModels, "../Results/covar_models.rds")

ifelse(dir.exists("../Results/never_smoker"),"",dir.create("../Results/never_smoker"))
saveRDS(forNeverSmoker, "../Results/never_smoker/covar_never_smoker.rds")
