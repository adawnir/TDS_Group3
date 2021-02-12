### TDS Project -- Descriptive analysis of risk factors (Medical)
## Programme created by Rin Wada on 10 Feb 2021

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(data.table)
library(tidyverse)

# Load data sets
case_control=readRDS("../Results/case_control.rds")
recoded_covar=readRDS("../Results/recoded_covar.rds")

# Select medical data from covariates
environ_ids=as.character(c(24004,24005,24007,24006,24008,24500,24503,24011,24014,22609,22610,
                           22611,22612,22613,22615))

column_id=grep("eid", colnames(recoded_covar))
found_fieldids=NULL
for (i in 1:length(environ_ids)){
  mygrep=grep(paste0("X",environ_ids[i],"."), fixed=TRUE, colnames(recoded_covar))
  if (length(mygrep)>0){
    found_fieldids=c(found_fieldids, environ_ids[i])
  }
  column_id=c(column_id, mygrep)
}
setdiff(environ_ids,found_fieldids) # Field ids not found in dataset
covar_env=recoded_covar[,column_id]

# Merge data sets by eid
mydata=merge(case_control,covar_env,by="eid",all=TRUE)
str(mydata)
mydata=mydata %>% mutate_at(vars(lung,bladder,control,X24014.0.0), as.factor)
colnames(mydata)=c("eid","lung","bladder","control",
                   "nox","pm10","pm2.5_absorb","pm2.5","pm2.5_10","traffic","maj_road")
str(mydata)
saveRDS(mydata, "../Results/environ_data.rds")

lung=mydata[mydata$lung==1,-c(2:4)]
bladder=mydata[mydata$bladder==1,-c(2:4)]
control=mydata[mydata$control==1,-c(2:4)]

round(apply(lung[,-c(1,8)],2,function(x) mean(x,na.rm=T)),2)

table(lung$maj_road)
round(prop.table(table(lung$maj_road))*100,2)

round(apply(bladder[,-c(1,8)],2,function(x) mean(x,na.rm=T)),2)

table(bladder$maj_road)
round(prop.table(table(bladder$maj_road))*100,2)

round(apply(control[,-c(1,8)],2,function(x) mean(x,na.rm=T)),2)

table(control$maj_road)
round(prop.table(table(lung$maj_road))*100,2)

round(apply(lung[,-c(1,8)],2,function(x) sum(!is.na(x))))
round(apply(bladder[,-c(1,8)],2,function(x) sum(!is.na(x))))
round(apply(control[,-c(1,8)],2,function(x) sum(!is.na(x))))

lung %>%
  select(-eid) %>%
  drop_na %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  labs(title = "Lung cancer", x="", y="Frequency") +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins = 30)

bladder %>%
  select(-eid) %>%
  drop_na %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  labs(title = "Bladder cancer", x="", y="Frequency") +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins = 30)

control %>%
  select(-eid) %>%
  drop_na %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  labs(title = "No cancer", x="", y="Frequency") +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins = 30)

# Correlation plot
cor=mydata %>%
  filter(lung==1|bladder==1|control==1) %>%
  select(-eid) %>%
  drop_na %>%
  keep(is.numeric) %>%
  cor()
library("pheatmap")
pheatmap(cor)
