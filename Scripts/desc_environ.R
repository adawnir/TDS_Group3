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
environ_ids=as.character(c(24003,24004,24005,24007,24006,24008,24009,24011,24012,24013,24014))

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
colnames(covar_env)=c("eid","no2","nox","pm10","pm2.5_absorb","pm2.5","pm2.5_10",
                      "traff_intens_near", "traff_intens_major", "inv_dist_major",
                      "traff_load_major","close_major_rd")

# Merge data sets by eid
environ_data=left_join(case_control,covar_env,by="eid")

# Combine traffic load major road and binary variable
summary(environ_data$traff_load_major)
hist(environ_data$traff_load_major[environ_data$traff_load_major>0],
     main="Total traffic load on major roads",
     xlab="Average total number of motor vehicles per 24 hours*length of road segment")
table(environ_data$close_major_rd, useNA = "ifany")
environ_data$close_major_rd_load=ifelse(environ_data$close_major_rd=="No","No",
                                        ifelse(environ_data$traff_load_major<3000000,
                                               "Low load","High load"))
table(environ_data$close_major_rd_load, useNA = "ifany")
# Resturcture and save
str(environ_data)
environ_data=environ_data %>%
  mutate(close_major_rd=factor(close_major_rd, labels=c("No","Yes")),
         close_major_rd_load=factor(close_major_rd_load, labels=c("High load","Low load","No")))
str(environ_data)
saveRDS(environ_data, "../Results/environ_data.rds")

### Descriptive analysis ----
environ_data=readRDS("../Results/environ_data.rds")
colnames(environ_data)
foo=function(x){
  mean=mean(x, na.rm = T)
  sd=sd(x, na.rm = T)
  paste0(signif(mean,5)," (",signif(sd,5),")")
}
summary=environ_data %>%
  group_by(case_status) %>%
  summarise(no2 = foo(no2),
            nox = foo(nox),
            pm10 = foo(pm10),
            pm2.5_absorb = foo(pm2.5_absorb),
            pm2.5 = foo(pm2.5),
            pm2.5_10 = foo(pm2.5_10),
            traff_intens_near = foo(traff_intens_near),
            traff_intens_major = foo(traff_intens_major),
            inv_dist_major = foo(inv_dist_major))
table(environ_data$close_major_rd_load,environ_data$case_status, useNA = 'ifany')
round(prop.table(table(environ_data$close_major_rd_load,environ_data$case_status),2)*100,1)
apply(environ_data,2,function(x) sum(!is.na(x)))
apply(environ_data,2,function(x) round(sum(is.na(x))/length(x)*100,1))
