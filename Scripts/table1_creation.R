#TDS Project group 3
#Creating table 1
#Fergal
#Finna be the sexiest table 1 of all time
#Let's goooo

rm(list=ls())
getwd()

#Load data sets
case_control <- readRDS("../Results/case_control.rds")
covars <- readRDS('../Results/covar_master.rds')
biomarkers <- readRDS('../Results/biomarker_master.rds')

#Combine them
mydata=inner_join(biomarkers, case_control, by="eid") %>%
  inner_join(covars)

#Remove columns we do not require
mydata <- subset(mydata, select = -c(diag_icd9, diag_icd10, date_baseline, epistart, time_diag_days, time_diag_years,
                                     age_diag, eid))

#Sleep variable needs recoding

mydata$sleep <- as.numeric(unlist(mydata$sleep))
mydata$sleep[mydata$sleep <= 6] <- '6 hours or less'
mydata$sleep[mydata$sleep == 7 | mydata$sleep == 8] <- '7-8 hours'
mydata$sleep[mydata$sleep >= 9] <- '9 hours or more'

mydata$sleep <- factor(mydata$sleep)

#Install table one package
install.packages('tableone')
library(tableone)

#Separate out cases and contols
#Turns out this wasn't necessary
lung <- mydata[mydata$case_status == 'lung',]
bladder <- mydata[mydata$case_status == 'bladder',]
control <- mydata[mydata$case_status == 'control',]

lung <- subset(lung, select = -c(case_status))
bladder <- subset(bladder, select = -c(case_status))
control <- subset(control, select = -c(case_status))

#Set the variables and specify the categorical variables
myVars <- colnames(mydata)[1:81]
myVars <- myVars[- 32] #Getting rid of case_status
myVars
catVars <- names(Filter(is.factor, mydata))
catVars <- catVars[- 1] #getting rid of case_status
catVars



#Make the table
tab3 <- CreateTableOne(vars = myVars, strata = "case_status" , data = mydata, factorVars = catVars)
print(tab3, formatOptions = list(big.mark = ","))
print(tab3, exact = "stage", noSpaces = TRUE, showAllLevels = TRUE)


str(mydata)
summary(tab3)

#Values for total n (% missing column)
missing <- print(mydata%>% summarise_all(list(name = ~sum(is.na(.))/length(.))))
totalobs <- 452753 - apply(is.na(mydata), 2, sum)



                