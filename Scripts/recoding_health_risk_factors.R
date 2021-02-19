#TDS Project
#Recoding health risk factors
#Fergal Madden

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(data.table)
library(tidyverse)

#Extract data set
data=data.frame(fread("../Data/ukb26390.csv", nrows=1))

#Age - 21022
#Sex - 31
#Maternal smoking around birth - 1787
#Smoking status - 20116
#Current tobacco smoking 1239
#Past tobacco smoking 1249
#Smokers/smoking in household 1259
#Acohol intake frequency - 1558
#Processed meat intake - 1349
#Poultry intake - 1359
#Beef intake - 1369
#Lamb/mutton intake - 1379
#Pork intake - 1389
#Coffee intake - 1498
#Sleep hours per 24 hours - 1160
#Moderate physical activity per day - 884, 894
#Vigorous physical activity per day - 904, 914
#Water intake - 1528
#Fresh fruit intake per day - 1309
#Cooked vegetable intake - 1289
#Salad/raw vegetable intake - 1299
#Fat intake yesterday - 100004
#Saturated fat intake yesterday - 100006

myfields=list('21022', '31', "1787", "20116", '1239', '1249','1259', '1558', '1349', '1359', '1369', '1379', '1389', '1498',
              '1160', '884', '894', '904', '914', '1528', '1309', '1289', '1299', '100004', '100006')

# Extracting the column ids 
column_id=grep("eid", colnames(data))
found_fieldids=NULL
for (k in 1:length(myfields)){
  mygrep=grep(paste0("X",myfields[k],"."), fixed=TRUE, colnames(data))
  if (length(mygrep)>0){
    found_fieldids=c(found_fieldids, myfields[k])
  }
  column_id=c(column_id, mygrep)
}

# Extracting required columns from dataset
extracted=data.frame(fread("../Data/ukb26390.csv", select=column_id))

#Recoding...

#Maternal smoking 1787

extracted$MaternalSmoking[extracted$X1787.0.0 == 1] <- 'Yes'
extracted$MaternalSmoking[extracted$X1787.0.0 == 0] <- 'No'
extracted$MaternalSmoking[is.na(extracted$X1787.0.0)
                          | extracted$X1787.0.0 == -3
                          | extracted$X1787.0.0 == -1] <- NA

extracted$MaternalSmoking <- factor(extracted$MaternalSmoking)


#Smoking status 20116

extracted$SmokingStatus[extracted$X20116.0.0 == 2] <- 'Current'
extracted$SmokingStatus[extracted$X20116.0.0 == 1] <- 'Previous'
extracted$SmokingStatus[extracted$X20116.0.0 == 0] <- 'Never'
extracted$SmokingStatus[extracted$X20116.0.0 == -1
                        | is.na(extracted$X20116.0.0)] <- NA

extracted$SmokingStatus <- factor(extracted$SmokingStatus)


#Current tobacco smoking 1239

extracted$CurrentSmoking[extracted$X1239.0.0 == 1] <- 'Most/all days'
extracted$CurrentSmoking[extracted$X1239.0.0 == 2] <- 'Ocassionally'
extracted$CurrentSmoking[extracted$X1239.0.0 == 0] <- 'No'
extracted$CurrentSmoking[extracted$X1239.0.0 == -3
                         | is.na(extracted$X1239.0.0)] <- NA

extracted$CurrentSmoking <- factor(extracted$CurrentSmoking)


#Past tobacco smoking 1249

extracted$PastSmoking[extracted$X1249.0.0 == 1] <- 'Most/all days'
extracted$PastSmoking[extracted$X1249.0.0 == 2] <- 'Ocassionally'
extracted$PastSmoking[extracted$X1249.0.0 == 3] <- 'Tried once or twice'
extracted$PastSmoking[extracted$X1249.0.0 == 4] <- 'Never'
extracted$PastSmoking[extracted$X1249.0.0 == -3
                      | is.na(extracted$X1249.0.0)] <- NA
extracted$PastSmoking[extracted$X1239.0.0 == 1] <- 'Current'

extracted$PastSmoking <- factor(extracted$PastSmoking)


#Smoking/smokers in household 1259

extracted$HouseholdSmokers[extracted$X1259.0.0 == 1] <- 'Yes, one'
extracted$HouseholdSmokers[extracted$X1259.0.0 == 2] <- 'Yes, more than one'
extracted$HouseholdSmokers[extracted$X1259.0.0 == 0] <- 'No'
extracted$HouseholdSmokers[extracted$X1259.0.0 == -3
                           | is.na(extracted$X1259.0.0)] <- NA
extracted$HouseholdSmokers[extracted$X1239.0.0 == 1] <- 'Current'

extracted$HouseholdSmokers <- factor(extracted$HouseholdSmokers)


#Alcohol intake frequency 1558

extracted$AlcoholFrequency[extracted$X1558.0.0 == 1] <- 'Daily/almost daily'
extracted$AlcoholFrequency[extracted$X1558.0.0 == 2] <- '3 or 4 times a week'
extracted$AlcoholFrequency[extracted$X1558.0.0 == 3] <- '1 or 2 times a week'
extracted$AlcoholFrequency[extracted$X1558.0.0 == 4
                           | extracted$X1558.0.0 == 5
                           | extracted$X1558.0.0 ==6] <- '3 times a month or less'
extracted$AlcoholFrequency[extracted$X1558.0.0 == -3
                           | is.na(extracted$X1558.0.0)] <- NA

extracted$AlcoholFrequency <- factor(extracted$AlcoholFrequency)


#Pocessed meat intake 1349

extracted$ProcessedMeat[extracted$X1349.0.0 == 0] <- 'Never'
extracted$ProcessedMeat[extracted$X1349.0.0 == 1] <- 'Less than once a week'
extracted$ProcessedMeat[extracted$X1349.0.0 == 2] <- 'Once a week'
extracted$ProcessedMeat[extracted$X1349.0.0 == 3
                        | extracted$X1349.0.0 == 4
                        | extracted$X1349.0.0 == 5] <- 'More than once a week'
extracted$ProcessedMeat[extracted$X1349.0.0 == -3
                        | extracted$X1349.0.0 == -1
                        | is.na(extracted$X1349.0.0)] <- NA

extracted$ProcessedMeat <- factor(extracted$ProcessedMeat)


#Poultry Intake 1359

extracted$Poultry[extracted$X1359.0.0 == 0] <- 'Never'
extracted$Poultry[extracted$X1359.0.0 == 1] <- 'Less than once a week'
extracted$Poultry[extracted$X1359.0.0 == 2] <- 'Once a week'
extracted$Poultry[extracted$X1359.0.0 == 3
                        | extracted$X1359.0.0 == 4
                        | extracted$X1359.0.0 == 5] <- 'More than once a week'
extracted$Poultry[extracted$X1359.0.0 == -3
                        | extracted$X1359.0.0 == -1
                        | is.na(extracted$X1359.0.0)] <- NA

extracted$Poultry <- factor(extracted$Poultry)


#Beef intake 1369

extracted$Beef[extracted$X1369.0.0 == 0] <- 'Never'
extracted$Beef[extracted$X1369.0.0 == 1] <- 'Less than once a week'
extracted$Beef[extracted$X1369.0.0 == 2] <- 'Once a week'
extracted$Beef[extracted$X1369.0.0 == 3
                  | extracted$X1369.0.0 == 4
                  | extracted$X1369.0.0 == 5] <- 'More than once a week'
extracted$Beef[extracted$X1369.0.0 == -3
                  | extracted$X1369.0.0 == -1
                  | is.na(extracted$X1369.0.0)] <- NA

extracted$Beef <- factor(extracted$Beef)


#Lamb/mutton intake 1379

extracted$LambMutton[extracted$X1379.0.0 == 0] <- 'Never'
extracted$LambMutton[extracted$X1379.0.0 == 1] <- 'Less than once a week'
extracted$LambMutton[extracted$X1379.0.0 == 2] <- 'Once a week'
extracted$LambMutton[extracted$X1379.0.0 == 3
                  | extracted$X1379.0.0 == 4
                  | extracted$X1379.0.0 == 5] <- 'More than once a week'
extracted$LambMutton[extracted$X1379.0.0 == -3
                  | extracted$X1379.0.0 == -1
                  | is.na(extracted$X1379.0.0)] <- NA

extracted$LambMutton <- factor(extracted$LambMutton)


#Pork intake 1389

extracted$Pork[extracted$X1389.0.0 == 0] <- 'Never'
extracted$Pork[extracted$X1389.0.0 == 1] <- 'Less than once a week'
extracted$Pork[extracted$X1389.0.0 == 2] <- 'Once a week'
extracted$Pork[extracted$X1389.0.0 == 3
                  | extracted$X1389.0.0 == 4
                  | extracted$X1389.0.0 == 5] <- 'More than once a week'
extracted$Pork[extracted$X1389.0.0 == -3
                  | extracted$X1389.0.0 == -1
                  | is.na(extracted$X1389.0.0)] <- NA

extracted$Pork <- factor(extracted$Pork)


#Coffee intake 1498

extracted$CoffeeCupsPerDay[extracted$X1498.0.0 == 0] <- 'Never'
extracted$CoffeeCupsPerDay[extracted$X1498.0.0 == -10
                           | extracted$X1498.0.0 == 1] <- '1 or less'
extracted$CoffeeCupsPerDay[extracted$X1498.0.0 == 2
                           | extracted$X1498.0.0 == 3] <- '2 or 3'
extracted$CoffeeCupsPerDay[extracted$X1498.0.0 >= 4] <- '4 or more'
extracted$CoffeeCupsPerDay[extracted$X1498.0.0 == -1
                           | extracted$X1498.0.0 == -3
                           | is.na(extracted$X1498.0.0)] <- NA

extracted$CoffeeCupsPerDay <- factor(extracted$CoffeeCupsPerDay)


#Sleep per 24 hours 1160

extracted$SleepHours[extracted$X1160.0.0 <= 6] <- '6 or less'
extracted$SleepHours[extracted$X1160.0.0 == 7
                     | extracted$X1160.0.0 == 8] <- '7 or 8'
extracted$SleepHours[extracted$X1160.0.0 >= 9] <- '9 or more'
extracted$SleepHours[extracted$X1160.0.0 == -1
                     | extracted$X1160.0.0 == -3
                     | is.na(extracted$X1160.0.0)] <- NA

extracted$SleepHours <- factor(extracted$SleepHours)


#Moderate physical activity per day 884, 894
extracted$ModerateActivityPerDay<- NA
extracted$ModerateActivityPerDay[extracted$X884.0.0 == 0
                                 | extracted$X894.0.0 == 0] <- 'None'
extracted$ModerateActivityPerDay[extracted$X894.0.0 <= 30
                                 & extracted$X894.0.0 > 0] <- '30 mins or less'
extracted$ModerateActivityPerDay[extracted$X894.0.0 > 30
                                 & extracted$X894.0.0 <= 60] <- '31-60 mins'
extracted$ModerateActivityPerDay[extracted$X894.0.0 > 60] <- 'More than 60 mins'
extracted$ModerateActivityPerDay[extracted$X884.0.0 == -1
                                 | extracted$X884.0.0 == -3
                                 | is.na(extracted$X884.0.0)] <- NA

extracted$ModerateActivityPerDay <- factor(extracted$ModerateActivityPerDay)


#Vigorous physical activity per day 904, 914
extracted$VigorousActivityPerDay <- NA
extracted$VigorousActivityPerDay[extracted$X904.0.0 == 0
                                 | extracted$X914.0.0 == 0] <- 'None'
extracted$VigorousActivityPerDay[extracted$X914.0.0 <= 30
                                 & extracted$X914.0.0 > 0] <- '30 mins or less'
extracted$VigorousActivityPerDay[extracted$X914.0.0 > 30
                                 & extracted$X914.0.0 <= 60] <- '31-60 mins'
extracted$VigorousActivityPerDay[extracted$X914.0.0 > 60] <- 'More than 60 mins'
extracted$VigorousActivityPerDay[extracted$X904.0.0 == -1
                                 | extracted$X904.0.0 == -3
                                 | is.na(extracted$X904.0.0)] <- NA

extracted$VigorousActivityPerDay <- factor(extracted$VigorousActivityPerDay)


#Water intake 1528

extracted$WaterPerDay[extracted$X1528.0.0 == 0
                      | extracted$X1528.0.0 == -10] <- 'None/less than 1 glass'
extracted$WaterPerDay[extracted$X1528.0.0 == 1] <- '1 glass'
extracted$WaterPerDay[extracted$X1528.0.0 == 2] <- '2 glasses'
extracted$WaterPerDay[extracted$X1528.0.0 >= 3] <- '3 or more glasses'
extracted$WaterPerDay[extracted$X1528.0.0 == -1
                      | extracted$X1528.0.0 == -3
                      | is.na(extracted$X1528.0.0)] <- NA

extracted$WaterPerDay <- factor(extracted$WaterPerDay)


#Fresh fruit intake 1309

extracted$FruitPerDay[extracted$X1309.0.0 == 0
                      | extracted$X1309.0.0 == -10] <- 'None/less than 1 piece'
extracted$FruitPerDay[extracted$X1309.0.0 == 1] <- '1 piece'
extracted$FruitPerDay[extracted$X1309.0.0 == 2] <- '2 pieces'
extracted$FruitPerDay[extracted$X1309.0.0 >= 3] <- '3 or more pieces'
extracted$FruitPerDay[extracted$X1309.0.0 == -1
                      | extracted$X1309.0.0 == -3
                      | is.na(extracted$X1309.0.0)] <- NA

extracted$FruitPerDay <- factor(extracted$FruitPerDay)


#Cooked vegetable intake 1289

extracted$CookedVegPerDay[extracted$X1289.0.0 == 0
                      | extracted$X1289.0.0 == -10] <- 'None/less than 1 serving'
extracted$CookedVegPerDay[extracted$X1289.0.0 == 1] <- '1 serving'
extracted$CookedVegPerDay[extracted$X1289.0.0 == 2] <- '2 serving'
extracted$CookedVegPerDay[extracted$X1289.0.0 >= 3] <- '3 or more serving'
extracted$CookedVegPerDay[extracted$X1289.0.0 == -1
                      | extracted$X1289.0.0 == -3
                      | is.na(extracted$X1289.0.0)] <- NA

extracted$CookedVegPerDay <- factor(extracted$CookedVegPerDay)


#Salad/raw veg per day 1299

extracted$RawVegPerDay[extracted$X1299.0.0 == 0
                          | extracted$X1299.0.0 == -10] <- 'None/less than 1 serving'
extracted$RawVegPerDay[extracted$X1299.0.0 == 1] <- '1 serving'
extracted$RawVegPerDay[extracted$X1299.0.0 == 2] <- '2 serving'
extracted$RawVegPerDay[extracted$X1299.0.0 >= 3] <- '3 or more serving'
extracted$RawVegPerDay[extracted$X1299.0.0 == -1
                          | extracted$X1299.0.0 == -3
                          | is.na(extracted$X1299.0.0)] <- NA

extracted$RawVegPerDay <- factor(extracted$RawVegPerDay)


#Fat intake yesterday 100004

extracted$FatIntake <- extracted$X100004.0.0


#Saturated fat intake yesterday

extracted$SatFatIntake <- extracted$X100006.0.0


#Rename gender and age variables

extracted$Age <- extracted$X21022.0.0
extracted$Gender <- factor(extracted$X31.0.0)


#Save all the recoded variables

healthriskfactors <- extracted %>% select(eid, Gender, Age, MaternalSmoking, SmokingStatus, CurrentSmoking,
                                          PastSmoking, HouseholdSmokers, AlcoholFrequency,
                                          ProcessedMeat, Poultry, Beef, LambMutton, Pork, CoffeeCupsPerDay, SleepHours,
                                          ModerateActivityPerDay, VigorousActivityPerDay, WaterPerDay, FruitPerDay,
                                          CookedVegPerDay, RawVegPerDay, FatIntake, SatFatIntake)

saveRDS(healthriskfactors, "../Results/health_risk_factors.rds")
