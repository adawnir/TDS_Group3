### TDS Project -- Multiple Correspondence Analysis for categorical (meat) variables
### Programme created by Rin Wada on 27 Feb 2021

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(FactoMineR)
library(factoextra)
library(tidyverse)

### Load first instances of recoded covariates ----
mydata=readRDS("../Results/recoded_covar_1col.rds")

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
covar=mydata %>% select(o_fish,no_fish,poultry,pro_meat,beef,lamb,pork) %>%
  na_if("Do not know") %>% na_if("Prefer not to answer")

table(covar$o_fish,useNA = "ifany")
table(covar$no_fish,useNA = "ifany")
table(covar$poultry,useNA = "ifany")
table(covar$pro_meat,useNA = "ifany")
table(covar$beef,useNA = "ifany")
table(covar$lamb,useNA = "ifany")
table(covar$pork,useNA = "ifany")

## Total meat
# Subset only meat variables
total_meat=covar %>% select(o_fish,no_fish,poultry,pro_meat,beef,lamb,pork) %>%
  na.omit %>%
  mutate_all(as.factor)
summary(total_meat)

# MCA
total_meat.res=MCA(total_meat, ncp = 5, graph = F)

# Get eigen values
total_meat.eig=get_eigenvalue(total_meat.res)
# Get individual results
total_meat.ind=get_mca_ind(total_meat.res)

# Save data as rds
ifelse(dir.exists("../Results/MCA"),"",dir.create("../Results/MCA"))
saveRDS(total_meat.res,"../Results/MCA/total_meat.res.rds")
saveRDS(total_meat.eig,"../Results/MCA/total_meat.eig.rds")
saveRDS(total_meat.ind,"../Results/MCA/total_meat.ind.rds")

## White meat
# Subset only white meat variables
white_meat=covar %>% select(o_fish,no_fish,poultry) %>%
  na.omit %>%
  mutate_all(as.factor)
summary(white_meat)

# MCA
white_meat.res=MCA(white_meat, ncp = 5, graph = F)

# Get eigen values
white_meat.eig=get_eigenvalue(white_meat.res)
# Get individual results
white_meat.ind=get_mca_ind(white_meat.res)

# Save data as rds
saveRDS(white_meat.res,"../Results/MCA/white_meat.res.rds")
saveRDS(white_meat.eig,"../Results/MCA/white_meat.eig.rds")
saveRDS(white_meat.ind,"../Results/MCA/white_meat.ind.rds")

# Red meat
# Subset only meat variables
red_meat=covar %>% select(beef,lamb,pork) %>%
  na.omit %>%
  mutate_all(as.factor)
summary(red_meat)

# MCA
red_meat.res=MCA(red_meat, ncp = 5, graph = F)

# Get eigen values
red_meat.eig=get_eigenvalue(red_meat.res)
# Get individual results
red_meat.ind=get_mca_ind(red_meat.res)

# Save data as rds
saveRDS(red_meat.res,"../Results/MCA/red_meat.res.rds")
saveRDS(red_meat.eig,"../Results/MCA/red_meat.eig.rds")
saveRDS(red_meat.ind,"../Results/MCA/red_meat.ind.rds")

