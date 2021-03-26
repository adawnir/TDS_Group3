#Table 1 creation using gtsummary
#Fergal Madden

rm(list=ls())
install.packages('gtsummary')
install.packages('labelled')
install.packages('flextable')
library(labelled)
library(gtsummary)
library(gt)
library(flextable)
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

#Load data sets
case_control <- readRDS("../Results/case_control.rds")
covars <- readRDS('../Results/covar_table1.rds')
biomarkers <- readRDS('../Results/biomarker_master.rds')

#Combine them
mydata=inner_join(covars, case_control, by="eid") %>%
  inner_join(biomarkers)

#Remove columns we do not require
mydata <- subset(mydata, select = -c(diag_icd9, diag_icd10, date_baseline, epistart, time_diag_days, time_diag_years,
                                     age_diag, eid))

#Remove pesky extra age variable that seems to have appeared
mydata <- subset(mydata, select = -c(age_baseline.y))


#Relabel all the variables
#Deep breath...

var_label(mydata) <- list(age_baseline.x='Age', sex='Sex', ethnic='Ethnicity',
                          townsend='Townsend deprivation index', employment='Current employment status',
                          education='Education', type_accom='Type of accomodation lived in',
                          own_rent='Own or rent accomodation lived in', num_hh='Number of people in household',
                          hh_income='Average total household income', bmi='BMI',
                          phys_score='Physical activity score', sleep='Sleep per 24 hours (hours)',
                          num_walk='Number of days per week spent 10+ mins walking',
                          num_mod='Number of days per week spent 10+ mins doing moderate exercise',
                          num_vig='Number of days per week spent 10+ mins doing vigorous exercise',
                          total_meat='Total meat intake score', white_meat='White meat intake score',
                          red_meat='Red meat intake score', pro_meat='Processed meat intake',
                          o_fish='Oily fish intake', no_fish='Non oily fish intake',
                          poultry='Poultry intake', pork='Pork intake', beef='Beef intake',
                          lamb='Lamb intake', total_fru_veg='Total fruit and vegetable intake score',
                          cook_veg='Cooked vegetable intake', salad='Salad/raw vegetable intake',
                          fresh_fru='Fresh fruit intake', dried_fru='Dried fruit intake',
                          salt='Salt added to food', tea='Tea intake per day (cups)',
                          coffee='Coffee intake per day (cups)', water='Water intake per day (glasses)',
                          alcohol='Alcohol intake frequency', smoking='Smoking status',
                          mat_smoke='Maternal smoking around birth', no2='NO2 (μg/m3)',
                          nox='NOx (μg/m3)', pm10='PM10 (μg/m3)', pm2.5_absorb='PM2.5 (absorbance/m)',
                          pm2.5='PM2.5 (μg/m3)', pm2.5_10='PM2.5-10μm (μg/m3)',
                          close_major_rd='Close to major road', num_med='Number of medications',
                          parent_COPD='Parental history of COPD', parent_diabetes='Parental history of diabetes',
                          parent_hypertension='Parental history of hypertension',
                          parent_stroke='Parental history of stroke', parent_CHD='Parental history of heart disease',
                          parent_breast='Parental history of breast cancer',
                          parent_bowel='Parental history of bowel cancer',
                          parent_lung='Parental history of lung cancer',
                          parent_prostate='Parental history of prostate cancer',
                          cardiovascular='Cardiovascular disease', hypertension='Hypertension',
                          diabetes='Diabetes', respiratory='Respiratory disease',
                          autoimmune='Autoimmune disease')

var_label(mydata)

myVars <- colnames(mydata)[1:89]





#Need to split up the table into what's gonna be in the actual table 1 and what's gonna be in the supplementary 

table1Vars <- myVars[c(1:11, 36:38, 46, 61)]
biomarkerVars <- myVars[c(61:89)]  
supptableVars <- myVars[-c(1:11, 36:38, 46, 62:89)] 




#Proper table 1

# table summarizing data with no p-values
t0 <- mydata %>%
  select(all_of(table1Vars)) %>%
  tbl_summary(by = case_status, missing = "no",               
              statistic = all_continuous() ~ '{mean} ({sd})',
              digits = all_continuous() ~ 2) %>%
  modify_header(update = list(stat_1 ~ '**Control (n=448973)**',
                              stat_2 ~ '**Lung (n=1999)**',
                              stat_3 ~ '**Bladder (n=1806)**')) %>%
  add_n() %>%
  bold_labels()


#Create function for student t test
ttest_common_variance <- function(data, variable, by, ...) {
  data <- data[c(variable, by)] %>% dplyr::filter(complete.cases(.))
  t.test(data[[variable]] ~ factor(data[[by]]), var.equal = TRUE) %>%
    broom::tidy()
}

bonf <- 0.05/89
# table comparing control and lung
t1 <- mydata %>%
  select(all_of(table1Vars)) %>%
  filter(case_status %in% c("control", "lung")) %>%
  tbl_summary(by = case_status, missing = "no") %>%
  add_p(test = list(all_categorical() ~ 'chisq.test',
                    all_continuous() ~ 'ttest_common_variance')) %>%
  bold_p(t=bonf) %>%
  modify_header(p.value ~ md("**Control vs. Lung**")) %>%
  # hide summary stat columns
  modify_table_header(all_stat_cols(), hide = TRUE)

# table comparing control and bladder
t2 <- mydata %>%
  select(all_of(table1Vars)) %>%
  filter(case_status %in% c("control", "bladder")) %>%
  tbl_summary(by = case_status, missing = "no") %>%
  add_p(test = list(all_categorical() ~ 'chisq.test',
                    all_continuous() ~ 'ttest_common_variance')) %>%
  bold_p(t=bonf) %>%
  modify_header(p.value ~ md("**Control vs. Bladder**")) %>%
  # hide summary stat columns
  modify_table_header(all_stat_cols(), hide = TRUE)

# merging the 3 tables together, and adding additional gt formatting
tab1 <- tbl_merge(list(t0, t1, t2)) %>%
  modify_spanning_header(list(all_stat_cols() ~ "**Case/control status**",
                              starts_with("p.value") ~ "**P values**",
                              n_1 ~ NA)) %>%
  modify_footnote(update = starts_with('p.value') ~ 'Student t-test for continuous, Chi-squared test for categorical') %>%
  as_gt()
  
tab1

webshot::install_phantomjs()
gtsave(tab1, filename = 'table1_final.pdf', path = '../Figures')

saveRDS(tab1, "../Results/table1.rds")  







#Now the supplementary table
# table summarizing data with no p-values
t1_0 <- mydata %>%
  select(all_of(supptableVars)) %>%
  tbl_summary(by = case_status, missing = "no",               
              statistic = all_continuous() ~ '{mean} ({sd})',
              digits = all_continuous() ~ 2) %>%
  modify_header(update = list(stat_1 ~ '**Control (n=448973)**',
                              stat_2 ~ '**Lung (n=1999)**',
                              stat_3 ~ '**Bladder (n=1806)**')) %>%
  add_n() %>%
  bold_labels()


#Create function for student t test
ttest_common_variance <- function(data, variable, by, ...) {
  data <- data[c(variable, by)] %>% dplyr::filter(complete.cases(.))
  t.test(data[[variable]] ~ factor(data[[by]]), var.equal = TRUE) %>%
    broom::tidy()
}


# table comparing control and lung
t1_1 <- mydata %>%
  select(all_of(supptableVars)) %>%
  filter(case_status %in% c("control", "lung")) %>%
  tbl_summary(by = case_status, missing = "no") %>%
  add_p(test = list(all_categorical() ~ 'chisq.test',
                    all_continuous() ~ 'ttest_common_variance')) %>%
  bold_p(t=0.0005) %>%
  modify_header(p.value ~ md("**Control vs. Lung**")) %>%
  # hide summary stat columns
  modify_table_header(all_stat_cols(), hide = TRUE)

# table comparing control and bladder
t1_2 <- mydata %>%
  select(all_of(supptableVars)) %>%
  filter(case_status %in% c("control", "bladder")) %>%
  tbl_summary(by = case_status, missing = "no") %>%
  add_p(test = list(all_categorical() ~ 'chisq.test',
                    all_continuous() ~ 'ttest_common_variance')) %>%
  bold_p(t=0.0005) %>%
  modify_header(p.value ~ md("**Control vs. Bladder**")) %>%
  # hide summary stat columns
  modify_table_header(all_stat_cols(), hide = TRUE)

# merging the 3 tables together, and adding additional gt formatting
supptab1 <- tbl_merge(list(t1_0, t1_1, t1_2)) %>%
  modify_spanning_header(list(all_stat_cols() ~ "**Case/control status**",
                              starts_with("p.value") ~ "**P values**",
                              n_1 ~ NA)) %>%
  modify_footnote(update = starts_with('p.value') ~ 'Student t-test for continuous, Chi-squared test for categorical') %>%
  as_gt()

supptab1

gtsave(supptab1, filename = 'supptable1_final.pdf', path = '../Figures/table1_final.pdf')

saveRDS(supptab1, "../Results/table1supp.rds")  






#And now the biomarkers
# table summarizing data with no p-values
t2_0 <- mydata %>%
  select(all_of(biomarkerVars)) %>%
  tbl_summary(by = case_status, missing = "no",               
              statistic = all_continuous() ~ '{mean} ({sd})',
              digits = all_continuous() ~ 2) %>%
  modify_header(update = list(stat_1 ~ '**Control (n=448973)**',
                              stat_2 ~ '**Lung (n=1999)**',
                              stat_3 ~ '**Bladder (n=1806)**')) %>%
  add_n() %>%
  bold_labels()


#Create function for student t test
ttest_common_variance <- function(data, variable, by, ...) {
  data <- data[c(variable, by)] %>% dplyr::filter(complete.cases(.))
  t.test(data[[variable]] ~ factor(data[[by]]), var.equal = TRUE) %>%
    broom::tidy()
}


# table comparing control and lung
t2_1 <- mydata %>%
  select(all_of(biomarkerVars)) %>%
  filter(case_status %in% c("control", "lung")) %>%
  tbl_summary(by = case_status, missing = "no") %>%
  add_p(test = list(all_categorical() ~ 'chisq.test',
                    all_continuous() ~ 'ttest_common_variance')) %>%
  bold_p(t=0.0005) %>%
  modify_header(p.value ~ md("**Control vs. Lung**")) %>%
  # hide summary stat columns
  modify_table_header(all_stat_cols(), hide = TRUE)

# table comparing control and bladder
t2_2 <- mydata %>%
  select(all_of(biomarkerVars)) %>%
  filter(case_status %in% c("control", "bladder")) %>%
  tbl_summary(by = case_status, missing = "no") %>%
  add_p(test = list(all_categorical() ~ 'chisq.test',
                    all_continuous() ~ 'ttest_common_variance')) %>%
  bold_p(t=0.0005) %>%
  modify_header(p.value ~ md("**Control vs. Bladder**")) %>%
  # hide summary stat columns
  modify_table_header(all_stat_cols(), hide = TRUE)

# merging the 3 tables together, and adding additional gt formatting
biomarkertab1 <- tbl_merge(list(t2_0, t2_1, t2_2)) %>%
  modify_spanning_header(list(all_stat_cols() ~ "**Case/control status**",
                              starts_with("p.value") ~ "**P values**",
                              n_1 ~ NA)) %>%
  modify_footnote(update = starts_with('p.value') ~ 'Student t-test ') %>%
  as_gt()

biomarkertab1

gtsave(biomarkertab1, filename = 'biotable1_final.pdf', path = '../Figures/table1_final.pdf')

saveRDS(biomarkertab1, "../Results/table1biomarkers.rds")  

