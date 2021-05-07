### TDS Project -- Table 1 (RUN ON HPC)
### Programme created by Fergal Madden reviewed by Rin on 1 April

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

t0=Sys.time()
# Load packages
library(tidyverse)
library(gtsummary)
library(gt)

# Load data sets
case_control <- readRDS("../Results/case_control.rds")
covars <- readRDS('../Results/covar_table1.rds')
biomarkers <- readRDS('../Results/biomarker_master.rds')

# Combine them
case_control=case_control %>% select(eid,case_status)
mydata=inner_join(case_control, covars, by="eid") %>%
  inner_join(biomarkers, by="eid") %>%
  select(-eid)

lung = mydata[mydata$case_status != "bladder",]
bladder = mydata[mydata$case_status != "lung",]

# Reorder factors
mydata=mydata %>%
  mutate(sex=relevel(sex, "Female"),
         ethnic=relevel(ethnic, "White"),
         employment=relevel(employment, "Employed"),
         education=factor(education,levels=c("Low","Intermediate","High")),
         type_accom=relevel(type_accom,"House"),
         own_rent=factor(own_rent,levels=c("Own","Rent","Other")),
         num_hh=factor(num_hh,levels=c("1","2","3-4",">=5")),
         hh_income=factor(hh_income,
                          levels=c("<18,000","18,000-30,999","31,000-51,999",">=52,000")),
         bmi=factor(bmi,
                    levels=c("Underweight","Normal",
                             "Pre-obesity",
                             "Obesity class I",
                             "Obesity class II",
                             "Obesity class III"),
                    labels=c("Underweight <18.5","Normal [18.5,25)",
                             "Overweight [25,30)",
                             "Obesity [30,35)",
                             "Severe obesity [35,40)",
                             "Morbid obesity >=40")),
         sleep=factor(sleep,levels=c("<=6","7-8",">=9")),
         salt=factor(salt,levels=c("Never/rarely","Sometimes","Usually","Always")),
         alcohol=factor(alcohol,levels=c("Never/rarely","Occasionally","Regularly")),
         # smoking=factor(smoking,
         #                levels=c("Never - No smoker in HH",
         #                                 "Never - Yes, smoker in HH",
         #                                 "Previous - No smoker in HH",
         #                                 "Previous - Yes, smoker in HH",
         #                                 "Current"),
         #                labels=c("Never - No smoker in household",
         #                         "Never - Yes, smoker in household",
         #                         "Previous - No smoker in household",
         #                         "Previous - Yes, smoker in household",
         #                         "Current")),
         num_med=factor(num_med,levels=c("0","1",">1"))) %>%
  mutate_at(vars((o_fish:pork)),
            function(x) factor(x,levels=c("Never","Less than once a week","Once a week",
                                          "2-4 times a week","5-6 times a week",
                                          "Once or more daily")))
class(mydata$num_walk)
class(mydata$num_mod)
class(mydata$num_vig)

# Relabel columns
colnames(mydata)

colnames(mydata)=c("case_status",readLines("../Dictionaries/table_1_labels.txt"))

# # toy
# library(caret)
# sub=createDataPartition(mydata$case_status, p=0.001, list=FALSE)
# mydata=mydata[sub,]


# # Subset for smaller tables
# forMain=mydata[,c(1:7,10:12,41)]
# forPresentation=mydata[,c(1:5,7,11,12,41)]

# fn returns t-test pvalue
lung.test <- function(data, variable, by, ...) {
  dat=filter(data, case_status %in% c("control", "lung"))
  if(is.numeric(dat[[variable]])){
    return(
      t.test(dat[[variable]] ~ as.factor(dat[[by]]))$p.value
    )
  } else{
    return(
      chisq.test(dat[[variable]], as.factor(dat[[by]]))$p.value
    )
  }
}

bladder.test <- function(data, variable, by, ...) {
  dat=filter(data, case_status %in% c("control", "bladder"))
  if(is.numeric(dat[[variable]])){
    return(
      t.test(dat[[variable]] ~ as.factor(dat[[by]]))$p.value
    )
  } else{
    return(
      chisq.test(dat[[variable]], as.factor(dat[[by]]))$p.value
    )
  }
}

cancer.test <- function(data, variable, by, ...) {
  dat=filter(data, case_status %in% c("lung", "bladder"))
  if(is.numeric(dat[[variable]])){
    return(
      t.test(dat[[variable]] ~ as.factor(dat[[by]]))$p.value
    )
  } else{
    return(
      chisq.test(dat[[variable]], as.factor(dat[[by]]))$p.value
    )
  }
}

# Set p-value format
ReformatP=function(x){
  x = formatC(x, format="e", digits=2)
  xsub=x[!is.na(x)]
  mysplit=strsplit(xsub, split="e")
  xsub=sapply(mysplit, FUN=function(tmp){paste0("$",tmp[1],"\\times 10^{",as.numeric(tmp[2]),"}$")})
  x[!is.na(x)]=xsub
  return(x)
}

### SUPPLEMENTARY ----
tab1_supp=mydata %>%
  tbl_summary(by = case_status, missing = "no",               
              statistic = all_continuous() ~ '{mean} ({sd})',
              digits = all_continuous() ~ 2,
              type = list(all_categorical() ~ "categorical",
                          starts_with("Number of days/week")~ "continuous")) %>%
  modify_header(update = list(stat_1 ~ paste0('Control (n=',
                                              sum(mydata$case_status=="control"),
                                              ')'),
                              stat_2 ~ paste0('Lung cancer (n=',
                                              sum(mydata$case_status=="lung"),
                                              ')'),
                              stat_3 ~ paste0('Bladder cancer (n=',
                                              sum(mydata$case_status=="bladder"),
                                              ')'))) %>%
  add_n(statistic = "{n} ({p_miss}%)",
        col_label = "N (% missing)") %>%
  add_stat(
    fns = everything() ~ lung.test, # only lung data
    fmt_fun = ReformatP, # format result with custom function
    header = "Lung cancer vs controls",      # new column header
    new_col_name = "lung_p.value"
  ) %>%
  add_stat(
    fns = everything() ~ bladder.test, # only lung data
    fmt_fun = ReformatP, # format result with custom function
    header = "Bladder cancer vs controls",      # new column header
    new_col_name = "bladder_p.value"
  ) %>%
  add_stat(
    fns = everything() ~ cancer.test,
    fmt_fun = ReformatP, # format result with custom function
    header = "Lung cancer vs bladder cancer",      # new column header
    new_col_name = "cancer_p.value"
  ) %>%
  modify_spanning_header(list(starts_with("stat_") ~ "Mean (SD) or n (%)",
                              ends_with("p.value") ~ "P-value",
                              n ~ NA)) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_table_body(
    mutate,
    group_variable = case_when(variable %in% colnames(mydata)[2:11] ~ "Sociodemographic",
                               variable %in% colnames(mydata)[12:41] ~ "Health risk",
                               variable %in% colnames(mydata)[42:49] ~ "Environment",
                               variable %in% colnames(mydata)[50:64] ~ "Medical",
                               variable %in% colnames(mydata)[65:ncol(mydata)] ~ "Biomarkers")
  ) %>%
  modify_table_body(group_by, group_variable) %>%
  as_gt() %>%
  as_latex()

ifelse(dir.exists("../Results/tables"),"",dir.create("../Results/tables"))
sink("../Results/tables/tab1_supp.txt")
tab1_supp %>%
  as.character() %>%
  cat()
sink()
sink()

# ### MAIN ----
# tab1=forMain %>%
#   tbl_summary(by = case_status, missing = "no",               
#               statistic = all_continuous() ~ '{mean} ({sd})',
#               digits = all_continuous() ~ 2,
#               type = list(all_categorical() ~ "categorical",
#                           starts_with("Number of days/week")~ "continuous")) %>%
#   modify_header(update = list(stat_1 ~ paste0('Control (n=',
#                                               sum(mydata$case_status=="control"),
#                                               ')'),
#                               stat_2 ~ paste0('Lung cancer (n=',
#                                               sum(mydata$case_status=="lung"),
#                                               ')'),
#                               stat_3 ~ paste0('Bladder cancer (n=',
#                                               sum(mydata$case_status=="bladder"),
#                                               ')'))) %>%
#   modify_spanning_header(list(starts_with("stat_") ~ "Mean (SD) or n (%)")) %>%
#   modify_footnote(update = everything() ~ NA) %>%
#   as_gt() %>%
#   as_latex()
# 
# sink("../Results/tables/tab1_main.txt")
# tab1 %>%
#   as.character() %>%
#   cat()
# sink()
# 
# ### PRESENTATION----
# tab1_pres=forPresentation %>%
#   tbl_summary(by = case_status, missing = "no",               
#               statistic = all_continuous() ~ '{mean} ({sd})',
#               digits = all_continuous() ~ 2,
#               type = list(all_categorical() ~ "categorical",
#                           starts_with("Number of days/week")~ "continuous")) %>%
#   modify_header(update = list(stat_1 ~ paste0('Control (n=',
#                                               sum(mydata$case_status=="control"),
#                                               ')'),
#                               stat_2 ~ paste0('Lung cancer (n=',
#                                               sum(mydata$case_status=="lung"),
#                                               ')'),
#                               stat_3 ~ paste0('Bladder cancer (n=',
#                                               sum(mydata$case_status=="bladder"),
#                                               ')'))) %>%
#   add_stat(
#     fns = everything() ~ lung.test, # only lung data
#     fmt_fun = ReformatP, # format result with custom function
#     header = "Lung cancer vs. controls",      # new column header
#     new_col_name = "lung_p.value"
#   ) %>%
#   bold_labels() %>%
#   add_stat(
#     fns = everything() ~ bladder.test, # only lung data
#     fmt_fun = ReformatP, # format result with custom function
#     header = "Bladder cancer vs. controls",      # new column header
#     new_col_name = "bladder_p.value"
#   ) %>%
#   modify_spanning_header(list(starts_with("stat_") ~ "Mean (SD) or n (%)",
#                               ends_with("p.value") ~ "P-value")) %>%
#   modify_footnote(update = everything() ~ NA) %>%
#   as_gt() %>%
#   as_latex()
# 
# sink("../Results/tables/tab1_pres.txt")
# tab1_pres %>%
#   as.character() %>%
#   cat()
# sink()

# ### Export ----
# 
# # Get LaTeX packages needed
# gt_latex_dependencies() %>%
#   as.character() %>%
#   cat()
t1=Sys.time()
print(t1-t0)