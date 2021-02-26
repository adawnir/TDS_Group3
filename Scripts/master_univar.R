### TDS Project -- Univariate logistic regression analysis (Forest plot and Manhattan plot)
### Programme created by Rin Wada on 25 Feb 2021

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(data.table)
library(tidyverse)

# Load new datasets
biomarkers=readRDS("../Results/biomarker_imp_master.rds")
covar=readRDS("../Results/covar_master.rds")
cc=readRDS("../Results/case_control.rds")

# Restructure data - case/control status in first column, confounding factors is subsequent
covar = covar %>%
  select((eid:sex), bmi, smoking,(ethnic:hh_income),(total_meat:alcohol),
         (mat_smoke:autoimmune))

mydata=cc %>% select(eid, case_status) %>%
  left_join(covar, by="eid") %>%
  select((eid:sex), bmi, smoking,(ethnic:hh_income),(total_meat:alcohol),
         (mat_smoke:autoimmune)) %>%
  left_join(biomarkers, by="eid") %>%
  select(-eid)
str(mydata) # Ensure categorical variables are coded as factors and continuous numerical

# Separate by case
lung_data=mydata[mydata$case_status!="bladder",]
bladder_data=mydata[mydata$case_status!="lung",]
# Recode case/control status
lung_data$case_status = as.factor(ifelse(as.character(lung_data$case_status) == "control",0,1))
bladder_data$case_status = as.factor(ifelse(as.character(bladder_data$case_status) == "control",0,1))

# Make empty table for pvalues (per category), odds ratio and 95% CI
x=model.matrix(~., mydata[,-c(1:4)])[,-1] # Remove base confounders
dim(x)
# four columns for p, or, upper ci and lower ci
# multiplied by four for each lung/bladder and model 1/model 2
forest=matrix(0, nrow=ncol(x), ncol=4*4) %>% data.frame()
rownames(forest)=colnames(x)
cases=c("lung", "bladder")
colnames(forest)=paste0(rep(c("p","or","u95","l95"),times=4),"_",
                        rep(cases,each=4),"_",rep(1:2,each=4))
# four columns for pfor lung/bladder and model 1/model 2
manhattan=matrix(0, nrow=ncol(mydata), ncol=4) %>% data.frame()
rownames(manhattan)=colnames(mydata)
colnames(manhattan)=paste0("p","_",rep(cases,each=2),"_",1:2)

### Logistic regression----
# Models
c=1
i=7
for (c in 1:length(cases)){
  case=cases[c]
  print(case)
  dat=eval(parse(text=paste0(case,"_","data")))
  p.1=NULL
  or.1=NULL
  conf95.1=NULL
  pvar.1=NULL
  p.2=c(rep(NA,2))
  or.2=c(rep(NA,2))
  conf95.2=matrix(0,nrow=3,ncol=2)
  pvar.2=c(NA)
  for (i in 5:ncol(dat)){
    var_name.1=colnames(dat)[i]
    print(var_name.1)
    model1 = glm(paste0("case_status~age_baseline + sex + bmi +", var_name.1),
                 data = dat, family="binomial")
    model0.1 = glm(case_status~age_baseline + sex + bmi,
                   data = dat[complete.cases(dat[,var_name.1]),], family="binomial")
    tmp.1=exp(tail(coef(model1),-8))
    or.1=append(or.1,tmp.1)
    conf95.1=rbind(conf95.1,exp(tail(confint(model1),-8)))
    if(is.numeric(dat[,i])){
      tmp.1=summary(model1)$coefficients[-c(1:8),4]
      p.1=append(p.1,tmp.1)
      pvar.1=append(pvar.1,tmp.1)
    } else {
      p.1=append(p.1,head(summary(model1)$coefficients,-8)[-1,4])
      pvar.1=append(pvar.1,anova(model0.1,model1,test = 'Chisq')$`Pr(>Chi)`[2])
    }
    if(i == 5){
      next
    }
    var_name.2=colnames(dat)[i]
    print(var_name.2)
    model2 = glm(paste0("case_status~age_baseline + sex + bmi + smoking +", var_name.2),
                 data = dat, family="binomial")
    model0.2 = glm(case_status~age_baseline + sex + bmi + smoking,
                   data = dat[complete.cases(dat[,var_name.2]),], family="binomial")
    tmp.2=exp(tail(coef(model2),-10))
    or.2=append(or.2,tmp.2)
    conf95.2=rbind(conf95.2,exp(tail(confint(model2),-10)))
    if(is.numeric(dat[,i])){
      tmp.2=summary(model2)$coefficients[-c(1:10),4]
      p.2=append(p.2,tmp.2)
      pvar.2=append(pvar.2,tmp.2)
    } else {
      p.2=append(p.2,tail(summary(model2)$coefficients,-10)[,4])
      pvar.2=append(pvar.2,anova(model0.2,model2,test = 'Chisq')$`Pr(>Chi)`[2])
    }
  }
  forest[,paste0("p_",case,"_1")]=p.1
  forest[,paste0("p_",case,"_2")]=p.2
  forest[,paste0("or_",case,"_1")]=or.1
  forest[,paste0("or_",case,"_2")]=or.2
  forest[,paste0("u95_",case,"_1")]=conf95.1[,2]
  forest[,paste0("u95_",case,"_2")]=conf95.2[,2]
  forest[,paste0("l95_",case,"_1")]=conf95.1[,1]
  forest[,paste0("l95_",case,"_2")]=conf95.2[,1]
  mahattan[,paste0("p_",case,"_1")]=pvar.1
  mahattan[,paste0("p_",case,"_2")]=pvar.2
}

saveRDS(forest,"../Results/forest_data.rds")
saveRDS(manhattan,"../Results/manhattan_data.rds")
