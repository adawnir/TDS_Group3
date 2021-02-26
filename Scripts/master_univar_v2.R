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
mydata=cc %>% select(eid, case_status) %>%
  left_join(covar, by="eid") %>% select((eid:sex), bmi, smoking,(ethnic:hh_income),(total_meat:alcohol),
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

#Separate by predictor and response
covar_lung.1 = lung_data %>% select(-case_status,-age_baseline,-sex,-bmi)
covar_lung.2 = lung_data %>% select(-case_status,-age_baseline,-sex,-bmi,-smoking)
Y_lung = lung_data %>% select(case_status)

covar_bladder.1 = bladder_data %>% select(-case_status,-age_baseline,-sex,-bmi)
covar_bladder.2 = bladder_data %>% select(-case_status,-age_baseline,-sex,-bmi,-smoking)
Y_bladder = bladder_data %>% select(case_status)

### Logistic regression----
foo=function(Y_){
  model0 = lmer(X ~ (1|chip) + (1|subject.id) +
                  age + gender + smoking, data = covars, REML = F,
                control= lmerControl(check.conv.singular=.makeCC(action='ignore', tol=1e-04)))
  model1 = lmer(X ~ (1|chip) + (1|subject.id) +
                  age + gender + smoking + bin.expo, data = covars, REML = F,
                control= lmerControl(check.conv.singular=.makeCC(action='ignore', tol=1e-04)))
  vcov = as.data.frame(VarCorr(model1))$vcov
  res = c(summary(model1)$coefficients['bin.expo1', 1:2],
          anova(model0, model1)$`Pr(>Chisq)`[2],
          vcov[1]/sum(vcov), vcov[2]/sum(vcov))
  names(res) = c('coef', 'coef.se', 'pval', 'icc.subject', 'icc.chip')
  return(res)
}
  
)
univ1 <- t(apply(tr_log[,1:1000], 2, FUN = foo))
# Models
for (c in 1:length(cases)){
  case=cases[c]
  print(case)
  dat=eval(parse(text=paste0(case,"_data")))
  Y="case_status"
  confound_m1=c("age_baseline","sex", "bmi")
  confound_m2=c("age_baseline","sex", "bmi", "smoking")
  Xm1=dat[,-which(names(dat) %in% c(Y,confound_m1))]
  Xm2=dat[,-which(names(dat) %in% c(Y,confound_m2))]
  p_cat.1=NULL
  or.1=NULL
  conf95.1=NULL
  p_var.1=NULL
  p_cat.2=NULL
  or.2=NULL
  conf95.2=NULL
  p_var.2=NULL
  for (i in 1:ncol(Xm1)){
    var_name=colnames(Xm1)[i]
    print(var_name)
    model = glm(as.formula(paste(Y,paste(c(var_name,confound_m1),collapse="+"),sep="~")),
                data = dat, family="binomial")
    tmp=c(exp(head(coef(model),-7)[-1]))
    or.1=append(or.1,tmp)
    conf95.1=rbind(conf95.1,exp(head(confint(model),-7)[,-1]))
    if(is.numeric(Xm1[,i])){
      tmp=summary(model)$coefficients[2,4]
      p_cat.1=append(p_cat.1,tmp)
      p_var.1=append(p_var.1,tmp)
    } else {
      model0 = glm(as.formula(paste(Y,paste(confound_m1,collapse="+"),sep="~")),
                   data = dat[complete.cases(Xm1[,i]),], family="binomial")
      p_cat.1=append(p_cat.1,head(summary(model)$coefficients,-7)[-1,4])
      p_var.1=append(p_var.1,anova(model0,model,test = 'Chisq')$`Pr(>Chi)`[2])
    }
  }
  for (i in 1:ncol(Xm2)){
    var_name=colnames(Xm2)[i]
    model = glm(as.formula(paste(Y,paste(c(var_name,confound_m2),collapse="+"),sep="~")),
                data = dat, family="binomial")
    or.1=append(or.1,unlist(exp(head(coef(model),-9)[,-1])))
    conf95.1=rbind(conf95.1,exp(head(confint(model),-9)[,-1]))
    if(is.numeric(Xm2[,i])){
      tmp=summary(model)$coefficients[2,4]
      p_cat.2=append(p_cat.2,tmp)
      p_var.2=append(p_var.2,tmp)
    } else {
      model0 = glm(as.formula(paste(Y,paste(confound_m2,collapse="+"),sep="~")),
                   data = dat[complete.cases(Xm2[,i]),], family="binomial")
      p_cat.2=append(p_cat.2,head(summary(model)$coefficients,-9)[-1,4])
      p_var.2=append(p_var.2,anova(model0,model,test = 'Chisq')$`Pr(>Chi)`[2])
    }
  }
  forest[,paste0("p_",case,"_1")]=p_cat.1
  forest[,paste0("p_",case,"_2")]=p_cat.2
  forest[,paste0("or_",case,"_1")]=or.1
  forest[,paste0("or_",case,"_2")]=or.2
  forest[,paste0("u95_",case,"_1")]=conf95.1[,2]
  forest[,paste0("u95_",case,"_2")]=conf95.2[,2]
  forest[,paste0("l95_",case,"_1")]=conf95.1[,1]
  forest[,paste0("l95_",case,"_2")]=conf95.2[,1]
  mahattan[,paste0("p_",case,"_1")]=p_var.1
  mahattan[,paste0("p_",case,"_2")]=p_var.2
}

saveRDS(forest,"../Results/forest_data.rds")
saveRDS(manhattan,"../Results/manhattan_data.rds")
