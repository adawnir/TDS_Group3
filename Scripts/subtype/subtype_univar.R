# subtype univariate analysis
# by ines on march 22

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(tidyverse)
library(parallel)

covar=readRDS("../Results/covar_models.rds")
biomarkers=readRDS("../Results/biomarker_imp_master.rds")
cc=readRDS("../Results/case_control.rds")
subtype=readRDS("../Results/lung_subtype.rds")

covar = covar %>%
  select((eid:sex), bmi,smoking,(ethnic:hh_income),(phys_score:alcohol),
         (mat_smoke:autoimmune))

mydata=cc %>% select(eid, case_status) %>%
  left_join(covar, by="eid") %>%
  left_join(biomarkers, by="eid") %>%
  left_join(subtype, by="eid") %>%
  select(-eid)

colnames(mydata)
# Scale biomarkers and continuous environmental variables
# (all continuous variables that are not composite scores)
mydata[c(27:32,49:76)]=scale(mydata[c(27:32,49:76)])

summary(mydata)

# get rid of all bladder cases & recode case control status
lung_data=mydata[mydata$case_status!="bladder",]
lung_data$case_status = as.factor(ifelse(as.character(lung_data$case_status) == "control",0,1))

# separate by subtype
subtypes <- levels(subtype$subtype)
insitu.lung=subset(lung_data, subtype==subtypes[1] | is.na(subtype))
insitu.trachea=subset(lung_data, subtype==subtypes[2] | is.na(subtype)) # no trachea cases
mal.trachea=subset(lung_data, subtype==subtypes[3] | is.na(subtype))
mal.lung=subset(lung_data, subtype==subtypes[4] | is.na(subtype))
mal.lower=subset(lung_data, subtype==subtypes[5] | is.na(subtype))
mal.main=subset(lung_data, subtype==subtypes[6] | is.na(subtype))
mal.middle=subset(lung_data, subtype==subtypes[7] | is.na(subtype))
mal.overlap=subset(lung_data, subtype==subtypes[8] | is.na(subtype))
mal.upper=subset(lung_data, subtype==subtypes[9] | is.na(subtype))


# separate by subtype
subtypes <- levels(subtype$subtype)
levels(subtype$subtype) <- c("insitu.lung","insitu.trachea","mal.trachea","mal.lung","mal.lower","mal.main","mal.middle","mal.overlap","mal.upper")

# remove subtype variable before doing analysis
insitu.lung<- insitu.lung[-c(77)]
mal.lower<- mal.lower[-c(77)]
mal.lung<- mal.lung[-c(77)]
mal.main<- mal.main[-c(77)]
mal.middle<- mal.middle[-c(77)]
mal.overlap<- mal.overlap[-c(77)]
mal.trachea<- mal.trachea[-c(77)]
mal.upper<- mal.upper[-c(77)]


### Logistic regression----
# Get beta coefficients, standard errors, glm p-values and anova p-values
foo = function(i, dat, smoking = FALSE){
  var_name=colnames(dat)[i]
  print(var_name)
  confound=c("age_baseline", "sex", "bmi")  # Change confounders accordingly
  if(smoking==TRUE){
    confound=append(confound,"smoking")
  }
  model = glm(as.formula(paste("case_status",paste(c(var_name,confound),collapse="+"),sep="~")),
              data = dat, family="binomial")
  model0 = glm(as.formula(paste("case_status",paste(confound,collapse="+"),sep="~")),
               data = dat[complete.cases(dat[,i]),], family="binomial")
  summary_table=summary(model)$coefficients
  res = list(summary_table[grepl(paste0("^",var_name),rownames(summary_table)),-3],
             anova(model0, model, test = 'Chisq')$`Pr(>Chi)`[2])
  names(res) = c('glm_output', 'anova_output')
  return(res)
}

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("insitu.lung", "foo"))
insitu.lung.1=parLapply(cl=cl, 5:ncol(insitu.lung), foo, dat=insitu.lung)
names(insitu.lung.1)=colnames(insitu.lung)[5:ncol(insitu.lung)]
insitu.lung.2=parLapply(cl=cl, 6:ncol(insitu.lung), foo, dat=insitu.lung, smoking=T)
names(insitu.lung.2)=colnames(insitu.lung)[6:ncol(insitu.lung)]
stopCluster(cl)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("mal.lower", "foo"))
mal.lower.1=parLapply(cl=cl, 5:ncol(mal.lower), foo, dat=mal.lower)
names(mal.lower.1)=colnames(mal.lower)[5:ncol(mal.lower)]
mal.lower.2=parLapply(cl=cl, 6:ncol(mal.lower), foo, dat=mal.lower, smoking=T)
names(mal.lower.2)=colnames(mal.lower)[6:ncol(mal.lower)]
stopCluster(cl)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("mal.lung", "foo"))
mal.lung.1=parLapply(cl=cl, 5:ncol(mal.lung), foo, dat=mal.lung)
names(mal.lung.1)=colnames(mal.lung)[5:ncol(mal.lung)]
mal.lung.2=parLapply(cl=cl, 6:ncol(mal.lung), foo, dat=mal.lung, smoking=T)
names(mal.lung.2)=colnames(mal.lung)[6:ncol(mal.lung)]
stopCluster(cl)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("mal.main", "foo"))
mal.main.1=parLapply(cl=cl, 5:ncol(mal.main), foo, dat=mal.main)
names(mal.main.1)=colnames(mal.main)[5:ncol(mal.main)]
mal.main.2=parLapply(cl=cl, 6:ncol(mal.main), foo, dat=mal.main, smoking=T)
names(mal.main.2)=colnames(mal.main)[6:ncol(mal.main)]
stopCluster(cl)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("mal.middle", "foo"))
mal.middle.1=parLapply(cl=cl, 5:ncol(mal.middle), foo, dat=mal.middle)
names(mal.middle.1)=colnames(mal.middle)[5:ncol(mal.middle)]
mal.middle.2=parLapply(cl=cl, 6:ncol(mal.middle), foo, dat=mal.middle, smoking=T)
names(mal.middle.2)=colnames(mal.middle)[6:ncol(mal.middle)]
stopCluster(cl)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("mal.overlap", "foo"))
mal.overlap.1=parLapply(cl=cl, 5:ncol(mal.overlap), foo, dat=mal.overlap)
names(mal.overlap.1)=colnames(mal.overlap)[5:ncol(mal.overlap)]
mal.overlap.2=parLapply(cl=cl, 6:ncol(mal.overlap), foo, dat=mal.overlap, smoking=T)
names(mal.overlap.2)=colnames(mal.overlap)[6:ncol(mal.overlap)]
stopCluster(cl)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("mal.trachea", "foo"))
mal.trachea.1=parLapply(cl=cl, 5:ncol(mal.trachea), foo, dat=mal.trachea)
names(mal.trachea.1)=colnames(mal.trachea)[5:ncol(mal.trachea)]
mal.trachea.2=parLapply(cl=cl, 6:ncol(mal.trachea), foo, dat=mal.trachea, smoking=T)
names(mal.trachea.2)=colnames(mal.trachea)[6:ncol(mal.trachea)]
stopCluster(cl)

no_cores=detectCores()-1
cl <- makeCluster(no_cores) 
clusterExport(cl, c("mal.upper", "foo"))
mal.upper.1=parLapply(cl=cl, 5:ncol(mal.upper), foo, dat=mal.upper)
names(mal.upper.1)=colnames(mal.upper)[5:ncol(mal.upper)]
mal.upper.2=parLapply(cl=cl, 6:ncol(mal.upper), foo, dat=mal.upper, smoking=T)
names(mal.upper.2)=colnames(mal.upper)[6:ncol(mal.upper)]
stopCluster(cl)

arr=paste0(rep(c("insitu.lung","mal.trachea","mal.lung","mal.lower","mal.main","mal.middle","mal.overlap","mal.upper"),each=2),".",1:2)
ifelse(dir.exists("../Results/strat_site_univ"),"",dir.create("../Results/strat_site_univ"))
for (i in 1:length(arr)) {
  saveRDS(eval(parse(text=arr[i])),paste0("../Results/strat_site_univ/",arr[i],".rds"))
}

### Make data for forest plot ----
# Create empty rows for smoking variable
smokingNA=matrix(NA, nrow=4, ncol=4)
rownames(smokingNA)=rownames(insitu.lung.1[[1]]$glm_output)

foo=function(x, smoking = FALSE){
  # Extract first element of nest list and rbind for each model results
  table=do.call("rbind", lapply(x, `[[`, 1))
  tmp=apply(table, 1, function(y) exp(y[1]+c(0,-1,1)*qnorm(0.975)*y[2]))
  res=t(tmp)
  res=cbind(res,-log10(table[,3]))
  if(smoking==T){
    # Add NA to model 2 tables
    res=rbind(smokingNA,res)
  }
  # Rename columns
  colnames(res)=paste0(c("or_","l95_","u95_","logp_"), substitute(x))
  return(res)
}
# cbind into one table
forest=cbind(foo(insitu.lung.1),foo(insitu.lung.2, smoking = T),
             foo(mal.lower.1),foo(mal.lower.2, smoking = T),
             foo(mal.upper.1),foo(mal.upper.2, smoking = T),
             foo(mal.trachea.1),foo(mal.trachea.2, smoking = T),
             foo(mal.middle.1),foo(mal.middle.2, smoking = T),
             foo(mal.main.1),foo(mal.main.2, smoking = T),
             foo(mal.lung.1),foo(mal.lung.2, smoking = T),
             foo(mal.overlap.1),foo(mal.overlap.2, smoking = T))
tmp=as.data.frame(model.matrix(~.,insitu.lung.1[,-1])[,-1])
mynames=colnames(tmp)
rownames(forest)=mynames[-c(1:7)]
saveRDS(forest, "../Results/strat_site_univ/forest_plot.rds")

### Make data set for Manhattan plot ----
manhattan=cbind(do.call("rbind", lapply(insitu.lung.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(insitu.lung.2, `[[`, 2))),
                do.call("rbind", lapply(mal.lower.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(mal.lower.2, `[[`, 2))),
                do.call("rbind", lapply(mal.lung.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(mal.lung.2, `[[`, 2))),
                do.call("rbind", lapply(mal.main.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(mal.main.2, `[[`, 2))),
                do.call("rbind", lapply(mal.middle.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(mal.middle.2, `[[`, 2))),
                do.call("rbind", lapply(mal.overlap.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(mal.overlap.2, `[[`, 2))),
                do.call("rbind", lapply(mal.trachea.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(mal.trachea.2, `[[`, 2))),
                do.call("rbind", lapply(mal.upper.1, `[[`, 2)),
                c(NA, do.call("rbind", lapply(mal.upper.2, `[[`, 2))))

# Change to log scale
manhattan=-log10(manhattan)
# Change column name
colnames(manhattan)=arr
saveRDS(manhattan, "../Results/strat_site_univ/manhattan_plot.rds")

