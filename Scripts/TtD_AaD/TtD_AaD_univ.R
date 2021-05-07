### TDS Project -- Sensitivity: TtD AaD Univariate logistic regression analysis (ARRAY)
### Programme created by Rin Wada on 2 May

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])

library(tidyverse)
arr = c("lung_aad_mx","lung_aad_my","bladder_aad_mx","bladder_aad_my",
        "lung_aad_qx","lung_aad_qy","bladder_aad_qx","bladder_aad_qy",
        "lung_ttd_mx","lung_ttd_my","bladder_ttd_mx","bladder_ttd_my",
        "lung_ttd_qx","lung_ttd_qy","bladder_ttd_qx","bladder_ttd_qy")[m]

data = readRDS(paste0("../Results/TtD_AaD_univ/",arr,".rds"))

### Logistic regression----
# Get beta coefficients, standard errors, glm p-values and anova p-values
foo = function(i, dat, smoking = FALSE){
  var_name=colnames(dat)[i]
  print(var_name)
  confound=c("age_baseline","sex", "bmi")  # Change confounders accordingly
  if(smoking==TRUE){
    confound=append(confound,"smoking")
  }
  model = glm(as.formula(paste("case_status",paste(c(var_name,confound),collapse="+"),sep="~")),
              data = dat, family="binomial")
  model0 = glm(as.formula(paste("case_status",paste(confound,collapse="+"),sep="~")),
               data = dat[complete.cases(dat[,i]),], family="binomial")
  summary_table=summary(model)$coefficients
  res = summary_table[grepl(paste0("^",var_name),rownames(summary_table)),-3]
  return(res)
}
res.1=sapply(5:ncol(data), foo, dat=data)
names(res.1)=colnames(data)[5:ncol(data)]
res.2=sapply(6:ncol(data), foo, dat=data, smoking=T)
names(res.2)=colnames(data)[6:ncol(data)]

saveRDS(res.1, paste0("../Results/TtD_AaD_univ/",arr,"_res.1.rds"))
saveRDS(res.2, paste0("../Results/TtD_AaD_univ/",arr,"_res.2.rds"))

### Make data for forest plot ----
# Create empty rows for smoking variable
smokingNA=matrix(NA, nrow=4, ncol=4)
rownames(smokingNA)=rownames(res.1[[1]])

foo=function(x, smoking = FALSE){
  name=gsub("res","",substitute(x))
  # Extract first element of nest list and rbind for each model results
  table=do.call("rbind", x)
  tmp=apply(table, 1, function(y) exp(y[1]+c(0,-1,1)*qnorm(0.975)*y[2]))
  res=t(tmp)
  res=cbind(res,-log10(table[,3]))
  if(smoking==T){
    # Add NA to model 2 tables
    res=rbind(smokingNA,res)
  }
  # Rename columns
  colnames(res)=paste0(c("or.","l95.","u95.","logp."),name)
  return(res)
}
# cbind into one table
forest=cbind(foo(res.1),foo(res.2, smoking = T))
tmp=as.data.frame(model.matrix(~.,data[,-1])[,-1])
rownames(forest)=colnames(tmp)[-c(1:7)]

saveRDS(forest, paste0("../Results/TtD_AaD_univ/forest_plot_",arr,".rds"))
