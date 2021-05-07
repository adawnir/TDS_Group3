### TDS Project -- Table S2
### Programme created by Rin on 3 April 2020

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# Load packages
library(tidyverse)
library(xtable)

# Load dataset
forest=readRDS("../Results/forest_plot.rds")

# Load references
tab_annot=read_csv("../Dictionaries/univ_tab.csv")

# Reorder variables
rownames(forest)
mydata=forest[c(5:38,1:4,39:nrow(forest)),]
rownames(mydata)

# Check colnames
colnames(forest)

# Reformat OR and CI to OR (l95-u95)
mydata=as.data.frame(mydata)
mydata$or.l1=ifelse(!is.na(mydata$or_lung.1),
                    paste0(round(mydata$or_lung.1,2),
                           " [",
                           round(mydata$l95_lung.1,2),
                           "-",
                           round(mydata$u95_lung.1, 2),
                           "]"),
                    NA)
mydata$or.l2=ifelse(!is.na(mydata$or_lung.2),
                    paste0(round(mydata$or_lung.2,2),
                           " [",
                           round(mydata$l95_lung.2,2),
                           "-",
                           round(mydata$u95_lung.2, 2),
                           "]"),
                    NA)
mydata$or.b1=ifelse(!is.na(mydata$or_bladder.1),
                    paste0(round(mydata$or_bladder.1,2),
                           " [",
                           round(mydata$l95_bladder.1,2),
                           "-",
                           round(mydata$u95_bladder.1, 2),
                           "]"),
                    NA)
mydata$or.b2=ifelse(!is.na(mydata$or_bladder.2),
                    paste0(round(mydata$or_bladder.2,2),
                           " [",
                           round(mydata$l95_bladder.2,2),
                           "-",
                           round(mydata$u95_bladder.2, 2),
                           "]"),
                    NA)
# Reformat p-values (exponentiate)
mydata$p.l1=sapply(mydata$logp_lung.1, function(x) 10^(-x))
mydata$p.l2=sapply(mydata$logp_lung.2, function(x) 10^(-x))

mydata$p.b1=sapply(mydata$logp_bladder.1, function(x) 10^(-x))
mydata$p.b2=sapply(mydata$logp_bladder.2, function(x) 10^(-x))


# Add column with variable category
mydata$cat=tab_annot$cat

# Add column with variable names
mydata$var=tab_annot$var

# Add column with variable level names
mydata$level=tab_annot$level


# Add row with column names
tab=mydata[,c(25,26,27,17,21,18,22,19,23,20,24)]

# Save
ifelse(dir.exists("../Results/tables"),"",dir.create("../Results/tables"))
sink("../Results/tables/univ_tab.txt")
print(xtable(tab,auto = TRUE, digits=2, display = c(rep("s",4),rep(c("s","e"),4))),
      math.style.exponents = TRUE,
      include.rownames = FALSE, include.colnames = FALSE,
      tabular.environment = "longtable",
      floating = FALSE,
      hline.after = c(0,nrow(tab)))
sink()

