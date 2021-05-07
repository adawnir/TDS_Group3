### TDS Project -- Composite score PCA table
### Programme created by Rin on 2 May 2020

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# Load packages
library(tidyverse)
library(xtable)

### Physical activity ----
# Load dataset
loadings=readRDS("../Results/phys_act_loadings.rds")
ev=readRDS("../Results/phys_act_ev.rds")

phys_act = rbind(loadings,ev)

# change row names
rownames(phys_act) = c("Number of days/week walked 10+ min",
                       "Number of days/week moderate activity 10+ min",
                       "Number of days/week vigorous activity 10+ min",
                       "Explained variance")
# round decimals
phys_act = round(phys_act,2)

# Save
ifelse(dir.exists("../Results/tables"),"",dir.create("../Results/tables"))
sink("../Results/tables/phys_act_tab.txt")
print(xtable(phys_act,auto = TRUE),
      include.rownames = TRUE, include.colnames = TRUE,
      tabular.environment = "longtable",
      floating = FALSE,
      hline.after = c(0,nrow(phys_act)-1))
sink()

### Diet ----
# Load dataset
loadings=readRDS("../Results/diet_loadings.rds")
ev=readRDS("../Results/diet_ev.rds")

diet = rbind(loadings,ev)

# change row names
rownames(diet) = c("Oily fish","Non-oily fish","Poultry","Processed meat","Beef","Lamb/mutton",
                   "Pork", "Cooked vegetables","Salad/fresh vegetables","Fresh fruits",
                   "Explained variance")
# round decimals
diet = round(diet,2)

# Save
ifelse(dir.exists("../Results/tables"),"",dir.create("../Results/tables"))
sink("../Results/tables/diet_tab.txt")
print(xtable(diet,auto = TRUE),
      include.rownames = TRUE, include.colnames = TRUE,
      tabular.environment = "longtable",
      floating = FALSE,
      hline.after = c(0,nrow(diet)-1))
sink()

### bev ----
# Load dataset
loadings=readRDS("../Results/bev_loadings.rds")
ev=readRDS("../Results/bev_ev.rds")

bev = rbind(loadings,ev)

# change row names
rownames(bev) = c("Coffee (cups) per day","Tea (cups) per day","Water (glasses) per day",
                   "Explained variance")
# round decimals
bev = round(bev,2)

# Save
ifelse(dir.exists("../Results/tables"),"",dir.create("../Results/tables"))
sink("../Results/tables/bev_tab.txt")
print(xtable(bev,auto = TRUE),
      include.rownames = TRUE, include.colnames = TRUE,
      tabular.environment = "longtable",
      floating = FALSE,
      hline.after = c(0,nrow(bev)-1))
sink()
