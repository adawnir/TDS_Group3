# Manhattan Plots
# Created by Ines on Feb 18, revisted Feb 25

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

suppressPackageStartupMessages(library(RColorBrewer))
install.packages("qqman")
library(qqman)
library(openxlsx)

#  takes p values from results & combines with corresponding positions in annot

#### TO DO: ----
# GROUP variables (our variables don't have an order like chromosome position)
# instead we group them categorically, then coerce the category to numeric so its ordered by position
results=read.xlsx("../Dictionaries/manhattan.xlsx")
results$pval<-as.numeric(results$pval)
results$group <- as.integer(results$group)

names(results)
GroupVars <-c("demographic", "social", "early life", "health risk", "environment", "medical", "biomarkers")

bonf <- 0.05/(77*2*154)
## create dataframe called results that pretends to be GWAS so we can use the manhattan() function from qqman
# (see https://cran.r-project.org/web/packages/qqman/vignettes/qqman.html)

# results:
# SNP: chr <name of covariate> ...bmi.lung, bmi.bladder??
# CHR: int (Group Variable above coerced to number from a factor) (MUST BE NUMERIC)
# BP: order of covariates within groups (arbitrary)
# P: p values (one for lung, one for bladder, in order)
# we will not have columns named this way so need to specify in manhattan call

# vector of covars we want to highlight
SigVars<-c("")

manhattan(x=results, chr = "group", bp = "position", p = "pval", snp = "covar",  main = "Manhattan Plot", ylim = c(0,350), xlab="Variable Group",
          col = c("deeppink", "cyan", "green", "orange", "blue", "violet","firebrick"), suggestiveline = F, genomewideline = bonf, # highlight = SigVars, 
          annotateTop = F, annotatePval=bonf)

legend("topright",legend=GroupVars,
       col=c("deeppink", "cyan", "green", "orange", "blue", "violet","firebrick"), lty=1:2, cex=0.6)

# highlight significant variables: