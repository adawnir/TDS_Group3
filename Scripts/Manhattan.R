# Manhattan Plot
# Ines losing her goddamn mind Feb 18

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
results=read.csv("../Dictionaries/manhattan.csv")

GroupVars <-c("demographic", "social", "early life", "health risk", "environment", "medical", "biomarkers")

## create dataframe called results that pretends to be GWAS so we can use the manhattan() function from qqman
# (see https://cran.r-project.org/web/packages/qqman/vignettes/qqman.html)

# results:
# SNP: chr <name of covariate> ...bmi.lung, bmi.bladder??
# CHR: int (Group Variable above coerced to number from a factor) (MUST BE NUMERIC)
# BP: order of covariates within groups (arbitrary)
# P: p values (one for lung, one for bladder, in order)
# we will not have columns named this way so need to specify in manhattan call

manhattan(results, bp = position, p = pval, snp = covar, chr = group, main = "Manhattan Plot", ylim = c(0, 10), cex = 0.6, 
          cex.axis = 0.9, col = c("firebrick", "cyan"), suggestiveline = F, genomewideline = F, 
          chrlabs = c(1:20, "P", "Q"), annotatePval=0.01)
 



#### Reads in a dataframe of 
# results = coefficient, coefficient SE, p value, icc subject, icc chip
# annot = annotation file, includes names of chromosomes & genome position
# these dfs are same length


### MANHATTAN PLOT CODE FROM WEEK 1 COMP EPI
Manhattan = function(results, annot = NULL, annotate = FALSE, thr = 0.05) {
  nchr = max(as.numeric(annot$Chromosome), na.rm = TRUE) # grouped by chromosome: these are all dif colors - need to remain so grouped by TYPE of variable?
  annot = annot[rownames(results), ]
  annot = annot[rownames(results), ]
  library(RColorBrewer)
  mypal = brewer.pal(n = 12, name = "Paired")
  colors = colorRampPalette(c("navy", "blue", "skyblue"))(24)
  colors = colorRampPalette(mypal)(24)
  par(mar = c(3, 4.5, 1, 1))
  plot(annot$covar, -log10(results$pval), # need to rename here so location corresponds to variable
       col = colors[annot$GroupVars], pch = 19, cex = 0.5,
       xlab = "", ylab = expression(-log[10](p[value])),
       las = 1, xaxt = "n")
  axis(side = 1, at = chr_boundaries[-length(chr_boundaries)] + # RETHINK IF NOT REMOVE THIS
         (chr_boundaries[-1] - chr_boundaries[-length(chr_boundaries)])/2,
       labels = c(seq(1:22), "X", "Y")[1:nchr], tick = FALSE,
       cex.axis = 0.6)
  axis(side = 1, at = chr_boundaries, labels = NA, 
       col.ticks = "grey")
  abline(h = -log10(0.05/nrow(results)), lty = 2,
         col = "darkred")
  if (annotate) { 
    text(annot$GenomePosition, -log10(results$pval),
         pos = 3, offset = 0.2, cex = 0.5, labels = ifelse(results$pval <
                                                             thr, yes = annot$Symbol, no = ""))
  }
}

#### MANHATTAN PLOT CODE FROM WEEK 4 COMP EPI ----
#### Manhattan plots

Table=read.table("Tables/Univariate_full_non_adjusted_wbc.txt")
Table_wbc=read.table("Tables/Univariate_subset_adjusted_wbc.txt")
Table_no_wbc=read.table("Tables/Univariate_subset_non_adjusted_wbc.txt")

MyPal=brewer.pal("Paired", n = 12)
MyPalPairsExt <- colorRampPalette(MyPal)(28)

Groups=c("EGF", "FGF2", "GCSF", "VEGF", "GMSCF", "TGFa",
         "Eotaxin", "Fractalkine", "GRO", "MCP1", "MCP3", "MDC", "MIP1a", "MIP1b", "IP10", "IL8", 
         "IL1b", "IL2", "IL4", "IL5", "IL6", "IL7", "IL10", "IL13", "INFa", "INFg", "TNFa", "sCD40L")

Table=Table[Groups, ]
Table_wbc=Table_wbc[Groups, ]
Table_no_wbc=Table_no_wbc[Groups, ]

plot(-log10(Table$Pooled.Pvalue), pch=17, col=ifelse(Table$Pooled.Beta<0, yes=MyPal[2], no=MyPal[8]), 
     xaxt="n", ylab=expression(-log[10](italic(p))), xlab="", las=1,
     ylim=c(min(c(-log10(Table$Pooled.Pvalue), -log10(Table_wbc$Pooled.Pvalue))), max(c(-log10(Table$Pooled.Pvalue), -log10(Table_wbc$Pooled.Pvalue)))))
points(-log10(Table_wbc$Pooled.Pvalue), pch=19, col=ifelse(Table_wbc$Pooled.Beta<0, yes=MyPal[2], no=MyPal[8]))
points(-log10(Table_no_wbc$Pooled.Pvalue), pch=18, col=ifelse(Table_no_wbc$Pooled.Beta<0, yes=MyPal[2], no=MyPal[8]))
abline(h=-log10(0.05/28), col="red")
abline(v=seq(1, 28), lty=3, col="grey")
axis(1, at=1:28, ifelse(Table_wbc$Pooled.Beta<0, yes=rownames(Table), no=''), las=2, col.axis=MyPal[2])
axis(1, at=1:28, ifelse(Table_wbc$Pooled.Beta<0, yes='', no=rownames(Table)), las=2, col.axis=MyPal[8])
legend("topright", pch=c(19, 17, 18, 19, 19), col=c("black", "black", "black", MyPal[2], MyPal[8]),
       legend = c("Subset adjusted on WBC", "Full not adjusted on WBC", "Subset not adjusted on WBC", paste("Negative", expression(beta)), paste("Positive", expression(beta))))
dev.off()


### By subtype

for(k in 1:4){
  subtype=c("BCLL", "DLBL", "FL", "MM")[k]
  subtype_name=c("CLL", "DLBCL", "FL", "MM")[k]
  
  Table1=eval(parse(text=paste0("Table_", subtype)))
  Table2=eval(parse(text=paste0("Table_", subtype, "_wbc")))
  Table3=eval(parse(text=paste0("Table_", subtype, "_no_wbc")))
  
  Table1=Table1[Groups, ]
  Table2=Table2[Groups, ]
  Table3=Table3[Groups, ]
  
  pdf(paste0("Figures/Fig1_", subtype_name, ".pdf"), height = 7, width = 10)
  par(mar=c(6,5,3,3))
  plot(-log10(Table1[,2]), pch=17, col=ifelse(Table1[,1]<0, yes=MyPal[2], no=MyPal[8]), 
       xaxt="n", ylab=expression(-log[10](italic(p))), xlab="", las=1,
       ylim=c(min(c(-log10(Table$MM.Pvalue), -log10(Table_wbc$MM.Pvalue))), max(c(-log10(Table$MM.Pvalue), -log10(Table_wbc$MM.Pvalue)))),
       main=paste0(subtype_name, " (N=", sum(covars$type==0)+sum(covars$LY_subtype==subtype), ")"))
  points(-log10(Table2[,2]), pch=19, col=ifelse(Table2[,1]<0, yes=MyPal[2], no=MyPal[8]))
  points(-log10(Table3[,2]), pch=18, col=ifelse(Table3[,1]<0, yes=MyPal[2], no=MyPal[8]))
  abline(h=-log10(0.05/28), col="red")
  abline(v=seq(1, 28), lty=3, col="grey")
  axis(1, at=1:28, ifelse(Table2[,1]<0, yes=rownames(Table), no=''), las=2, col.axis=MyPal[2])
  axis(1, at=1:28, ifelse(Table2[,1]<0, yes='', no=rownames(Table)), las=2, col.axis=MyPal[8])
  legend("topright", pch=c(19, 17, 18, 19, 19), col=c("black", "black", "black", MyPal[2], MyPal[8]),
         legend = c("Subset adjusted on WBC", "Full not adjusted on WBC", "Subset not adjusted on WBC", paste("Negative", expression(beta)), paste("Positive", expression(beta))))
  dev.off()
}
