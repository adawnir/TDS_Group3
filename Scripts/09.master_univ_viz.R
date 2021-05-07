### TDS Project --  Univariate analysis Visualisation (Forest plot and Manhattan plot)
## Programme created by Ines on 11 March reviewed by Rin Wada on 15 March

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/"
setwd(project_path)

# Loading packages
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(ggrepel)

# plot labels
setwd("../Dictionaries")
manhat_annot=read_csv("manhat_annot.csv")
plot_annot=read_csv("plot_annot.csv")[-c(1,2,21:25),] # removing base model confounders


### MANHATTAN ----
manhat=as.data.frame(readRDS("../Results/manhattan_plot.rds"))
bonf=-log10(0.05/nrow(manhat))
# manhat=manhat[-1,] # Remove smoking

# Convert p=0 lung smoking to maximum
manhat["smoking","lung.1"]=max(manhat[which(manhat<Inf),1:2],na.rm=T)
# settings
mylabels=manhat_annot$label
variable_cat=c(rep(NA,1), # smoking in first column
               rep("Sociodemographic",8),
               rep("Health risk", 12),
               rep("Environmental", 8),
               rep("Medical", 15), 
               rep("Biomarkers", 28))
models=c("Base model", "Model adjusted on smoking status")
mycolours=c("navy","red","darkmagenta")
xseq=seq(1,nrow(manhat))


# Lung
pdf("../Figures/Final/Presentation/manhattan_lung.pdf", height = 5, width = 10)
par(mar = c(10, 3, 3, 5))
plot(manhat[,1], pch = 17,col=mycolours[1], cex=0.7,
     xaxt = "n", yaxt="n", ylab="", xlab="",
     ylim = c(min(manhat[,1:2],na.rm=T), max(manhat[,1:2],na.rm=T)+1))
axis(side=4, at=axTicks(2), cex.axis=0.7)
abline(h=bonf, lty = 2, col="darkred")
abline(h=-log10(0.05), lty = 2, col="grey")
abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
mtext(side=4, text=expression(-log[10](italic(p))), line=2, cex=0.7)
points(manhat[,1], pch = 17, col=mycolours[1], cex=0.7)
points(manhat[,2], pch = 19, col=mycolours[2], cex=0.7)
for (k in 1:length(xseq)){
  mytext=mylabels[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  labcol=ifelse((manhat[k,1]>=bonf & manhat[k,2]>=bonf), mycolours[3],
                ifelse(manhat[k,1]>=bonf,mycolours[1],
                       ifelse(manhat[k,2]>=bonf, mycolours[2],"black")))
  if(is.na(labcol)){
    labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
                  ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  }
  labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7,
       col.axis=labcol, col = labcol, font=labfont)
}
xseqblack=c(xseq[!duplicated(variable_cat)]-1/2, max(xseq)+1/2)
abline(v=xseqblack,lty=3,col="black")
for (k in 2:(length(xseqblack)-1)){
  axis(side=3, at=xseqblack[c(k,k+1)]+c(0.5,-0.5), line=0.5, labels=NA)
}
for (k in 2:(length(xseqblack)-1)){
  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
       labels=unique(variable_cat)[k], cex.axis=0.9)
}
legend("topright", pch=c(17, 19, NA, NA, NA), lty=c(NA, NA, 2, 2,NA),
       col=c(mycolours[1:2],"darkred","grey"),
       legend=c(models,"Bonferroni threshold","Nominal threshold",
                expression(paste("*",symbol("<"),2.23 %*% 10^-308))),
       cex=0.7, bg = "white")
text(xseq[1]+0.5,manhat[1,1],expression(paste("*")))
dev.off()

# Bladder
pdf("../Figures/Final/Presentation/manhattan_bladder.pdf", height = 5, width = 10)
par(mar = c(10, 3, 3, 5))
plot(manhat[,3], pch = 17,col=mycolours[1], cex=0.7,
     xaxt = "n", yaxt="n", ylab="", xlab="",
     ylim = c(min(manhat[3:4], na.rm = T), max(manhat[3:4], na.rm = T)+1))
axis(side=4, at=axTicks(2), cex.axis=0.7)
abline(h=bonf, lty = 2, col="darkred")
abline(h=-log10(0.05), lty = 4, col="grey")
abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
mtext(side=4, text=expression(-log[10](italic(p))), line=2, cex=0.7)
points(manhat[,3], pch = 17, col=mycolours[1], cex=0.7)
points(manhat[,4], pch = 19, col=mycolours[2], cex=0.7)
for (k in 1:length(xseq)){
  mytext=mylabels[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  labcol=ifelse((manhat[k,3]>=bonf & manhat[k,4]>=bonf), mycolours[3],
                ifelse(manhat[k,3]>=bonf,mycolours[1],
                       ifelse(manhat[k,4]>=bonf, mycolours[2],"black")))
  if(is.na(labcol)){
    labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
                  ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  }
  labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7,
       col.axis=labcol, col = labcol, font=labfont)
}
xseqblack=c(xseq[!duplicated(variable_cat)]-1/2, max(xseq)+1/2)
abline(v=xseqblack,lty=3,col="black")
for (k in 2:(length(xseqblack)-1)){
  axis(side=3, at=xseqblack[c(k,k+1)]+c(0.5,-0.5), line=0.5, labels=NA)
}
for (k in 2:(length(xseqblack)-1)){
  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
       labels=unique(variable_cat)[k], cex.axis=0.9)
}
legend("topright", pch=c(17, 19, NA, NA), lty=c(NA, NA, 2, 2),
       col=c(mycolours[1:2],"darkred","grey"), legend=c(models,"Bonferroni threshold","Nominal threshold"),
       cex=0.7, bg = "white")
dev.off()

### SCATTER PLOTS ----
forest=as.data.frame(readRDS("../Results/forest_plot.rds"))
mynames=rownames(forest)

# Reorder
myorder=c(mynames[5:38],mynames[1:4],mynames[39:length(mynames)])
forest=forest %>%
  slice(match(myorder,rownames(forest)))

# Labels for significant
mylabels=plot_annot$label.point
bonf=-log10(0.05/72)
models=c("Base model", "Model adjusted on smoking status")
mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")

pval=forest[,which(grepl("logp_",colnames(forest)))] # Extract pval
or=forest[,-which(grepl("logp_",colnames(forest)))] # Extract or and ci

pval[which(pval==Inf),1]=max(pval[which(pval<Inf),c(1,3)], na.rm=TRUE)
mylabels_asterisk = mylabels
mylabels_asterisk[which(mylabels_asterisk=="Current smoker")]="Current smoker*"

or$label=1:nrow(or)
or$mycolour=rep(mycolours,times=c(18,20,8,16,28))
or$mycolour_lab=darken(or$mycolour, amount=0.5)

pval$label=1:nrow(pval)
pval$mycolour=rep(mycolours,times=c(18,20,8,16,28))
pval$mycolour_lab=darken(pval$mycolour, amount=0.5)

### Presentation (Male, Base model: zoomed out)  ----
xlim=c(min(or[,c(1:3,7:9)],na.rm=T),max(or[,1:3],na.rm=T))
ylim=c(min(or[,c(1:3,7:9)],na.rm=T),max(or[,7:9],na.rm=T))

p1 <- ggplot(or, aes(x=or_lung.1, y=or_bladder.1,
                     label=ifelse((pval$logp_lung.1>=bonf&pval$logp_bladder.1>=bonf),
                                  mylabels,""))) + 
  geom_abline(linetype = 'dotted', colour = 'grey') +
  geom_errorbar(aes(ymin=l95_bladder.1, ymax=u95_bladder.1),colour=ifelse((pval$logp_lung.1>=bonf|pval$logp_bladder.1>=bonf),or$mycolour,alpha("grey90",0.01)), width=.01*xlim[2]) +
  geom_errorbarh(aes(xmin=l95_lung.1, xmax=u95_lung.1),colour=ifelse((pval$logp_lung.1>=bonf|pval$logp_bladder.1>=bonf),or$mycolour,alpha("grey90",0.01)), height=.01*ylim[2]) +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_label_repel(size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.02, nudge_x = 0.02,
                   box.padding = .5,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  geom_point(colour=ifelse((pval$logp_lung.1>=bonf|pval$logp_bladder.1>=bonf),or$mycolour,alpha("grey90",0.01))) +
  xlim(xlim) +
  ylim(ylim) +
  ylab('OR [95% CI] (Bladder cancer vs control)') +
  xlab('OR [95% CI] (Lung cancer vs control)') +
  ggtitle(models[1]) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(0,max(pval[,1],na.rm=T))
ylim=c(0,max(pval[,3],na.rm=T))

p2 = ggplot(pval,aes(logp_lung.1, logp_bladder.1,
                     label = ifelse((logp_lung.1>=bonf&logp_bladder.1>=bonf),
                                    mylabels_asterisk,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_point(color = ifelse((pval$logp_lung.1>=bonf|pval$logp_bladder.1>=bonf),
                            pval$mycolour, "grey90")) +
  geom_label_repel(size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.02, nudge_x = 0.02,
                   box.padding = .5, point.padding = 0.1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  labs(title = models[1]) +
  xlab(expression(-log[10](italic(p))~"(Lung cancer vs control)"))+
  ylab(expression(-log[10](italic(p))~"(Bladder cancer vs control)")) +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw()

pdf("../Figures/Final/Presentation/univ_base_or.pdf", width = 5, height = 5)
p1
dev.off()
pdf("../Figures/Final/Presentation/univ_base_pval.pdf", width = 5, height = 5)
p2
dev.off()

### Presentation (Male, Base model: smoking only)  ----
xlim=c(min(or[,c(1:3,7:9)],na.rm=T),max(or[,1:3],na.rm=T))
ylim=c(min(or[,c(1:3,7:9)],na.rm=T),max(or[,7:9],na.rm=T))

p1 <- ggplot(or, aes(x=or_lung.1, y=or_bladder.1,
                     label=ifelse(grepl("smok",rownames(or)),mylabels,""))) + 
  geom_abline(linetype = 'dotted', colour = 'grey') +
  geom_errorbar(aes(ymin=l95_bladder.1, ymax=u95_bladder.1),colour=ifelse(grepl("smok",rownames(or)),or$mycolour,alpha("grey90",0.01)), width=.01*xlim[2]) +
  geom_errorbarh(aes(xmin=l95_lung.1, xmax=u95_lung.1),colour=ifelse(grepl("smok",rownames(or)),or$mycolour,alpha("grey90",0.01)), height=.01*ylim[2]) +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_label_repel(size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.02, nudge_x = 0.02,
                   box.padding = .5,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  geom_point(colour=ifelse(grepl("smok",rownames(or)),or$mycolour,alpha("grey90",0.01))) +
  xlim(xlim) +
  ylim(ylim) +
  ylab('OR [95% CI] (Bladder cancer vs control)') +
  xlab('OR [95% CI] (Lung cancer vs control)') +
  ggtitle(models[1]) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(0,max(pval[,1],na.rm=T))
ylim=c(0,max(pval[,3],na.rm=T))

p2 = ggplot(pval,aes(logp_lung.1, logp_bladder.1,
                     label = ifelse(grepl("smok",rownames(or)),mylabels_asterisk,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_point(color = ifelse(grepl("smok",rownames(or)),pval$mycolour, alpha("grey90",0.01))) +
  geom_label_repel(size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.02, nudge_x = 0.02,
                   box.padding = .5, point.padding = 0.1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  labs(title = models[1]) +
  xlab(expression(-log[10](italic(p))~"(Lung cancer vs control)"))+
  ylab(expression(-log[10](italic(p))~"(Bladder cancer vs control)")) +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw()

pdf("../Figures/Final/Presentation/univ_base_or_smok.pdf", width = 5, height = 5)
p1
dev.off()
pdf("../Figures/Final/Presentation/univ_base_pval_smok.pdf", width = 5, height = 5)
p2
dev.off()


### Presentation (Male, Adjusted model: zoomed out)  ----
xlim=c(min(or[,c(4:6,10:12)],na.rm=T),max(or[,4:6],na.rm=T))
ylim=c(min(or[,c(4:6,10:12)],na.rm=T),max(or[,10:12],na.rm=T))

p3 <- ggplot(or, aes(x=or_lung.2, y=or_bladder.2,
                     label=ifelse((pval$logp_lung.2>=bonf&pval$logp_bladder.2>=bonf)|
                                    pval$logp_bladder.2>=bonf|
                                    pval$logp_lung.2>=bonf&abs(or_lung.2-1)>1,
                                  mylabels,""))) + 
  geom_abline(linetype = 'dotted', colour = 'grey') +
  geom_errorbar(aes(ymin=l95_bladder.2, ymax=u95_bladder.2),colour=ifelse((pval$logp_lung.2>=bonf|pval$logp_bladder.2>=bonf),or$mycolour,alpha("grey90",0.01)), width=.01*xlim[2]) +
  geom_errorbarh(aes(xmin=l95_lung.2, xmax=u95_lung.2),colour=ifelse((pval$logp_lung.2>=bonf|pval$logp_bladder.2>=bonf),or$mycolour,alpha("grey90",0.01)), height=.01*ylim[2]) +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_point(colour=ifelse((pval$logp_lung.2>=bonf|pval$logp_bladder.2>=bonf),or$mycolour,alpha("grey90",0.01))) +
  geom_label_repel(size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.02, nudge_x = 0.02,
                   box.padding = .5,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlim(xlim) +
  ylim(ylim) +
  ylab('OR [95% CI] (Bladder cancer vs control)') +
  xlab('OR [95% CI] (Lung cancer vs control)') +
  ggtitle(models[2]) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(0,max(pval[,2],na.rm=T))
ylim=c(0,max(pval[,4],na.rm=T))

p4 = ggplot(pval,aes(logp_lung.2, logp_bladder.2,
                     label = ifelse((logp_lung.2>=bonf&logp_bladder.2>=bonf)|
                                      logp_bladder.2>=bonf|
                                      (logp_lung.2>=bonf&logp_bladder.2>-log10(0.05))|
                                      logp_lung.2>=30,
                                    mylabels,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_point(color = ifelse((pval$logp_lung.2>=bonf|pval$logp_bladder.2>=bonf),
                            pval$mycolour, "grey90")) +
  geom_label_repel(size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.02, nudge_x = 0.02, point.padding = 0.1,
                   label.size = NA, label.padding=.05, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  labs(title = models[2]) +
  xlab(expression(-log[10](italic(p))~"(Lung cancer vs control)"))+
  ylab(expression(-log[10](italic(p))~"(Bladder cancer vs control)")) +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw()

pdf("../Figures/Final/Presentation/univ_adj_or.pdf", width = 5, height = 5)
p3
dev.off()
pdf("../Figures/Final/Presentation/univ_adj_pval.pdf", width = 5, height = 5)
p4
dev.off()

### Main (base) ----
xlim=c(min(or[,c(1:3,7:9)],na.rm=T),max(or[,c(1:3)],na.rm=T))
ylim=c(min(or[,c(1:3,7:9)],na.rm=T),max(or[,c(7:9)],na.rm=T))

label_asterisk = 1:nrow(or)
label_asterisk[38]="38*"

p5 = ggplot(or, aes(x=or_lung.1, y=or_bladder.1,
                         label=ifelse((pval$logp_lung.1>=bonf|pval$logp_bladder.1>=bonf),
                                      label, ""))) + 
  geom_abline(linetype = 'dotted', colour = 'grey') +
  geom_errorbar(aes(ymin=l95_bladder.1, ymax=u95_bladder.1),colour=ifelse((pval$logp_lung.1>=bonf|pval$logp_bladder.1>=bonf),or$mycolour,alpha("grey90",0.01)), width=.01*xlim[2]) +
  geom_errorbarh(aes(xmin=l95_lung.1, xmax=u95_lung.1),colour=ifelse((pval$logp_lung.1>=bonf|pval$logp_bladder.1>=bonf),or$mycolour,alpha("grey90",0.01)), height=.01*ylim[2]) +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_label_repel(color=pval$mycolour_lab, size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = 30,
                   label.size = NA, label.padding=0.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  geom_point(colour=ifelse((pval$logp_lung.1>=bonf|pval$logp_bladder.1>=bonf),or$mycolour,alpha("grey90",0.01))) +
  xlim(xlim) +
  ylim(ylim) +
  ylab('OR [95% CI] (Bladder cancer vs control)') +
  xlab('OR [95% CI] (Lung cancer vs control)') +
  ggtitle(models[1]) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(0,max(pval[,1],na.rm=T))
ylim=c(0,max(pval[,3],na.rm=T))

main = ggplot(pval,aes(logp_lung.1, logp_bladder.1,
                            label = ifelse((logp_lung.1>=bonf|logp_bladder.1>=bonf),label,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_point(color = ifelse((pval$logp_lung.1>=bonf|pval$logp_bladder.1>=bonf),
                            pval$mycolour, "grey90")) +
  geom_label_repel(color=pval$mycolour_lab, size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.02, nudge_x = 0.02, 
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  labs(title = models[1])+
  xlab(expression(-log[10](italic(p))~"(Lung cancer vs control)"))+
  ylab(expression(-log[10](italic(p))~"(Bladder cancer vs control)")) +
  xlim(0,60) +
  ylim(0,bonf) +
  theme_bw()

sub = ggplot(pval,aes(logp_lung.1, logp_bladder.1,
                           label = ifelse((logp_lung.1>=bonf&logp_bladder.1>=bonf)|
                                             logp_lung.1>=50,
                                          label_asterisk,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_point(color = ifelse((pval$logp_lung.1>=bonf&pval$logp_bladder.1>=bonf)|pval$logp_lung.1>=50,
                            pval$mycolour,
                            ifelse(pval$logp_lung.1>=bonf|pval$logp_bladder.1>=bonf,
                                   alpha(pval$mycolour,0.1),alpha("grey90",0.1))),
             size = ifelse((pval$logp_lung.1>=bonf&pval$logp_bladder.1>=bonf)|pval$logp_lung.1>=50,
                           1.5, 1)) +
  geom_label_repel(color=pval$mycolour_lab, size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlim(xlim) +
  ylim(ylim) +
  # geom_rect(xmin=-1, ymin=-0.5, xmax=45, ymax=bonf, size = 0.1, color="black", fill = alpha("grey90",0.1)) +
  labs(title = "Full view") +
  theme_bw() +
  theme(text = element_text(size=7.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p6 = main + annotation_custom(ggplotGrob(sub), xmin=35, xmax=60*1.05, ymin=1.6, ymax=bonf*1.05)

pdf("../Figures/Final/Main/univ_base_or.pdf", width = 5, height = 5)
p5
dev.off()
pdf("../Figures/Final/Main/univ_base_pval.pdf", width = 5, height = 5)
p6
dev.off()

### Main (adjusted) ----
xlim=c(min(or[,c(4:6,10:12)],na.rm=T),max(or[,c(4:6)],na.rm=T))
ylim=c(min(or[,c(4:6,10:12)],na.rm=T),max(or[,c(10:12)],na.rm=T))

p7 = ggplot(or, aes(x=or_lung.2, y=or_bladder.2,
                    label=ifelse((pval$logp_lung.2>=bonf|pval$logp_bladder.2>=bonf),
                                 label, ""))) + 
  geom_abline(linetype = 'dotted', colour = 'grey') +
  geom_errorbar(aes(ymin=l95_bladder.2, ymax=u95_bladder.2),colour=ifelse((pval$logp_lung.2>=bonf|pval$logp_bladder.2>=bonf),or$mycolour,alpha("grey90",0.01)), width=.01*xlim[2]) +
  geom_errorbarh(aes(xmin=l95_lung.2, xmax=u95_lung.2),colour=ifelse((pval$logp_lung.2>=bonf|pval$logp_bladder.2>=bonf),or$mycolour,alpha("grey90",0.01)), height=.01*ylim[2]) +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_label_repel(color=pval$mycolour_lab, size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = 20,
                   label.size = NA, label.padding=0.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  geom_point(colour=ifelse((pval$logp_lung.2>=bonf|pval$logp_bladder.2>=bonf),or$mycolour,alpha("grey90",0.01))) +
  xlim(xlim) +
  ylim(ylim) +
  ylab('OR [95% CI] (Bladder cancer vs control)') +
  xlab('OR [95% CI] (Lung cancer vs control)') +
  ggtitle(models[2]) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(0,max(pval[,2],na.rm=T))
ylim=c(0,max(pval[,4],na.rm=T))

p8 = ggplot(pval,aes(logp_lung.2, logp_bladder.2,
                       label = ifelse((logp_lung.2>=bonf|logp_bladder.2>=bonf),label,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_point(color = ifelse((pval$logp_lung.2>=bonf|pval$logp_bladder.2>=bonf),
                            pval$mycolour, "grey90")) +
  geom_label_repel(color=pval$mycolour_lab, size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.02, nudge_x = 0.02, 
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  labs(title = models[2])+
  xlab(expression(-log[10](italic(p))~"(Lung cancer vs control)"))+
  ylab(expression(-log[10](italic(p))~"(Bladder cancer vs control)")) +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw()

pdf("../Figures/Final/Main/univ_adj_or.pdf", width = 5, height = 5)
p7
dev.off()
pdf("../Figures/Final/Main/univ_adj_pval.pdf", width = 5, height = 5)
p8
dev.off()
