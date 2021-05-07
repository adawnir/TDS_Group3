### TDS Project --  Stratified analysis by sex: Univariate analysis Visualisation (Forest plot and Manhattan plot)
## Programme created by Ines on 11 March reviewed by Rin Wada on 15 March

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

# Loading packages
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(cowplot)
library(ggrepel)

### Load data set ----
# plot labels
setwd("../Dictionaries")
manhat_annot=read_csv("manhat_annot.csv")
plot_annot=read_csv("plot_annot.csv")[-c(1,2,21:25),]

forest=as.data.frame(readRDS("../Results/strat_sex_univ/forest_plot.rds"))

# Reorder variables
mynames=rownames(forest)
myorder=c(mynames[5:38],mynames[1:4],mynames[39:length(mynames)])
forest=forest %>%
  slice(match(myorder,rownames(forest)))

# Labels for significant
mylabels=plot_annot$label.point
bonf=-log10(0.05/72)
models=c("Males, Base model", "Males, Model adjusted on smoking status")
mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")


pval=forest[,which(grepl("logp_",colnames(forest)))] # Extract pval
or=forest[,-which(grepl("logp_",colnames(forest)))] # Extract or and ci

# Extract male
pval_male=pval[,which(grepl(".m.", colnames(pval)))]
pval_male$label=1:nrow(pval_male)
pval_male$mycolour=rep(mycolours,times=c(18,20,8,16,28))
pval_male$mycolour_lab=darken(pval_male$mycolour, amount=0.5)

or_male=or[,which(grepl(".m.", colnames(or)))]
or_male$label=1:nrow(or_male)
or_male$mycolour=rep(mycolours,times=c(18,20,8,16,28))
or_male$mycolour_lab=darken(or_male$mycolour, amount=0.5)

### Presentation (Male, Base model: zoomed out)  ----
xlim=c(min(or_male[,c(1:3,7:9)],na.rm=T),max(or_male[,c(1:3)],na.rm=T))
ylim=c(min(or_male[,c(1:3,7:9)],na.rm=T),max(or_male[,c(7:9)],na.rm=T))

p1 <- ggplot(or_male, aes(x=or_lung.m.1, y=or_bladder.m.1,
                          label=ifelse((pval_male$logp_lung.m.1>=bonf&pval_male$logp_bladder.m.1>=bonf)|
                                         pval_male$logp_bladder.m.1>=bonf, mylabels,""))) + 
  geom_abline(linetype = 'dotted', colour = 'grey') +
  geom_errorbar(aes(ymin=l95_bladder.m.1, ymax=u95_bladder.m.1),colour=ifelse((pval_male$logp_lung.m.1>=bonf|pval_male$logp_bladder.m.1>=bonf),or_male$mycolour,alpha("grey90",0.01)), width=.01*xlim[2]) +
  geom_errorbarh(aes(xmin=l95_lung.m.1, xmax=u95_lung.m.1),colour=ifelse((pval_male$logp_lung.m.1>=bonf|pval_male$logp_bladder.m.1>=bonf),or_male$mycolour,alpha("grey90",0.01)), height=.01*ylim[2]) +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_label_repel(size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.02, nudge_x = 0.02,
                   box.padding = 1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  geom_point(colour=ifelse((pval_male$logp_lung.m.1>=bonf|pval_male$logp_bladder.m.1>=bonf),or_male$mycolour,alpha("grey90",0.01))) +
  xlim(xlim) +
  ylim(ylim) +
  ylab('OR [95% CI] (Bladder cancer vs control)') +
  xlab('OR [95% CI] (Lung cancer vs control)') +
  ggtitle(models[1]) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(0,max(pval_male[,1],na.rm=T))
ylim=c(0,max(pval_male[,3],na.rm=T))

p2 = ggplot(pval_male,aes(logp_lung.m.1, logp_bladder.m.1,
                     label = ifelse((logp_lung.m.1>=bonf&logp_bladder.m.1>=bonf)|
                                      logp_bladder.m.1>=bonf,
                                    mylabels,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_point(color = ifelse((pval_male$logp_lung.m.1>=bonf|pval_male$logp_bladder.m.1>=bonf),
                            pval_male$mycolour, "grey90")) +
  geom_label_repel(size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.02, nudge_x = 0.02,
                   box.padding = 1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  labs(title = models[1]) +
  xlab(expression(-log[10](italic(p))~"(Lung cancer vs control)"))+
  ylab(expression(-log[10](italic(p))~"(Bladder cancer vs control)")) +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw()

pdf("../Figures/Final/Presentation/strat_sex_univ_male_base_or.pdf", width = 5, height = 5)
p1
dev.off()
pdf("../Figures/Final/Presentation/strat_sex_univ_male_base_pval.pdf", width = 5, height = 5)
p2
dev.off()

### Main (Male, Base model: OR and pval_male)----
xlim=c(min(or_male[,c(1:3,7:9)],na.rm=T),max(or_male[,c(1:3)],na.rm=T))
ylim=c(min(or_male[,c(1:3,7:9)],na.rm=T),max(or_male[,c(7:9)],na.rm=T))

p3 = ggplot(or_male, aes(x=or_lung.m.1, y=or_bladder.m.1,
                          label=ifelse((pval_male$logp_lung.m.1>=bonf|pval_male$logp_bladder.m.1>=bonf),
                                       label, ""))) + 
  geom_abline(linetype = 'dotted', colour = 'grey') +
  geom_errorbar(aes(ymin=l95_bladder.m.1, ymax=u95_bladder.m.1),colour=ifelse((pval_male$logp_lung.m.1>=bonf|pval_male$logp_bladder.m.1>=bonf),or_male$mycolour,alpha("grey90",0.01)), width=.01*xlim[2]) +
  geom_errorbarh(aes(xmin=l95_lung.m.1, xmax=u95_lung.m.1),colour=ifelse((pval_male$logp_lung.m.1>=bonf|pval_male$logp_bladder.m.1>=bonf),or_male$mycolour,alpha("grey90",0.01)), height=.01*ylim[2]) +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_label_repel(color=pval_male$mycolour_lab, size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = 20,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  geom_point(colour=ifelse((pval_male$logp_lung.m.1>=bonf|pval_male$logp_bladder.m.1>=bonf),or_male$mycolour,alpha("grey90",0.01))) +
  xlim(xlim) +
  ylim(ylim) +
  ylab('OR [95% CI] (Bladder cancer vs control)') +
  xlab('OR [95% CI] (Lung cancer vs control)') +
  ggtitle(models[1]) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(0,max(pval_male[,1],na.rm=T))
ylim=c(0,max(pval_male[,3],na.rm=T))

main = ggplot(pval_male,aes(logp_lung.m.1, logp_bladder.m.1,
                            label = ifelse((logp_lung.m.1>=bonf|logp_bladder.m.1>=bonf),label,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_point(color = ifelse((pval_male$logp_lung.m.1>=bonf|pval_male$logp_bladder.m.1>=bonf),
                            pval_male$mycolour, "grey90")) +
  geom_label_repel(color=pval_male$mycolour_lab, size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  labs(title = models[1])+
  xlab(expression(-log[10](italic(p))~"(Lung cancer vs control)"))+
  ylab(expression(-log[10](italic(p))~"(Bladder cancer vs control)")) +
  xlim(0,55) +
  ylim(0,4) +
  theme_bw()

sub = ggplot(pval_male,aes(logp_lung.m.1, logp_bladder.m.1,
                           label = ifelse((logp_lung.m.1>=bonf&logp_bladder.m.1>=bonf),
                                          label,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_point(color = ifelse((pval_male$logp_lung.m.1>=bonf&pval_male$logp_bladder.m.1>=bonf),
                            pval_male$mycolour, ifelse((pval_male$logp_lung.m.1>=bonf|pval_male$logp_bladder.m.1>=bonf),alpha(pval_male$mycolour, 0.1),alpha("grey90",0.1))),
             size = ifelse((pval_male$logp_lung.m.1>=bonf&pval_male$logp_bladder.m.1>=bonf),1.5,1)) +
  geom_label_repel(color=pval_male$mycolour_lab, size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  # geom_rect(xmin=-1, ymin=-0.5, xmax=60, ymax=4, color="black", fill = NA) +
  labs(title = "Full view") +
  theme(text = element_text(size = 7.5),axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p4 = main + annotation_custom(ggplotGrob(sub), xmin=25, xmax=55*1.05, ymin=1.5, ymax=4*1.05)


pdf("../Figures/Final/Main/strat_sex_univ_male_base_or.pdf", width = 5, height = 5)
p3
dev.off()
pdf("../Figures/Final/Main/strat_sex_univ_male_base_pval.pdf", width = 5, height = 5)
p4
dev.off()


### Supplementary ----
## Male adjusted
xlim=c(min(or_male[,c(4:6,10:12)],na.rm=T),max(or_male[,c(4:6)],na.rm=T))
ylim=c(min(or_male[,c(4:6,10:12)],na.rm=T),max(or_male[,c(10:12)],na.rm=T))

p5 = ggplot(or_male, aes(x=or_lung.m.2, y=or_bladder.m.2,
                         label=ifelse((pval_male$logp_lung.m.2>=bonf|pval_male$logp_bladder.m.2>=bonf),
                                      label, ""))) + 
  geom_abline(linetype = 'dotted', colour = 'grey') +
  geom_errorbar(aes(ymin=l95_bladder.m.2, ymax=u95_bladder.m.2),colour=or_male$mycolour, width=.01*xlim[2]) +
  geom_errorbarh(aes(xmin=l95_lung.m.2, xmax=u95_lung.m.2),colour=or_male$mycolour, height=.01*ylim[2]) +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_label_repel(color=pval_male$mycolour_lab, size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = 20,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  geom_point(colour=or_male$mycolour) +
  xlim(xlim) +
  ylim(ylim) +
  ylab('OR [95% CI] (Bladder cancer vs control)') +
  xlab('OR [95% CI] (Lung cancer vs control)') +
  ggtitle(models[2]) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(0,max(pval_male[,2],na.rm=T))
ylim=c(0,bonf)

p6 = ggplot(pval_male,aes(logp_lung.m.2, logp_bladder.m.2,
                            label = ifelse((logp_lung.m.2>=bonf|logp_bladder.m.2>=bonf),label,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_point(color = ifelse((pval_male$logp_lung.m.2>=bonf|pval_male$logp_bladder.m.2>=bonf),
                            pval_male$mycolour, "grey90")) +
  geom_label_repel(color=pval_male$mycolour_lab, size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  labs(title = models[2])+
  xlab(expression(-log[10](italic(p))~"(Lung cancer vs control)"))+
  ylab(expression(-log[10](italic(p))~"(Bladder cancer vs control)")) +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw()

pdf("../Figures/Final/Supplementary/strat_sex_univ_male_adj_or.pdf", width = 5, height = 5)
p5
dev.off()
pdf("../Figures/Final/Supplementary/strat_sex_univ_male_adj_pval.pdf", width = 5, height = 5)
p6
dev.off()

### Load data set ----
# plot labels
setwd("../Dictionaries")
manhat_annot=read_csv("manhat_annot.csv")
plot_annot=read_csv("plot_annot.csv")[-c(1,2,21:25),]

forest=as.data.frame(readRDS("../Results/strat_sex_univ/forest_plot.rds"))

# Reorder variables
mynames=rownames(forest)
myorder=c(mynames[5:38],mynames[1:4],mynames[39:length(mynames)])
forest=forest %>%
  slice(match(myorder,rownames(forest)))

# Labels for significant
mylabels=plot_annot$label.point
bonf=-log10(0.05/72)
models=c("Females, Base model", "Females, Model adjusted on smoking status")
mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")


pval=forest[,which(grepl("logp_",colnames(forest)))] # Extract pval
or=forest[,-which(grepl("logp_",colnames(forest)))] # Extract or and ci

# Extract male
pval_female=pval[,which(grepl(".f.", colnames(pval)))]
pval_female$label=1:nrow(pval_female)
pval_female$mycolour=rep(mycolours,times=c(18,20,8,16,28))
pval_female$mycolour_lab=darken(pval_female$mycolour, amount=0.5)

or_female=or[,which(grepl(".f.", colnames(or)))]
or_female$label=1:nrow(or_female)
or_female$mycolour=rep(mycolours,times=c(18,20,8,16,28))
or_female$mycolour_lab=darken(or_female$mycolour, amount=0.5)

### Supplementary (female, Base model: OR and pval_female)----
xlim=c(min(or_female[,c(1:3,7:9)],na.rm=T),max(or_female[,c(1:3)],na.rm=T))
ylim=c(min(or_female[,c(1:3,7:9)],na.rm=T),max(or_female[,c(7:9)],na.rm=T))

p1 = ggplot(or_female, aes(x=or_lung.f.1, y=or_bladder.f.1,
                         label=ifelse((pval_female$logp_lung.f.1>=bonf|pval_female$logp_bladder.f.1>=bonf),
                                      label, ""))) + 
  geom_abline(linetype = 'dotted', colour = 'grey') +
  geom_errorbar(aes(ymin=l95_bladder.f.1, ymax=u95_bladder.f.1),colour=ifelse((pval_female$logp_lung.f.1>=bonf|pval_female$logp_bladder.f.1>=bonf),or_female$mycolour,alpha("grey90",0.01)), width=.01*xlim[2]) +
  geom_errorbarh(aes(xmin=l95_lung.f.1, xmax=u95_lung.f.1),colour=ifelse((pval_female$logp_lung.f.1>=bonf|pval_female$logp_bladder.f.1>=bonf),or_female$mycolour,alpha("grey90",0.01)), height=.01*ylim[2]) +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_label_repel(color=pval_female$mycolour_lab, size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = 20,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  geom_point(colour=ifelse((pval_female$logp_lung.f.1>=bonf|pval_female$logp_bladder.f.1>=bonf),or_female$mycolour,alpha("grey90",0.01))) +
  xlim(xlim) +
  ylim(ylim) +
  ylab('OR [95% CI] (Bladder cancer vs control)') +
  xlab('OR [95% CI] (Lung cancer vs control)') +
  ggtitle(models[1]) +
  theme_bw() +
  theme(legend.position = "none")

xlim=c(0,max(pval_female[,1],na.rm=T))
ylim=c(0,max(pval_female[,3],na.rm=T))

p2 = ggplot(pval_female,aes(logp_lung.f.1, logp_bladder.f.1,
                     label = ifelse((logp_lung.f.1>=bonf|logp_bladder.f.1>=bonf),label,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_point(color = ifelse((pval_female$logp_lung.f.1>=bonf|pval_female$logp_bladder.f.1>=bonf),
                            pval_female$mycolour, "grey90")) +
  geom_label_repel(color=pval_female$mycolour_lab, size=3.5, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  labs(title = models[1])+
  xlab(expression(-log[10](italic(p))~"(Lung cancer vs control)"))+
  ylab(expression(-log[10](italic(p))~"(Bladder cancer vs control)")) +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw()

pdf("../Figures/Final/Supplementary/strat_sex_univ_female_base_or.pdf", width = 5, height = 5)
p1
dev.off()
pdf("../Figures/Final/Supplementary/strat_sex_univ_female_base_pval.pdf", width = 5, height = 5)
p2
dev.off()

# ### Female and Male: MANHATTAN ----
# manhat=as.data.frame(readRDS("../Results/strat_sex_univ/manhattan_plot.rds"))
# bonf=-log10(0.05/nrow(manhat))
# # manhat=manhat[-1,] # Remove smoking
# 
# # settings
# mylabels=manhat_annot$label
# variable_cat=c(rep(NA,1), # smoking in first column
#                rep("Sociodemographic",8),
#                rep("Health risk", 12),
#                rep("Environmental", 8),
#                rep("Medical", 15), 
#                rep("Biomarkers", 28))
# models=c("Base model", "Model adjusted on smoking status")
# mycolours=c("navy","red","darkmagenta")
# xseq=seq(1,nrow(manhat))
# 
# 
# # Lung female
# ifelse(dir.exists("../Figures/strat_sex_univ"),"",dir.create("../Figures/strat_sex_univ"))
# {pdf("../Figures/strat_sex_univ/manhattan_lung_female.pdf", height = 5, width = 10)
# par(mar = c(10, 3, 3, 5))
# plot(manhat[,1], pch = 17,col=mycolours[1], cex=0.7,
#      xaxt = "n", yaxt="n", ylab="", xlab="",
#      ylim = c(min(manhat[,1:2],na.rm=T), max(manhat[,1:2],na.rm=T)+1))
# axis(side=4, at=axTicks(2), cex.axis=0.7)
# abline(h=bonf, lty = 2, col="darkred")
# abline(h=-log10(0.05), lty = 2, col="grey")
# abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
# abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
# mtext(side=4, text=expression(-log[10](italic(p))), line=2, cex=0.7)
# points(manhat[,1], pch = 17, col=mycolours[1], cex=0.7)
# points(manhat[,2], pch = 19, col=mycolours[2], cex=0.7)
# for (k in 1:length(xseq)){
#   mytext=mylabels[k]
#   if (grepl("m\\^", mytext)){
#     mytext=gsub("m\\^","'~m^", mytext)
#     mytext=sub(")","~')", mytext)
#   }
#   if (grepl("\\[", mytext)){
#     mytext=gsub("\\[","'[", mytext)
#     mytext=sub("\\(ug","~'(ug", mytext)
#   }
#   mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
#   labcol=ifelse((manhat[k,1]>=bonf & manhat[k,2]>=bonf), mycolours[3],
#                 ifelse(manhat[k,1]>=bonf,mycolours[1],
#                        ifelse(manhat[k,2]>=bonf, mycolours[2],"black")))
#   if(is.na(labcol)){
#     labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
#                   ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
#   }
#   labfont=ifelse(labcol=="black",1,2)
#   axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7,
#        col.axis=labcol, col = labcol, font=labfont)
# }
# xseqblack=c(xseq[!duplicated(variable_cat)]-1/2, max(xseq)+1/2)
# abline(v=xseqblack,lty=3,col="black")
# for (k in 2:(length(xseqblack)-1)){
#   axis(side=3, at=xseqblack[c(k,k+1)]+c(0.5,-0.5), line=0.5, labels=NA)
# }
# for (k in 2:(length(xseqblack)-1)){
#   axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#        labels=unique(variable_cat)[k], cex.axis=0.9)
# }
# legend("topright", pch=c(17, 19, NA, NA, NA), lty=c(NA, NA, 2, 2,NA),
#        col=c(mycolours[1:2],"darkred","grey"),
#        legend=c(models,"Bonferroni threshold","Nominal threshold"),
#        cex=0.7, bg = "white")
# dev.off()
# }
# 
# # Lung male
# {pdf("../Figures/strat_sex_univ/manhattan_lung_male.pdf", height = 5, width = 10)
# par(mar = c(10, 3, 3, 5))
# plot(manhat[,3], pch = 17,col=mycolours[1], cex=0.7,
#      xaxt = "n", yaxt="n", ylab="", xlab="",
#      ylim = c(min(manhat[3:4], na.rm = T), max(manhat[3:4], na.rm = T)+1))
# axis(side=4, at=axTicks(2), cex.axis=0.7)
# abline(h=bonf, lty = 2, col="darkred")
# abline(h=-log10(0.05), lty = 4, col="grey")
# abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
# abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
# mtext(side=4, text=expression(-log[10](italic(p))), line=2, cex=0.7)
# points(manhat[,3], pch = 17, col=mycolours[1], cex=0.7)
# points(manhat[,4], pch = 19, col=mycolours[2], cex=0.7)
# for (k in 1:length(xseq)){
#   mytext=mylabels[k]
#   if (grepl("m\\^", mytext)){
#     mytext=gsub("m\\^","'~m^", mytext)
#     mytext=sub(")","~')", mytext)
#   }
#   if (grepl("\\[", mytext)){
#     mytext=gsub("\\[","'[", mytext)
#     mytext=sub("\\(ug","~'(ug", mytext)
#   }
#   mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
#   labcol=ifelse((manhat[k,3]>=bonf & manhat[k,4]>=bonf), mycolours[3],
#                 ifelse(manhat[k,3]>=bonf,mycolours[1],
#                        ifelse(manhat[k,4]>=bonf, mycolours[2],"black")))
#   if(is.na(labcol)){
#     labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
#                   ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
#   }
#   labfont=ifelse(labcol=="black",1,2)
#   axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7,
#        col.axis=labcol, col = labcol, font=labfont)
# }
# xseqblack=c(xseq[!duplicated(variable_cat)]-1/2, max(xseq)+1/2)
# abline(v=xseqblack,lty=3,col="black")
# for (k in 2:(length(xseqblack)-1)){
#   axis(side=3, at=xseqblack[c(k,k+1)]+c(0.5,-0.5), line=0.5, labels=NA)
# }
# for (k in 2:(length(xseqblack)-1)){
#   axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#        labels=unique(variable_cat)[k], cex.axis=0.9)
# }
# legend("topright", pch=c(17, 19, NA, NA), lty=c(NA, NA, 2, 2),
#        col=c(mycolours[1:2],"darkred","grey"), legend=c(models,"Bonferroni threshold","Nominal threshold"),
#        cex=0.7, bg = "white")
# dev.off()
# }
# 
# 
# # Bladder female
# {pdf("../Figures/strat_sex_univ/manhattan_bladder_female.pdf", height = 5, width = 10)
#   par(mar = c(10, 3, 3, 5))
#   plot(manhat[,5], pch = 17,col=mycolours[1], cex=0.7,
#        xaxt = "n", yaxt="n", ylab="", xlab="",
#        ylim = c(min(manhat[,5:6],na.rm=T), max(manhat[,5:6],na.rm=T)+1))
#   axis(side=4, at=axTicks(2), cex.axis=0.7)
#   abline(h=bonf, lty = 2, col="darkred")
#   abline(h=-log10(0.05), lty = 2, col="grey")
#   abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
#   abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
#   mtext(side=4, text=expression(-log[10](italic(p))), line=2, cex=0.7)
#   points(manhat[,5], pch = 17, col=mycolours[1], cex=0.7)
#   points(manhat[,6], pch = 19, col=mycolours[2], cex=0.7)
#   for (k in 1:length(xseq)){
#     mytext=mylabels[k]
#     if (grepl("m\\^", mytext)){
#       mytext=gsub("m\\^","'~m^", mytext)
#       mytext=sub(")","~')", mytext)
#     }
#     if (grepl("\\[", mytext)){
#       mytext=gsub("\\[","'[", mytext)
#       mytext=sub("\\(ug","~'(ug", mytext)
#     }
#     mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
#     labcol=ifelse((manhat[k,5]>=bonf & manhat[k,6]>=bonf), mycolours[3],
#                   ifelse(manhat[k,5]>=bonf,mycolours[1],
#                          ifelse(manhat[k,6]>=bonf, mycolours[2],"black")))
#     if(is.na(labcol)){
#       labcol=ifelse(manhat[k,5]>=bonf,mycolours[1],
#                     ifelse(manhat[k,6]>=bonf, mycolours[2],"black"))
#     }
#     labfont=ifelse(labcol=="black",1,2)
#     axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7,
#          col.axis=labcol, col = labcol, font=labfont)
#   }
#   xseqblack=c(xseq[!duplicated(variable_cat)]-1/2, max(xseq)+1/2)
#   abline(v=xseqblack,lty=3,col="black")
#   for (k in 2:(length(xseqblack)-1)){
#     axis(side=3, at=xseqblack[c(k,k+1)]+c(0.5,-0.5), line=0.5, labels=NA)
#   }
#   for (k in 2:(length(xseqblack)-1)){
#     axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#          labels=unique(variable_cat)[k], cex.axis=0.9)
#   }
#   legend("topright", pch=c(17, 19, NA, NA, NA), lty=c(NA, NA, 2, 2,NA),
#          col=c(mycolours[1:2],"darkred","grey"),
#          legend=c(models,"Bonferroni threshold","Nominal threshold"),
#          cex=0.7, bg = "white")
#   dev.off()
# }
# 
# # Bladder male
# {pdf("../Figures/strat_sex_univ/manhattan_bladder_male.pdf", height = 5, width = 10)
#   par(mar = c(10, 3, 3, 5))
#   plot(manhat[,7], pch = 17,col=mycolours[1], cex=0.7,
#        xaxt = "n", yaxt="n", ylab="", xlab="",
#        ylim = c(min(manhat[,7:8],na.rm=T), max(manhat[,7:8],na.rm=T)+1))
#   axis(side=4, at=axTicks(2), cex.axis=0.7)
#   abline(h=bonf, lty = 2, col="darkred")
#   abline(h=-log10(0.05), lty = 2, col="grey")
#   abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
#   abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
#   mtext(side=4, text=expression(-log[10](italic(p))), line=2, cex=0.7)
#   points(manhat[,7], pch = 17, col=mycolours[1], cex=0.7)
#   points(manhat[,8], pch = 19, col=mycolours[2], cex=0.7)
#   for (k in 1:length(xseq)){
#     mytext=mylabels[k]
#     if (grepl("m\\^", mytext)){
#       mytext=gsub("m\\^","'~m^", mytext)
#       mytext=sub(")","~')", mytext)
#     }
#     if (grepl("\\[", mytext)){
#       mytext=gsub("\\[","'[", mytext)
#       mytext=sub("\\(ug","~'(ug", mytext)
#     }
#     mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
#     labcol=ifelse((manhat[k,7]>=bonf & manhat[k,8]>=bonf), mycolours[3],
#                   ifelse(manhat[k,7]>=bonf,mycolours[1],
#                          ifelse(manhat[k,8]>=bonf, mycolours[2],"black")))
#     if(is.na(labcol)){
#       labcol=ifelse(manhat[k,7]>=bonf,mycolours[1],
#                     ifelse(manhat[k,8]>=bonf, mycolours[2],"black"))
#     }
#     labfont=ifelse(labcol=="black",1,2)
#     axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7,
#          col.axis=labcol, col = labcol, font=labfont)
#   }
#   xseqblack=c(xseq[!duplicated(variable_cat)]-1/2, max(xseq)+1/2)
#   abline(v=xseqblack,lty=3,col="black")
#   for (k in 2:(length(xseqblack)-1)){
#     axis(side=3, at=xseqblack[c(k,k+1)]+c(0.5,-0.5), line=0.5, labels=NA)
#   }
#   for (k in 2:(length(xseqblack)-1)){
#     axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#          labels=unique(variable_cat)[k], cex.axis=0.9)
#   }
#   legend("topright", pch=c(17, 19, NA, NA, NA), lty=c(NA, NA, 2, 2,NA),
#          col=c(mycolours[1:2],"darkred","grey"),
#          legend=c(models,"Bonferroni threshold","Nominal threshold"),
#          cex=0.7, bg = "white")
#   dev.off()
# }

# ### FOREST PLOTS ----
# forest=as.data.frame(readRDS("../Results/strat_sex_univ/forest_plot.rds"))
# forest=forest[,-which(grepl("logp_",colnames(forest)))] # Remove pval_males
# # Reorder
# myorder=plot_annot$col.name
# forest=forest %>%
#   slice(match(myorder,rownames(forest)))
# 
# mylabels=plot_annot$label
# myref=plot_annot$ref
# variable_cat=c(rep("Sociodemographic",18),
#                rep("Health risk", 28),
#                rep("Environmental", 8),
#                rep("Medical", 16), 
#                rep("Biomarkers", 28))
# models=c("Female: Base model", "Female: Model adjusted on smoking status",
#          "Male: Base model", "Male: Model adjusted on smoking status")
# mycolours=c("lightcoral", darken("lightcoral",amount=0.5),"steelblue",darken("steelblue", amount=0.5))
# 
# mypch=18
# mycex=0.45
# background=TRUE
# myrange=c(min(forest[1:12], na.rm=T),max(forest[1:12],na.rm=T),min(forest[13:24], na.rm=T),max(forest[12:24],na.rm=T))
# 
# myspacing=6
# xseq=rep(myspacing, nrow(forest))
# xseq=cumsum(xseq)
# 
# # vertical two panels
# {pdf("../Figures/strat_sex_univ/forest_plot.pdf", height = 12, width = 10)
#   par(oma=c(4, 3, 2, 17), mar=c(0, 0.5, 0.5, 0), mfrow=c(1,2))
#   plot(x=forest[,1], y=xseq-1, 
#        ylim=rev(c(min(xseq), max(xseq))),xlim=myrange[1:2],
#        xaxt="n", yaxt="n",
#        pch=mypch, col=mycolours[1], lwd=1, cex=mycex+0.2,
#        ylab="",xlab="",log="x")
#   xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
#   if (background){
#     for (k in seq(1,length(xseqgreysep),by=2)){
#       polygon(y=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
#               x=exp(c(-30,-30,30,30)), col="grey95", border=NA)
#     }
#     for (k in seq(2,length(xseqgreysep),by=2)){
#       polygon(y=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
#               x=exp(c(-30,-30,30,30)), col="grey98", border=NA)
#     }
#     box()
#   }
#   plotCI(y=xseq-1.5, x=forest[,1], li=forest[,2], ui=forest[,3], err = "x",
#          xlim=myrange[1:2], yaxt="n", xaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex+0.2, sfrac=0.001,
#          ylab="",xlab="", add=TRUE)
#   plotCI(y=xseq-0.5, x=forest[,4], li=forest[,5], ui=forest[,6], err = "x",
#          xlim=myrange[1:2], yaxt="n", xaxt="n", pch=19, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE, slty=2)
#   plotCI(y=xseq+0.5, x=forest[,7], li=forest[,8], ui=forest[,9], err = "x",
#          xlim=myrange[1:2], yaxt="n", xaxt="n", pch=15, col=mycolours[3], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE, slty=3)
#   plotCI(y=xseq+1.5, x=forest[,10], li=forest[,11], ui=forest[,12], err = "x",
#          xlim=myrange[1:2], yaxt="n", xaxt="n", pch=4, col=mycolours[4], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE, slty=4)
#   axis(side=1, at=axTicks(1), cex.axis=0.7)
#   abline(v=1, lty=2)
#   abline(h=xseqgreysep,lty=3, col="grey")
#   xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#   abline(h=xseqblack,lty=3,col="black")
#   for (k in 1:(length(xseqblack)-1)){
#     axis(side=2, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#   }
#   for (k in 1:(length(xseqblack)-1)){
#     axis(side=2, at=mean(xseqblack[c(k,k+1)]), las = 3, line=0.2, tick=FALSE,
#          labels=unique(variable_cat)[k])
#   }
#   mtext(side=3, text="Lung cancer", line=0.2, cex.lab=0.7)
#   legend("bottomright", pch = c(mypch, 19,15,4), col = mycolours,
#          legend=models, cex=0.7, bg="white")
#   # Bladder
#   plot(x=forest[,7], y=xseq-1, 
#        ylim=rev(c(min(xseq), max(xseq))),xlim=myrange[3:4],
#        xaxt="n", yaxt="n",
#        pch=mypch, col=mycolours[1], lwd=1, cex=mycex,
#        ylab="",xlab="",log="x")
#   xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
#   if (background){
#     for (k in seq(1,length(xseqgreysep),by=2)){
#       polygon(x=exp(c(-30,-30,30,30)),
#               y=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
#               col="grey95", border=NA)
#     }
#     for (k in seq(2,length(xseqgreysep),by=2)){
#       polygon(x=exp(c(-30,-30,30,30)),
#               y=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
#               col="grey98", border=NA)
#     }
#     box()
#   }
#   plotCI(y=xseq-1.5, x=forest[,13], li=forest[,14], ui=forest[,15], err = "x",
#          xlim=myrange[3:4], yaxt="n", xaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex+0.2, sfrac=0.001,
#          ylab="",xlab="", add=TRUE)
#   plotCI(y=xseq-0.5, x=forest[,16], li=forest[,17], ui=forest[,18], err = "x",
#          xlim=myrange[3:4], yaxt="n", xaxt="n", pch=19, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE, slty=2)
#   plotCI(y=xseq+0.5, x=forest[,19], li=forest[,20], ui=forest[,21], err = "x",
#          xlim=myrange[3:4], yaxt="n", xaxt="n", pch=15, col=mycolours[3], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE, slty=3)
#   plotCI(y=xseq+1.5, x=forest[,22], li=forest[,23], ui=forest[,24], err = "x",
#          xlim=myrange[3:4], yaxt="n", xaxt="n", pch=4, col=mycolours[4], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE, slty=4)
#   for (k in 1:length(xseq)){
#     mytext=mylabels[k]
#     if (grepl("m\\^", mytext)){
#       mytext=gsub("m\\^","'~m^", mytext)
#       mytext=sub(")","~')", mytext)
#     }
#     if (grepl("\\[", mytext)){
#       mytext=gsub("\\[","'[", mytext)
#       mytext=sub("\\(ug","~'(ug", mytext)
#     }
#     mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
#     axis(side=4, at=xseq[k], labels=mytmp, las=1, cex.axis=0.5)
#   }
#   xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
#   tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
#   for (k in 1:(length(tmpseq)-1)){
#     if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
#       axis(side=4, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.5)
#     }
#   }
#   for (k in 1:(length(tmpseq)-1)){
#     if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
#       mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
#       if (grepl("m\\^", mytext)){
#         mytext=gsub("m\\^","'~m^", mytext)
#         mytext=sub(")","~')", mytext)
#       }
#       mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
#       axis(side=4, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
#            labels=mytmp, las=2)
#     }
#   }
#   axis(side=1, at=axTicks(1), cex.axis=0.7)
#   abline(v=1, lty=2)
#   abline(h=xseqgreysep,lty=3, col="grey")
#   xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#   abline(h=xseqblack,lty=3,col="black")
#   mtext(side=3, text="Bladder cancer", line=0.2, cex.lab=0.7)
#   legend("bottomright", pch = c(mypch, 19,15,4), col = mycolours,
#          legend=models, cex=0.7, bg="white")
#   mtext(side=1, outer=T, text="Odds Ratio", line=2, cex.lab=0.7)
#   dev.off()}





