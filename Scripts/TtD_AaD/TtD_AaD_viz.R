# Creating the plots for the sensitivity analysis
#Fergal

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

library(colorspace)
library(ggplot2)
library(ggrepel)

m = 6

#Load in the data
arr = c("lung_aad_m","bladder_aad_m",
        "lung_aad_q","bladder_aad_q",
        "lung_ttd_m","bladder_ttd_m",
        "lung_ttd_q","bladder_ttd_q")[m]

forest_x = readRDS(paste0("../Results/TtD_AaD_univ/forest_plot_",arr,"x.rds"))
forest_y = readRDS(paste0("../Results/TtD_AaD_univ/forest_plot_",arr,"y.rds"))

forest = cbind(forest_x, forest_y)

mynames=rownames(forest)

# Reorder
myorder=c(mynames[5:38],mynames[1:4],mynames[39:length(mynames)])

forest=forest[myorder,]

bonf=-log10(0.05/72)
models=c("Base model", "Model adjusted on smoking status")


if(grepl("aad_m",arr)){
        title = c("AaD below median", "AaD above median")
}
if(grepl("aad_q",arr)){
        title = c("AaD below Q1", "AaD above Q3")
}
if(grepl("ttd_m",arr)){
        title = c("TtD below median", "TtD above median")
}
if(grepl("ttd_q",arr)){
        title = c("TtD below Q1", "TtD above Q3")
}
if(grepl("^lung",arr)){
        models = paste0("Lung cancer, ",models)
}
if(grepl("^bladder",arr)){
        models = paste0("Bladder cancer, ",models)
}
mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")
pval=forest[,which(grepl("logp.",colnames(forest)))] # Extract pval
or=forest[,-which(grepl("logp.",colnames(forest)))] # Extract or and ci
or = as.data.frame(or)
pval = as.data.frame(pval)
colnames(or) = paste0(c("o","l","u"),rep(c("x","y"),each = 6),
                      rep(1:2, each = 3))
colnames(pval) = paste0(c("p"),rep(c("x","y"),each = 2),1:2)
ifelse(sum(Inf %in% pval)>0,"Inf","FALSE")
# mylabels_asterisk = mylabels
# mylabels_asterisk[which(mylabels_asterisk=="Current smoker")]="Current smoker*"

or$label = pval$label = 1:nrow(or)
or$col = pval$col = rep(mycolours,times=c(18,20,8,16,28))
or$col_lab = pval$col_lab = darken(col, amount = 0.5)

xlim=c(min(or[,c(1:3,7:9)],na.rm=T),max(or[,c(1:3)],na.rm=T))
ylim=c(min(or[,c(1:3,7:9)],na.rm=T),max(or[,c(7:9)],na.rm=T))

p1 = ggplot(or, aes(x=ox1, y=oy1,label=ifelse((pval$px1>=bonf|pval$py1>=bonf)&(abs(ox1-oy1)>0.1),label, ""))) + 
        geom_abline(linetype = 'dotted', colour = 'grey') +
        geom_errorbar(aes(ymin=ly1, ymax=uy1),colour=or$col, width=.01*xlim[2]) +
        geom_errorbarh(aes(xmin=lx1, xmax=ux1),colour=or$col, height=.01*ylim[2]) +
        geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
        geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
        geom_label_repel(color=pval$col_lab, size=3.5, segment.colour = "grey",
                         segment.size = 0.5, max.overlaps = Inf,
                         label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
        geom_point(colour=or$col) +
        xlim(xlim) +
        ylim(ylim) +
        ylab(paste0("OR [95% CI] (", title[2]," vs control)")) +
        xlab(paste0("OR [95% CI] (", title[1]," vs control)")) +
        ggtitle(models[1]) +
        theme_bw() +
        theme(legend.position = "none")

xlim=c(0,max(pval[,1],na.rm=T))
ylim=c(0,max(pval[,3],na.rm=T))

p2 = ggplot(pval,aes(px1, py1,
                     label = ifelse((pval$px1>=bonf|pval$py1>=bonf)&abs(px1-py1) > 1,label,""))) +
        geom_abline(slope = 1, linetype="dotted",colour = "grey") +
        geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
        geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
        geom_point(color = ifelse((pval$px1>=bonf|pval$py1>=bonf),
                                  pval$col, "grey90")) +
        geom_label_repel(color=pval$col_lab, size=3.5, segment.colour = "grey",
                         segment.size = 0.5, max.overlaps = Inf,
                         nudge_y = 0.02, nudge_x = 0.02, 
                         label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
        labs(title = models[1])+
        xlab(substitute(paste(-log[10](italic(p)), nn), list(nn=paste0(" (", title[1]," vs control)"))))+
        ylab(substitute(paste(-log[10](italic(p)), nn), list(nn=paste0(" (", title[2]," vs control)")))) +
        xlim(xlim) +
        ylim(ylim) +
        theme_bw()

pdf(paste0("../Figures/Final/Supplementary/",arr,"_univ_base_or.pdf"), width = 5, height = 5)
p1
dev.off()
pdf(paste0("../Figures/Final/Supplementary/",arr,"_univ_base_pval.pdf"), width = 5, height = 5)
p2
dev.off()

xlim=c(min(or[,c(4:6,10:12)],na.rm=T),max(or[,c(4:6)],na.rm=T))
ylim=c(min(or[,c(4:6,10:12)],na.rm=T),max(or[,c(10:12)],na.rm=T))

p1 = ggplot(or, aes(x=ox2, y=oy2,label=ifelse((pval$px2>=bonf|pval$py2>=bonf)&(abs(ox2-oy2)>0.1),label, ""))) + 
        geom_abline(linetype = 'dotted', colour = 'grey') +
        geom_errorbar(aes(ymin=ly2, ymax=uy2),colour=or$col, width=.01*xlim[2]) +
        geom_errorbarh(aes(xmin=lx2, xmax=ux2),colour=or$col, height=.01*ylim[2]) +
        geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
        geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
        geom_label_repel(color=pval$col_lab, size=3.5, segment.colour = "grey",
                         segment.size = 0.5, max.overlaps = Inf,
                         label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
        geom_point(colour=or$col) +
        xlim(xlim) +
        ylim(ylim) +
        ylab(paste0("OR [95% CI] (", title[2]," vs control)")) +
        xlab(paste0("OR [95% CI] (", title[1]," vs control)")) +
        ggtitle(models[2]) +
        theme_bw() +
        theme(legend.position = "none")

xlim=c(0,max(pval[,2],na.rm=T))
ylim=c(0,max(pval[,4],na.rm=T))

p2 = ggplot(pval,aes(px2, py2,
                     label = ifelse((px2>=bonf|py2>=bonf)&abs(px2-py2) > 1,label,""))) +
        geom_abline(slope = 1, linetype="dotted",colour = "grey") +
        geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
        geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
        geom_point(color = ifelse((pval$px2>=bonf|pval$py2>=bonf),
                                  pval$col, "grey90")) +
        geom_label_repel(color=pval$col_lab, size=3.5, segment.colour = "grey",
                         segment.size = 0.5, max.overlaps = Inf,
                         nudge_y = 0.02, nudge_x = 0.02, 
                         label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
        labs(title = models[2])+
        xlab(substitute(paste(-log[10](italic(p)), nn), list(nn=paste0(" (", title[1]," vs control)"))))+
        ylab(substitute(paste(-log[10](italic(p)), nn), list(nn=paste0(" (", title[2]," vs control)")))) +
        xlim(xlim) +
        ylim(ylim) +
        theme_bw()

pdf(paste0("../Figures/Final/Supplementary/",arr,"_univ_adj_or.pdf"), width = 5, height = 5)
p1
dev.off()
pdf(paste0("../Figures/Final/Supplementary/",arr,"_univ_adj_pval.pdf"), width = 5, height = 5)
p2
dev.off()

