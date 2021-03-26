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

# plot labels
setwd("../Dictionaries")
manhat_annot=read_csv("manhat_annot.csv")
plot_annot=read_csv("plot_annot.csv")[-c(1,2,21:25),]


### MANHATTAN ----
manhat=as.data.frame(readRDS("../Results/manhattan_plot.rds"))
bonf=-log10(0.05/nrow(manhat))
manhat=manhat[-1,] # Remove smoking

mylabels=manhat_annot$label
variable_cat=c(rep("Sociodemographic",8),
               rep("Health risk", 12),
               rep("Environmental", 8),
               rep("Medical", 15), 
               rep("Biomarkers", 28))
models=c("Base model", "Model adjusted on smoking status")
mycolours=c("navy","red","darkmagenta")
xseq=seq(1,nrow(manhat))

# Lung
ifelse(dir.exists("../Figures/Presentation"),"",dir.create("../Figures/Presentation"))
ifelse(dir.exists("../Figures/Report"),"",dir.create("../Figures/Report"))
pdf("../Figures/Presentation/manhattan_lung.pdf", height = 5, width = 10)
par(mar = c(10, 3, 3, 5))
plot(manhat[,1], pch = 17,col=mycolours[1], cex=0.7,
     xaxt = "n", yaxt="n", ylab="", xlab="", ylim = c(min(manhat[1:2]), max(manhat[1:2])+1))
axis(side=4, at=axTicks(2), cex.axis=0.7)
abline(h=bonf, lty = 2, col="darkred")
abline(h=-log10(0.05), lty = 2, col="grey")
abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
mtext(side=4, text=expression(-log[10](italic(p))), line=2, cex.lab=0.7)
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
  labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7,
       col.axis=labcol, col = labcol, font=labfont)
}
xseqblack=c(xseq[!duplicated(variable_cat)]-1/2, max(xseq)+1/2)
abline(v=xseqblack,lty=3,col="black")
for (k in 1:(length(xseqblack)-1)){
  axis(side=3, at=xseqblack[c(k,k+1)]+c(0.5,-0.5), line=0.5, labels=NA)
}
for (k in 1:(length(xseqblack)-1)){
  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
       labels=unique(variable_cat)[k])
}
legend("topright", pch=c(17, 19, NA, NA), lty=c(NA, NA, 2, 2),
       col=c(mycolours[1:2],"darkred","grey"), legend=c(models,"Bonferroni threshold","Nominal threshold"),
       cex=0.7, bg = "white")
dev.off()


# Bladder
pdf("../Figures/Presentation/manhattan_bladder.pdf", height = 5, width = 10)
par(mar = c(10, 3, 3, 5))
plot(manhat[,3], pch = 17,col=mycolours[1], cex=0.7,
     xaxt = "n", yaxt="n", ylab="", xlab="", ylim = c(min(manhat[3:4]), max(manhat[3:4])+1))
axis(side=4, at=axTicks(2), cex.axis=0.7)
abline(h=bonf, lty = 2, col="darkred")
abline(h=-log10(0.05), lty = 4, col="grey")
abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
abline(v = xseq, lty = 3, col = "grey", lwd=0.7)
mtext(side=4, text=expression(-log[10](italic(p))), line=2, cex.lab=0.7)
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
  labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7,
       col.axis=labcol, col = labcol, font=labfont)
}
xseqblack=c(xseq[!duplicated(variable_cat)]-1/2, max(xseq)+1/2)
abline(v=xseqblack,lty=3,col="black")
for (k in 1:(length(xseqblack)-1)){
  axis(side=3, at=xseqblack[c(k,k+1)]+c(0.5,-0.5), line=0.5, labels=NA)
}
for (k in 1:(length(xseqblack)-1)){
  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
       labels=unique(variable_cat)[k])
}
legend("topright", pch=c(17, 19, NA, NA), lty=c(NA, NA, 2, 2),
       col=c(mycolours[1:2],"darkred","grey"), legend=c(models,"Bonferroni threshold","Nominal threshold"),
       cex=0.7, bg = "white")
dev.off()

### NEW FOREST PLOT ----
forest=as.data.frame(readRDS("../Results/forest_plot.rds"))
forest=forest[,-which(grepl("logp_",colnames(forest)))] # Remove p-values
# Reorder
colnames=rownames(forest)
myorder=c(colnames[5:46],colnames[1:4],colnames[47:length(colnames)])
forest=forest %>%
  slice(match(myorder,rownames(forest)))

mylabels=plot_annot$label
myref=plot_annot$ref
variable_cat=c(rep("Sociodemographic",18),
               rep("Health risk", 28),
               rep("Environmental", 8),
               rep("Medical", 16), 
               rep("Biomarkers", 28))
models=c("Base model", "Model adjusted on smoking status")
mycolours=c("navy","red")
myspacing=2*2+1
xseq=seq((2+1),length(myorder)*myspacing, by=myspacing)
mypch=17
mycex=0.5
background=TRUE
mycolours=c("navy", "red")
myrange=c(min(forest[1:6], na.rm=T),max(forest[1:6],na.rm=T),min(forest[7:12], na.rm=T),max(forest[7:12],na.rm=T))
background=TRUE
background_colour=c("darkturquoise","hotpink") 

{pdf("../Figures/Report/forest_plot.pdf", height = 7, width = 11)
  par(oma=c(1, 5, 3, 1), mfrow=c(2,1),las=0)
  # Lung
  par(mar=c(5.5, 0, 0, 0),xpd=FALSE)
  plot(y=forest[,1], x=xseq-1,
       xlim=c(min(xseq), max(xseq)),ylim=myrange[1:2],
       xaxt="n", yaxt="n",
       pch=mypch, col=mycolours[1], lwd=1, cex=mycex,
       ylab="",xlab="",log="y")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=exp(c(-30,-30,30,30)), col=lighten(background_colour[1],0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=exp(c(-30,-30,30,30)), col=lighten(background_colour[1],0.99), border=NA)
    }
    box()
  }
  plotCI(x=xseq-1, y=forest[,1], li=forest[,2], ui=forest[,3],
         ylim=myrange[1:2], yaxt="n", xaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE)
  plotCI(x=xseq+1, y=forest[,4], li=forest[,5], ui=forest[,6],
         ylim=myrange[1:2], yaxt="n", xaxt="n", pch=19, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE, slty=2)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Lung cancer \nOdds ratio", line=2, cex.lab=0.7)
  abline(h=1, lty=2)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
  }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
  }
  par(xpd=TRUE)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
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
    if (is.na(myref)[k]){
      myadj=0.5
      mycex=0.5
      myline=5.5
    } else{
      myadj=0
      mycex=0.4
      myline=5.2
    }
    par(xpd=TRUE)
    mtext(side=1, mytmp, line=myline, at=xseq[k], adj=myadj, cex=mycex, las=2)
  }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=5.5, labels=NA, tck=-0.005)
    }
  }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      tmp=sub(" \\(","&(", mytext)
      split=strsplit(tmp, "&", perl=TRUE)
      mytext1=split[[1]][1]
      if (grepl("m\\^", mytext1)){
        mytext1=gsub("m\\^","'~m^", mytext1)
        mytext1=sub(")","~')", mytext1)
      }
      mytmp1=eval(parse(text=paste0("expression(","'", mytext1,"'",")")))
      mytmp2=split[[1]][2]
      mtext(mytmp1, side=1, at=mean(tmpseq[c(k,k+1)])-myspacing/2, line=5.8,  adj=1, cex=0.5,
            las=2)
      mtext(mytmp2, side=1, at=mean(tmpseq[c(k,k+1)])+myspacing/2, line=5.8,  adj=1, cex=0.4,
            las=2)
    }
  }
  legend("bottomright",inset=c(0,-0.2),pch=c(mypch,19), legend=models, col=mycolours, cex=0.6, bg="white")
  
  # Bladder
  par(mar=c(0, 0, 5.5, 0),xpd=FALSE)
  plot(y=forest[,7], x=xseq-1,
       xlim=c(min(xseq), max(xseq)),
       ylim=myrange[3:4],
       xaxt="n", yaxt="n",
       pch=mypch, col=mycolours[1], lwd=1, cex=mycex,
       ylab="",xlab="",log="y")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(y=exp(c(-30,-30,30,30)),
              x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              col=lighten(background_colour[2],0.95), border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(y=exp(c(-30,-30,30,30)),
              x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              col=lighten(background_colour[2],0.99), border=NA)
    }
    box()
  }
  plotCI(x=xseq-1, y=forest[,7], li=forest[,8], ui=forest[,9],
         ylim=myrange[3:4], yaxt="n", xaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE)
  plotCI(x=xseq+1, y=forest[,10], li=forest[,11], ui=forest[,12],
         ylim=myrange[3:4], yaxt="n", xaxt="n", pch=19, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE, slty=2)
  axis(side=2, at=axTicks(2), cex.axis=0.7)
  mtext(side=2, text="Bladder cancer \nOdds ratio", line=2, cex.lab=0.7)
  abline(h=1, lty=2)
  par(xpd=TRUE)
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black",lwd=0.5)
  abline(v=xseqgreysep,lty=1,lwd=0.1,col="grey")
  dev.off()
} # Run this to make plot

# ### Vertical FOREST PLOT ----
# forest=as.data.frame(readRDS("../Results/forest_plot.rds"))
# forest=forest[,-which(grepl("logp_",colnames(forest)))] # Remove p-values
# # Reorder
# colnames=rownames(forest)
# myorder=c(colnames[5:46],colnames[1:4],colnames[47:length(colnames)])
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
# 
# models=c("Base model", "Model adjusted on smoking status")
# # vertical two panels
# myspacing=4
# xseq=rep(myspacing, nrow(forest))
# xseq=cumsum(xseq)
# mypch=17
# mycex=0.5
# background=TRUE
# mycolours=c("navy", "red")
# myrange=c(min(forest[1:6], na.rm=T),max(forest[1:6],na.rm=T),min(forest[7:12], na.rm=T),max(forest[7:12],na.rm=T))
# 
# {pdf("../Figures/master_univ/forest_plot_v.pdf", height = 10, width = 7)
#   par(oma=c(4, 3, 2, 12), mar=c(0, 0.5, 0.5, 0), mfrow=c(1,2))
#   plot(x=forest[,1], y=xseq-1,
#        ylim=rev(c(min(xseq), max(xseq))),xlim=myrange[1:2],
#        xaxt="n", yaxt="n",
#        pch=mypch, col=mycolours[1], lwd=1, cex=mycex,
#        ylab="",xlab="",log="x")
#   xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
#   if (background){
#     for (k in seq(1,length(xseqgreysep),by=2)){
#       polygon(y=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
#               x=exp(c(-30,-30,30,30)), col="grey95", border=NA)
#       }
#     for (k in seq(2,length(xseqgreysep),by=2)){
#       polygon(y=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
#               x=exp(c(-30,-30,30,30)), col="grey98", border=NA)
#       }
#     box()
#     }
#   plotCI(y=xseq-1, x=forest[,1], li=forest[,2], ui=forest[,3], err = "x",
#          xlim=myrange[1:2], yaxt="n", xaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE)
#   plotCI(y=xseq+1, x=forest[,4], li=forest[,5], ui=forest[,6], err = "x",
#          xlim=myrange[1:2], yaxt="n", xaxt="n", pch=19, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE, slty=2)
#   axis(side=1, at=axTicks(1), cex.axis=0.7)
#   abline(v=1, lty=2)
#   abline(h=xseqgreysep,lty=3, col="grey")
#   xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#   abline(h=xseqblack,lty=3,col="black")
#   for (k in 1:(length(xseqblack)-1)){
#     axis(side=2, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#     }
#   for (k in 1:(length(xseqblack)-1)){
#     axis(side=2, at=mean(xseqblack[c(k,k+1)]), las = 3, line=0.2, tick=FALSE,
#          labels=unique(variable_cat)[k])
#     }
#   mtext(side=3, text="Lung cancer", line=0.2, cex.lab=0.7)
#   legend("bottomright", pch = c(mypch, 19), col = mycolours,
#          legend=models, cex=0.5, bg="white")
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
#       }
#     for (k in seq(2,length(xseqgreysep),by=2)){
#       polygon(x=exp(c(-30,-30,30,30)),
#               y=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
#               col="grey98", border=NA)
#       }
#     box()
#     }
#   plotCI(y=xseq-1, x=forest[,7], li=forest[,8], ui=forest[,9], err = "x",
#          xlim=myrange[3:4], yaxt="n", xaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE)
#   plotCI(y=xseq+1, x=forest[,10], li=forest[,11], ui=forest[,12], err = "x",
#          xlim=myrange[3:4], yaxt="n", xaxt="n", pch=19, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE, slty=2)
#   for (k in 1:length(xseq)){
#     mytext=mylabels[k]
#     if (grepl("m\\^", mytext)){
#       mytext=gsub("m\\^","'~m^", mytext)
#       mytext=sub(")","~')", mytext)
#       }
#     if (grepl("\\[", mytext)){
#       mytext=gsub("\\[","'[", mytext)
#       mytext=sub("\\(ug","~'(ug", mytext)
#       }
#     mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
#     axis(side=4, at=xseq[k], labels=mytmp, las=1, cex.axis=0.5)
#     }
#   xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
#   tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
#   for (k in 1:(length(tmpseq)-1)){
#     if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
#       axis(side=4, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.5)
#     }
#     }
#   for (k in 1:(length(tmpseq)-1)){
#     if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
#       mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
#       if (grepl("m\\^", mytext)){
#         mytext=gsub("m\\^","'~m^", mytext)
#         mytext=sub(")","~')", mytext)
#         }
#       mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
#       axis(side=4, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
#            labels=mytmp, las=2)
#     }
#     }
#   axis(side=1, at=axTicks(1), cex.axis=0.7)
#   abline(v=1, lty=2)
#   abline(h=xseqgreysep,lty=3, col="grey")
#   xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#   abline(h=xseqblack,lty=3,col="black")
#   mtext(side=3, text="Bladder cancer", line=0.2, cex.lab=0.7)
#   legend("bottomright", pch = c(mypch, 19), col = mycolours,
#          legend=models, cex=0.5, bg="white")
#   mtext(side=1, outer=T, text="Odds Ratio", line=2, cex.lab=0.7)
#   dev.off()
#   }
# 
# ### Horizontal FOREST PLOT ----
# forest=as.data.frame(readRDS("../Results/forest_plot.rds"))
# forest=forest[,-which(grepl("logp_",colnames(forest)))] # Remove p-values
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
# 
# models=c("Base model", "Model adjusted on smoking status")
# # Horizontal two panels
# myspacing=4
# xseq=rep(myspacing, nrow(forest))
# xseq=cumsum(xseq)
# mypch=17
# mycex=0.5
# background=TRUE
# mycolours=c("navy", "red")
# myrange=c(min(forest[1:6], na.rm=T),max(forest[1:6],na.rm=T),min(forest[7:12], na.rm=T),max(forest[7:12],na.rm=T))
# 
# {pdf("../Figures/master_univ/forest_plot_h.pdf", height = 8, width = 10)
#   par(oma=c(17, 2, 3, 4), mar=c(0, 0.5, 0.5, 0), mfrow=c(2,1))
#   plot(y=forest[,1], x=xseq-1,
#        xlim=c(min(xseq), max(xseq)),ylim=myrange[1:2],
#        xaxt="n", yaxt="n",
#        pch=mypch, col=mycolours[1], lwd=1, cex=mycex,
#        ylab="",xlab="",log="y")
#   xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
#   if (background){
#     for (k in seq(1,length(xseqgreysep),by=2)){
#       polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
#               y=exp(c(-30,-30,30,30)), col="grey95", border=NA)
#     }
#     for (k in seq(2,length(xseqgreysep),by=2)){
#       polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
#               y=exp(c(-30,-30,30,30)), col="grey98", border=NA)
#     }
#     box()
#   }
#   plotCI(x=xseq-1, y=forest[,1], li=forest[,2], ui=forest[,3],
#          ylim=myrange[1:2], yaxt="n", xaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE)
#   plotCI(x=xseq+1, y=forest[,4], li=forest[,5], ui=forest[,6],
#          ylim=myrange[1:2], yaxt="n", xaxt="n", pch=19, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE, slty=2)
#   axis(side=4, at=axTicks(2), cex.axis=0.7)
#   abline(h=1, lty=2)
#   abline(v=xseqgreysep,lty=3, col="grey")
#   xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#   abline(v=xseqblack,lty=3,col="black")
#   for (k in 1:(length(xseqblack)-1)){
#     axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#   }
#   for (k in 1:(length(xseqblack)-1)){
#     axis(side=3, at=mean(xseqblack[c(k,k+1)]), las = 1, line=0.2, tick=FALSE,
#          labels=unique(variable_cat)[k])
#   }
#   mtext(side=2, text="Lung cancer", line=0.2, cex.lab=0.7)
#   legend("topright", pch = c(mypch, 19), col = mycolours,
#          legend=models, cex=0.7, bg="white")
#   # Bladder
#   plot(y=forest[,7], x=xseq-1,
#        xlim=c(min(xseq), max(xseq)),ylim=myrange[3:4],
#        xaxt="n", yaxt="n",
#        pch=mypch, col=mycolours[1], lwd=1, cex=mycex,
#        ylab="",xlab="",log="y")
#   xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
#   if (background){
#     for (k in seq(1,length(xseqgreysep),by=2)){
#       polygon(y=exp(c(-30,-30,30,30)),
#               x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
#               col="grey95", border=NA)
#     }
#     for (k in seq(2,length(xseqgreysep),by=2)){
#       polygon(y=exp(c(-30,-30,30,30)),
#               x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
#               col="grey98", border=NA)
#     }
#     box()
#   }
#   plotCI(x=xseq-1, y=forest[,7], li=forest[,8], ui=forest[,9],
#          ylim=myrange[3:4], yaxt="n", xaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE)
#   plotCI(x=xseq+1, y=forest[,10], li=forest[,11], ui=forest[,12],
#          ylim=myrange[3:4], yaxt="n", xaxt="n", pch=19, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE, slty=2)
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
#     axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.5)
#   }
#   xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
#   tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
#   for (k in 1:(length(tmpseq)-1)){
#     if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
#       axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=6.7, labels=NA, tck=-0.005)
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
#       axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
#            labels=mytmp, las=2)
#     }
#   }
#   axis(side=4, at=axTicks(2), cex.axis=0.7)
#   abline(h=1, lty=2)
#   abline(v=xseqgreysep,lty=3, col="grey")
#   xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#   abline(v=xseqblack,lty=3,col="black")
#   mtext(side=2, text="Bladder cancer", line=0.2, cex.lab=0.7)
#   legend("topright", pch = c(mypch, 19), col = mycolours,
#          legend=models, cex=0.7, bg="white")
#   mtext(side=4, outer=T, text="Odds Ratio", line=2, cex.lab=0.7)
#   dev.off()}

### FOREST PLOTS ----
# forest=as.data.frame(readRDS("../Results/forest_plot.rds"))
# forest=forest[,-which(grepl("logp_",colnames(forest)))] # Remove p-values
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
# 
# models=c("Base model", "Model adjusted on smoking status")
# mypch=18
# mycex=0.45
# background=TRUE
# mycolours=darken(c("navy", "tomato", "forestgreen", "orange"), amount=0.2)
# myrange=c(min(forest, na.rm=T),max(forest,na.rm=T))
# myspacing=6
# xseq=rep(myspacing, nrow(forest))
# xseq=cumsum(xseq)

# # horizontal
# {pdf("../Figures/master_univ/forest_plot.pdf", height = 7, width = 10)
#   par(mar=c(17, 1, 3, 5))
#   plot(x=xseq-1.5, y=forest[,1],
#        ylim=myrange, xlim=c(min(xseq), max(xseq)),
#        xaxt="n", yaxt="n",
#        pch=mypch, col=mycolours[1], lwd=1, cex=mycex,
#        ylab="",xlab="",log="y")
#   xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
#   if (background){
#     for (k in seq(1,length(xseqgreysep),by=2)){
#       polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
#               y=exp(c(-30,-30,30,30)), col="grey95", border=NA)
#     }
#     for (k in seq(2,length(xseqgreysep),by=2)){
#       polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
#               y=exp(c(-30,-30,30,30)), col="grey98", border=NA)
#     }
#     box()
#   }
#   plotCI(x=xseq-1.5, y=forest[,1], li=forest[,2], ui=forest[,3],
#          xaxt="n", yaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex+0.2, sfrac=0.001,
#          ylab="",xlab="", add=TRUE)
#   plotCI(x=xseq-0.5, y=forest[,4], li=forest[,5], ui=forest[,6],
#          ylim=myrange, xaxt="n", yaxt="n", pch=15, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="",add=TRUE, slty=2)
#   plotCI(x=xseq+0.5, y=forest[,7], li=forest[,8], ui=forest[,9],
#          ylim=myrange, xaxt="n", yaxt="n", pch=19, col=mycolours[3], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="", add=TRUE, slty=3)
#   plotCI(x=xseq+1, y=forest[,10], li=forest[,11], ui=forest[,12],
#          ylim=myrange, xaxt="n", yaxt="n", pch=4, col=mycolours[4], lwd=1, cex=mycex, sfrac=0.001,
#          ylab="",xlab="",add=TRUE, slty=4)
#   axis(side=4, at=axTicks(2), cex.axis=0.7)
#   mtext(side=4, text="Odds Ratio", line=2, cex.lab=0.7)
#   abline(h=1, lty=2)
#   for (k in 1:length(xseq)){
#     mytext=mylabels[k]
#     if (grepl("m\\^", mytext)){
#       mytext=gsub("m\\^","'~m^", mytext)
#       mytext=sub(")","~')", mytext)
#       }
#     if (grepl("\\[", mytext)){
#       mytext=gsub("\\[","'[", mytext)
#       mytext=sub("\\(ug","~'(ug", mytext)
#       }
#     mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
#     axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.5)
#     }
#   abline(v=xseqgreysep,lty=3, col="grey")
#   xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
#   tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
#   for (k in 1:(length(tmpseq)-1)){
#     if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
#       axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.5)
#     }
#     }
#   for (k in 1:(length(tmpseq)-1)){
#     if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
#       mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
#       if (grepl("m\\^", mytext)){
#         mytext=gsub("m\\^","'~m^", mytext)
#         mytext=sub(")","~')", mytext)
#         }
#       mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
#       axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
#            labels=mytmp, las=2)
#     }
#     }
#   xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#   abline(v=xseqblack,lty=3,col="black")
#   for (k in 1:(length(xseqblack)-1)){
#     axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#     }
#   for (k in 1:(length(xseqblack)-1)){
#     axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#          labels=unique(variable_cat)[k])
#     }
#   legend("topright", pch = c(mypch, 15, 19, 4), col = mycolours,
#          legend=paste(rep(c("Lung cancer:","Bladder cancer:"),each=2),models),cex=0.7, bg="white")
#   dev.off()}

### SCATTER PLOTS ----
library(ggrepel)

forest=as.data.frame(readRDS("../Results/forest_plot.rds"))
pval=forest[,which(grepl("logp_",colnames(forest)))] # Extract p-values
mynames=rownames(pval)
pval=pval[-c(1:4),] # Remove smoking

# Reorder
myorder=c(colnames(forest)[5:46],colnames(forest)[1:4],colnames(forest)[47:ncol(forest)])
forest=forest %>%
  slice(match(myorder,rownames(forest)))

mylabels=plot_annot$label.point[-c(43:46)]
bonf=-log10(0.05/72)
models=c("Base model", "Model adjusted on smoking status")


## Base model

dat1=pval[,c(1,3)]

p1 = ggplot(dat1,aes(logp_lung.1, logp_bladder.1,
                    label = ifelse((logp_lung.1>=bonf&logp_bladder.1>=bonf),mylabels,""))) +
  geom_point(color = ifelse((dat1$logp_lung.1>=bonf&dat1$logp_bladder.1>=bonf),"darkred",
                            ifelse(dat1$logp_lung.1>=bonf, "darkturquoise",
                                   ifelse(dat1$logp_bladder.1>=bonf, "hotpink", "grey")))) +
  geom_text_repel(segment.colour = "grey", segment.size = 0.2) +
  labs(title = models[1])+
  xlab(expression(-log[10](italic(p))~"(Lung cancer vs Controls)"))+
  ylab(expression(-log[10](italic(p))~"(Bladder cancer vs Controls)")) +
  xlim(0, max(dat1)) +
  ylim(0, max(dat1)) +
  theme_bw() +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey")

p2 = ggplot(dat1,aes(logp_lung.1, logp_bladder.1,
                       label = ifelse((logp_lung.1>=bonf),mylabels,""))) +
  geom_point(color = ifelse((dat1$logp_lung.1>=bonf&dat1$logp_bladder.1>=bonf),"darkred",
                            ifelse(dat1$logp_lung.1>=bonf, "darkturquoise",
                                   ifelse(dat1$logp_bladder.1>=bonf, "hotpink", "grey")))) +
  geom_text_repel(size = 1.75, segment.colour = "grey", segment.size = 0.2) +
  labs(title = "Zoom in")+
  xlab(expression(-log[10](italic(p))~"(Lung cancer vs Controls)"))+
  ylab(expression(-log[10](italic(p))~"(Bladder cancer vs Controls)")) +
  xlim(bonf,max(dat1)) +
  ylim(0,bonf) +
  theme_bw() +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey")


## Model adjusted for smoking status

dat2=pval[,c(2,4)]

p3 = ggplot(dat2,aes(logp_lung.2, logp_bladder.2,
                    label = ifelse((logp_lung.2>=bonf&logp_bladder.2>=bonf),mylabels,""))) +
  geom_point(color = ifelse((dat2$logp_lung.2>=bonf&dat2$logp_bladder.2>=bonf),"darkred",
                            ifelse(dat2$logp_lung.2>=bonf, "darkturquoise",
                                   ifelse(dat2$logp_bladder.2>=bonf, "hotpink", "grey")))) +
  geom_text_repel(segment.colour = "grey", segment.size = 0.2) +
  labs(title = models[2])+
  xlab(expression(-log[10](italic(p))~"(Lung cancer vs Controls)"))+
  ylab(expression(-log[10](italic(p))~"(Bladder cancer vs Controls)")) +
  xlim(0, max(dat2)) +
  ylim(0, max(dat2)) +
  theme_bw() +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey")
p4 = ggplot(dat2,aes(logp_lung.2, logp_bladder.2,
                       label = ifelse((logp_lung.2<bonf&logp_bladder.2<bonf),"",mylabels))) +
  geom_point(color = ifelse((dat2$logp_lung.2>=bonf&dat2$logp_bladder.2>=bonf),"darkred",
                            ifelse(dat2$logp_lung.2>=bonf, "darkturquoise",
                                   ifelse(dat2$logp_bladder.2>=bonf, "hotpink", "grey")))) +
  geom_text_repel(size = 1.75, segment.colour = "grey", segment.size = 0.2) +
  labs(title = "Zoom in")+
  xlab(expression(-log[10](italic(p))~"(Lung cancer vs Controls)"))+
  ylab(expression(-log[10](italic(p))~"(Bladder cancer vs Controls)")) +
  xlim(bonf-1,max(dat2)) +
  ylim(0,4) +
  theme_bw() +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = bonf),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey")

library(cowplot)
pdf("../Figures/Report/scatter_pval.pdf", height = 10, width = 10)
plot_grid(p1, p2, p3, p4, nrow = 2, labels = c('A', '','B',''))
dev.off()

library(cowplot)
pdf("../Figures/Presentation/scatter_pval_base.pdf", height = 5, width = 10)
plot_grid(p1, p2, nrow = 1)
dev.off()

library(cowplot)
pdf("../Figures/Presentation/scatter_pval_adj.pdf", height = 5, width = 10)
plot_grid(p3, p4, nrow = 1)
dev.off()

