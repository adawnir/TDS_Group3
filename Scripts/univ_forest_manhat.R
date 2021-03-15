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
mycolours==darken(mycolours,amount=0.2)
xseq=seq(1,nrow(manhat))

# Lung
ifelse(dir.exists("../Figures/master_univ"),"",dir.create("../Figures/master_univ"))
pdf("../Figures/strat_sex_univ/manhattan_lung.pdf", height = 7, width = 10)
par(mar = c(10, 1, 3, 5))
plot(manhat[,1], pch = 17,col=mycolours[1], cex=0.7,
     xaxt = "n", yaxt="n", ylab="", xlab="", ylim = c(min(manhat[1:2]), max(manhat[1:2])))
axis(side=4, at=axTicks(2), cex.axis=0.7)
abline(h=bonf, lty = 2, col="red")
abline(h=-log10(0.05), lty = 4, col="grey")
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
legend("topright", pch=c(17, 19), col=mycolours[1:2], legend=models, cex=0.7, bg = "white")
dev.off()


# Bladder
pdf("../Figures/master_univ/manhattan_bladder.pdf", height = 7, width = 10)
par(mar = c(10, 1, 3, 5))
plot(manhat[,3], pch = 17,col=mycolours[1], cex=0.7,
     xaxt = "n", yaxt="n", ylab="", xlab="", ylim = c(min(manhat[3:4]), max(manhat[3:4])))
axis(side=4, at=axTicks(2), cex.axis=0.7)
abline(h=bonf, lty = 2, col="red")
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
legend("topright", pch=c(17, 19), col=mycolours[1:2], legend=models, cex=0.7, bg = "white")
dev.off()

### FOREST PLOTS ----
forest=as.data.frame(readRDS("../Results/forest_plot.rds"))
forest=forest[,-which(grepl("logp_",colnames(forest)))] # Remove p-values
# Reorder
myorder=plot_annot$col.name
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
mypch=18
mycex=0.45
background=TRUE
mycolours=darken(c("navy", "tomato", "forestgreen", "orange"), amount=0.2)
myrange=c(min(forest, na.rm=T),max(forest,na.rm=T))
myspacing=6
xseq=rep(myspacing, nrow(forest))
xseq=cumsum(xseq)
# horizontal
{pdf("../Figures/master_univ/forest_plot.pdf", height = 7, width = 10)
  par(mar=c(17, 1, 3, 5))
  plot(x=xseq-1.5, y=forest[,1],
       ylim=myrange, xlim=c(min(xseq), max(xseq)),
       xaxt="n", yaxt="n",
       pch=mypch, col=mycolours[1], lwd=1, cex=mycex,
       ylab="",xlab="",log="y")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=exp(c(-30,-30,30,30)), col="grey95", border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              y=exp(c(-30,-30,30,30)), col="grey98", border=NA)
    }
    box()
  }
  plotCI(x=xseq-1.5, y=forest[,1], li=forest[,2], ui=forest[,3],
         xaxt="n", yaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex+0.2, sfrac=0.001,
         ylab="",xlab="", add=TRUE)
  plotCI(x=xseq-0.5, y=forest[,4], li=forest[,5], ui=forest[,6],
         ylim=myrange, xaxt="n", yaxt="n", pch=15, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="",add=TRUE, slty=2)
  plotCI(x=xseq+0.5, y=forest[,7], li=forest[,8], ui=forest[,9],
         ylim=myrange, xaxt="n", yaxt="n", pch=19, col=mycolours[3], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE, slty=3)
  plotCI(x=xseq+1, y=forest[,10], li=forest[,11], ui=forest[,12],
         ylim=myrange, xaxt="n", yaxt="n", pch=4, col=mycolours[4], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="",add=TRUE, slty=4)
  axis(side=4, at=axTicks(2), cex.axis=0.7)
  mtext(side=4, text="Odds Ratio", line=2, cex.lab=0.7)
  abline(h=1, lty=2)
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
    axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.5)
    }
  abline(v=xseqgreysep,lty=3, col="grey")
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.5)
    }
    }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      if (grepl("m\\^", mytext)){
        mytext=gsub("m\\^","'~m^", mytext)
        mytext=sub(")","~')", mytext)
        }
      mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
      axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
           labels=mytmp, las=2)
    }
    }
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(v=xseqblack,lty=3,col="black")
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
    }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
    }
  legend("topright", pch = c(mypch, 15, 19, 4), col = mycolours,
         legend=paste(rep(c("Lung cancer:","Bladder cancer:"),each=2),models),cex=0.7, bg="white")
  dev.off()}

### Vertical FOREST PLOT ----
# vertical two panels
myspacing=4
xseq=rep(myspacing, nrow(forest))
xseq=cumsum(xseq)
mypch=17
mycex=0.5
background=TRUE
mycolours=darken(c("navy", "tomato"), amount=0.2)
myrange=c(min(forest[1:6], na.rm=T),max(forest[1:6],na.rm=T),min(forest[7:12], na.rm=T),max(forest[7:12],na.rm=T))

{pdf("../Figures/master_univ/forest_plot.2.pdf", height = 12, width = 10)
  par(oma=c(4, 3, 2, 17), mar=c(0, 0.5, 0.5, 0), mfrow=c(1,2))
  plot(x=forest[,1], y=xseq-1,
       ylim=rev(c(min(xseq), max(xseq))),xlim=myrange[1:2],
       xaxt="n", yaxt="n",
       pch=mypch, col=mycolours[1], lwd=1, cex=mycex,
       ylab="",xlab="",log="x")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(y=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              x=exp(c(-30,-30,30,30)), col="grey95", border=NA)
      }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(y=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              x=exp(c(-30,-30,30,30)), col="grey98", border=NA)
      }
    box()
    }
  plotCI(y=xseq-1, x=forest[,1], li=forest[,2], ui=forest[,3], err = "x",
         xlim=myrange[1:2], yaxt="n", xaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE)
  plotCI(y=xseq+1, x=forest[,4], li=forest[,5], ui=forest[,6], err = "x",
         xlim=myrange[1:2], yaxt="n", xaxt="n", pch=19, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE, slty=2)
  axis(side=1, at=axTicks(1), cex.axis=0.7)
  abline(v=1, lty=2)
  abline(h=xseqgreysep,lty=3, col="grey")
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(h=xseqblack,lty=3,col="black")
  for (k in 1:(length(xseqblack)-1)){
    axis(side=2, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
    }
  for (k in 1:(length(xseqblack)-1)){
    axis(side=2, at=mean(xseqblack[c(k,k+1)]), las = 3, line=0.2, tick=FALSE,
         labels=unique(variable_cat)[k])
    }
  mtext(side=3, text="Lung cancer", line=0.2, cex.lab=0.7)
  legend("bottomright", pch = c(mypch, 19), col = mycolours,
         legend=models, cex=0.5, bg="white")
  # Bladder
  plot(x=forest[,7], y=xseq-1,
       ylim=rev(c(min(xseq), max(xseq))),xlim=myrange[3:4],
       xaxt="n", yaxt="n",
       pch=mypch, col=mycolours[1], lwd=1, cex=mycex,
       ylab="",xlab="",log="x")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=exp(c(-30,-30,30,30)),
              y=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              col="grey95", border=NA)
      }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=exp(c(-30,-30,30,30)),
              y=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
              col="grey98", border=NA)
      }
    box()
    }
  plotCI(y=xseq-1, x=forest[,7], li=forest[,8], ui=forest[,9], err = "x",
         xlim=myrange[3:4], yaxt="n", xaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE)
  plotCI(y=xseq+1, x=forest[,10], li=forest[,11], ui=forest[,12], err = "x",
         xlim=myrange[3:4], yaxt="n", xaxt="n", pch=19, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE, slty=2)
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
    axis(side=4, at=xseq[k], labels=mytmp, las=1, cex.axis=0.5)
    }
  xseqgrey=xseq[which(!duplicated(myref)|is.na(myref))]-myspacing/2
  tmpseq=c(xseqgrey,max(xseqgrey)-myspacing/2)
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      axis(side=4, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.5)
    }
    }
  for (k in 1:(length(tmpseq)-1)){
    if (!is.na(myref[which(!duplicated(myref)|is.na(myref))])[k]){
      mytext=myref[which(!duplicated(myref)|is.na(myref))][k]
      if (grepl("m\\^", mytext)){
        mytext=gsub("m\\^","'~m^", mytext)
        mytext=sub(")","~')", mytext)
        }
      mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
      axis(side=4, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.5,
           labels=mytmp, las=2)
    }
    }
  axis(side=1, at=axTicks(1), cex.axis=0.7)
  abline(v=1, lty=2)
  abline(h=xseqgreysep,lty=3, col="grey")
  xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
  abline(h=xseqblack,lty=3,col="black")
  mtext(side=3, text="Bladder cancer", line=0.2, cex.lab=0.7)
  legend("bottomright", pch = c(mypch, 19), col = mycolours,
         legend=models, cex=0.5, bg="white")
  mtext(side=1, outer=T, text="Odds Ratio", line=2, cex.lab=0.7)
  dev.off()}


