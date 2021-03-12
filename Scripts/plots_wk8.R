# forest + manhattan wk 8
# by ines gerard-ursin march 11

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/"
setwd(project_path)

library(RColorBrewer)
library(plotrix)
library(openxlsx)
library(tableone)
library(plotrix)
library(colorspace)

# Update group names
mgroups <- c( "sociodemographic", "health risk", "environment", "medical", "biomarkers")

# to do FOR ALL:
# update group names
# eventually add vector of significant things to look at

### MANHATTAN ----
# update manhattan with groups
# add variables to highlight as vector
# do not include smoking

mydata = readRDS("../Results/manhattan_plot.rds")
mydata<-as.data.frame(mydata)
MyPal = brewer.pal("Paired", n = 12)

bonf=-log10(0.05/72)

# Lung
# significant = red (6), not significant = blue (2)
pdf("../Figures/manhattan_lung_wk8.pdf", height = 8, width = 20) # increase width so you can read all covariate names
par(mar = c(20, 5, 3, 3)) # sets the margins of the graphs
plot(mydata$lung.1[2:72], pch = 17, 
     col = ifelse(mydata$lung.1[2:72] > bonf, yes = MyPal[6], no = MyPal[2]),
     xaxt = "n", ylab = expression(-log[10](italic(p))), xlab = "",
     las = 1, ylim = c(min((mydata$lung.1[2:72])), 
                       max((mydata$lung.1[2:72]))))
points((mydata$lung.2[2:72]), pch = 18,
       col=ifelse(mydata$lung.2[2:72] > bonf, yes = MyPal[6], no = MyPal[2]))
abline(h = (bonf), col = "red")
abline(v = seq(1, 71), lty = 3, col = "grey")
axis(1, at = 1:71, ifelse(mydata$lung.1[2:72] > bonf & mydata$lung.2[2:72] > bonf,
                          yes = rownames(mydata[2:72,]), no = ""), las = 2, col.axis = MyPal[6])
axis(1, at = 1:71, ifelse(mydata$lung.1[2:72] > bonf & mydata$lung.2[2:72] > bonf,
                          yes = "", no = rownames(mydata[2:72,])), las = 2, col.axis = MyPal[2])
legend("topright", pch = c(17, 18, 19, 19), col = c("black", "black", MyPal[6],MyPal[2]), 
       legend = c("Lung cancer adjusted on sex, age, & bmi", "Lung cancer adjusted on 
                  sex, age, bmi, & smoking", "Significant for both models", "Not significant for both models"))
mtext(mgroups[1], at=c(4.5), side=1, line=8) # socio-demo
axis(side=1, at=c(1,8), line=7, labels=NA, cex=0.7)
mtext(mgroups[2], at=c(14.5), side=1, line=8) # health risk
axis(side=1, at=c(9,20), line=7, labels=NA, cex=0.7) 
mtext(mgroups[3], at=c(24.5), side=1, line=8) # environment
axis(side=1, at=c(21,28), line=7, labels=NA, cex=0.7)
mtext(mgroups[4], at=c(35.5), side=1, line=11) # medical
axis(side=1, at=c(29,43), line=10, labels=NA, cex=0.7) 
mtext(mgroups[5], at=c(58), side=1, line=14) # biomarkers
axis(side=1, at=c(44,72), line=13, labels=NA, cex=0.7)

dev.off()


# Bladder
# significant = orange (8), not significant = green (4)
pdf("../Figures/manhattan_bladder_wk8.pdf", height = 8, width = 20) # increase width so you can read all covariate names
par(mar = c(20, 5, 3, 3)) # sets the margins of the graphs # bottom, left, top, right
plot(mydata$bladder.1[2:72], pch = 17, 
     col = ifelse(mydata$bladder.1[2:72] > bonf, yes = MyPal[8], no = MyPal[4]),
     xaxt = "n", ylab = expression(-log[10](italic(p))), xlab = "",
     las = 1, ylim = c(min((mydata$bladder.1[2:72])), 
                       max((mydata$bladder.1[2:72]))))
points((mydata$bladder.2[2:72]), pch = 18,
       col=ifelse(mydata$bladder.2[2:72] > bonf, yes = MyPal[8], no = MyPal[4]))
abline(h = (bonf), col = "red")
abline(v = seq(1, 71), lty = 3, col = "grey")
axis(1, at = 1:71, ifelse(mydata$bladder.1[2:72] > bonf & mydata$bladder.2[2:72] > bonf,
                          yes = rownames(mydata[2:72,]), no = ""), las = 2, col.axis = MyPal[8])
axis(1, at = 1:71, ifelse(mydata$bladder.1[2:72] > bonf & mydata$bladder.2[2:72] > bonf,
                          yes = "", no = rownames(mydata[2:72,])), las = 2, col.axis = MyPal[4])
legend("topright", pch = c(17, 18, 19, 19), col = c("black", "black", MyPal[8],MyPal[4]), 
       legend = c("bladder cancer adjusted on sex, age, & bmi", "bladder cancer adjusted on 
                  sex, age, bmi, & smoking", "Significant for both models", "Not significant for both models"), cex=0.85)
mtext(mgroups[1], at=c(4.5), side=1, line=8) # socio-demo
axis(side=1, at=c(1,8), line=7, labels=NA, cex=0.7)
mtext(mgroups[2], at=c(14.5), side=1, line=8) # health risk
axis(side=1, at=c(9,20), line=7, labels=NA, cex=0.7) 
mtext(mgroups[3], at=c(24.5), side=1, line=8) # environment
axis(side=1, at=c(21,28), line=7, labels=NA, cex=0.7)
mtext(mgroups[4], at=c(35.5), side=1, line=11) # medical
axis(side=1, at=c(29,43), line=10, labels=NA, cex=0.7) 
mtext(mgroups[5], at=c(58), side=1, line=14) # biomarkers
axis(side=1, at=c(44,72), line=13, labels=NA, cex=0.7)

dev.off()

### SCATTER PLOTS ----
# use same scale for axes
# zoom in on 3 regions in plot
# for either & both individually
# remove labels of not significant

pdf("../Figures/scatter_pval_confounders.pdf", height = 7, width = 10)
par(mar = c(6, 5, 3, 3))
plot((mydata$lung.1), (mydata$bladder.1), main = expression(-Log[10](italic(p))~"values"), pch = 17, 
     col = ifelse(mydata$bladder.1 > bonf & mydata$lung.1 > bonf, yes = MyPal[6], no = MyPal[2]), 
     ylab =expression("Bladder"~-log[10](italic(p))), xlab = expression("Lung"~-log[10](italic(p))), las = 1,
     xlim=c(0,5),ylim=c(0,5))
abline(coef=c(0,1), col = "red")
abline(v=1,lty=2,col="grey") # add grey boundaries to show x = 1
abline(h=1,lty=2,col="grey") # add grey boundaries to show y = 1
abline(v = (bonf), lty = 3, col = "tomato") # add lines to show log bonferroni (3.15)
text(0, 3.25, "bonferroni", cex = 0.6, las=2, col = "tomato")
text(3.35, 0, "bonferroni", cex = 0.6, las=3, col = "tomato")
abline(h = (bonf), lty=3, col="tomato") # add lines to show log bonferroni (3.15)
text((mydata$lung.1), (mydata$bladder.1), labels=ifelse(mydata$bladder.1 > bonf & mydata$lung.1 > bonf, yes = rownames(mydata), no = ""), cex=0.6, font=2)
#text((mydata$lung.1), (mydata$bladder.1), labels=rownames(mydata), cex=0.6, font=2)
legend("topright", pch = c(17, 17), col = c(MyPal[6],MyPal[2]), 
       legend = c("Significant for both models", "Not significant for both models"), cex=0.6)

dev.off()

pdf("../Figures/scatter_pval_confounders_bladder.pdf", height = 7, width = 10)
par(mar = c(6, 5, 3, 3))
plot((mydata$lung.1), (mydata$bladder.1), main = expression(-Log[10](italic(p))~"values"), pch = 17, 
     col = ifelse(mydata$bladder.1 > bonf, yes = MyPal[6], no = MyPal[2]), 
     ylab =expression("Bladder"~-log[10](italic(p))), xlab = expression("Lung"~-log[10](italic(p))), las = 1,
     xlim=c(0,70),ylim=c(0,10))
abline(coef=c(0,1), col = "red")
abline(v=1,lty=2,col="grey") # add grey boundaries to show x = 1
abline(h=1,lty=2,col="grey") # add grey boundaries to show y = 1
abline(v = (bonf), lty = 3, col = "tomato") # add lines to show log bonferroni (3.15)
text(0, 3.3, "bonferroni", cex = 0.6, las=2, col = "tomato")
abline(h = (bonf), lty=3, col="tomato") # add lines to show log bonferroni (3.15)
text((mydata$lung.1), (mydata$bladder.1), labels=ifelse(mydata$bladder.1 > bonf, yes = rownames(mydata), no = ""), cex=0.6, font=2)
legend("topright", pch = c(17, 17), col = c(MyPal[6],MyPal[2]), 
       legend = c("Significant for bladder cancer", "Not significant for bladder cancer"), cex=0.6)

dev.off()

pdf("../Figures/scatter_pval_confounders_lung.pdf", height = 7, width = 10)
par(mar = c(6, 5, 3, 3))
plot((mydata$lung.1), (mydata$bladder.1), main = expression(-Log[10](italic(p))~"values"), pch = 17, 
     col = ifelse(mydata$lung.1 > bonf, yes = MyPal[6], no = MyPal[2]), 
     ylab =expression("Bladder"~-log[10](italic(p))), xlab = expression("Lung"~-log[10](italic(p))), las = 1,
     xlim=c(0,100),ylim=c(0,7))
abline(coef=c(0,1), col = "red")
abline(v=1,lty=2,col="grey") # add grey boundaries to show x = 1
abline(h=1,lty=2,col="grey") # add grey boundaries to show y = 1
abline(v = (bonf), lty = 3, col = "tomato") # add lines to show log bonferroni (3.15)
text(0, 3.3, "bonferroni", cex = 0.6, las=2, col = "tomato")
abline(h = (bonf), lty=3, col="tomato") # add lines to show log bonferroni (3.15)
text((mydata$lung.1)+1, (mydata$bladder.1), labels=ifelse(mydata$lung.1 > bonf, yes = rownames(mydata), no = ""), cex=0.5, font=1)
legend("topright", pch = c(17, 17), col = c(MyPal[6],MyPal[2]), 
       legend = c("Significant for lung cancer", "Not significant for lung cancer"), cex=0.6)

dev.off()

pdf("../Figures/scatter_pval_smoking.pdf", height = 7, width = 10)
par(mar = c(6, 5, 3, 3))
plot((mydata$lung.2), (mydata$bladder.2), main = expression(-Log[10](italic(p))~"values adjusted for smoking"), pch = 17, 
     col = ifelse(mydata$bladder.2 > bonf & mydata$lung.2 > bonf, yes = MyPal[6], no = MyPal[2]), 
     ylab =expression("Bladder"~-log[10](italic(p))), xlab = expression("Lung"~-log[10](italic(p))), las = 1,
     xlim=c(0,15),ylim=c(0,15))
abline(coef=c(0,1), col = "red")
abline(v=1,lty=2,col="grey") # add grey boundaries to show x = 1
abline(h=1,lty=2,col="grey") # add grey boundaries to show y = 1
abline(v = (bonf), lty = 3, col = "tomato")
abline(h = (bonf), lty=3, col="tomato")
text(0, 3.4, "bonferroni", cex = 0.6, las=2, col = "tomato")
text((mydata$lung.2), (mydata$bladder.2), labels=ifelse(mydata$bladder.2 > bonf & mydata$lung.2 > bonf, yes = rownames(mydata), no = ""), cex=0.6, font=2)
legend("topright", pch = c(17, 17), col = c(MyPal[6],MyPal[2]), 
       legend = c("Significant for both models", "Not significant for both models"), cex=0.6)

dev.off()

pdf("../Figures/scatter_pval_smoking_bladder.pdf", height = 7, width = 10)
par(mar = c(6, 5, 3, 3))
plot((mydata$lung.2), (mydata$bladder.2), main = expression(-Log[10](italic(p))~"values adjusted for smoking"), pch = 17, 
     col = ifelse(mydata$bladder.2 > bonf, yes = MyPal[6], no = MyPal[2]), 
     ylab =expression("Bladder"~-log[10](italic(p))), xlab = expression("Lung"~-log[10](italic(p))), las = 1,
     xlim=c(0,40),ylim=c(0,6))
abline(coef=c(0,1), col = "red")
abline(v=1,lty=2,col="grey") # add grey boundaries to show x = 1
abline(h=1,lty=2,col="grey") # add grey boundaries to show y = 1
abline(v = (bonf), lty = 3, col = "tomato")
abline(h = (bonf), lty=3, col="tomato")
text(0, 3.3, "bonferroni", cex = 0.6, las=2, col = "tomato")
text((mydata$lung.2), (mydata$bladder.2), labels=ifelse(mydata$bladder.2 > bonf, yes = rownames(mydata), no = ""), cex=0.6, font=2)
legend("topright", pch = c(17, 17), col = c(MyPal[6],MyPal[2]), 
       legend = c("Significant for bladder cancer", "Not significant for bladder cancer"), cex=0.6)

dev.off()

pdf("../Figures/scatter_pval_smoking_lung.pdf", height = 7, width = 10)
par(mar = c(6, 5, 3, 3))
plot((mydata$lung.2), (mydata$bladder.2), main = expression(-Log[10](italic(p))~"values adjusted for smoking"), pch = 17, 
     col = ifelse(mydata$lung.2 > bonf, yes = MyPal[6], no = MyPal[2]), 
     ylab =expression("Bladder"~-log[10](italic(p))), xlab = expression("Lung"~-log[10](italic(p))), las = 1,
     xlim=c(0,50),ylim=c(0,5))
abline(coef=c(0,1), col = "red")
abline(v=1,lty=2,col="grey") # add grey boundaries to show x = 1
abline(h=1,lty=2,col="grey") # add grey boundaries to show y = 1
abline(v = (bonf), lty = 3, col = "tomato")
abline(h = (bonf), lty=3, col="tomato")
text(0, 3.3, "bonferroni", cex = 0.6, las=2, col = "tomato")
text((mydata$lung.2), (mydata$bladder.2), labels=ifelse(mydata$lung.2 > bonf, yes = rownames(mydata), no = ""), cex=0.6, font=1)
legend("topright", pch = c(17, 17), col = c(MyPal[6],MyPal[2]), 
       legend = c("Significant for lung cancer", "Not significant for lung cancer"), cex=0.6)

dev.off()

### FOREST PLOTS ----
# do vertical plot (covariates on left side/y axis)

f = readRDS("../Results/forest_plot.rds")
f <- as.data.frame(f)

### data processing forest ----
fgroups <- c(rep("health risk",4),rep("sociodemographic", 18), rep("health risk", 24), rep("environment", 7), rep("medical", 17), rep("biomarkers", 28))
model<-c(rep(c("lung + bmi,age,sex","lung + bmi,age,sex,smoking","bladder + bmi,age,sex","bladder + bmi,age,sex,smoking"),98))
cancer <-c(rep(c(rep("lung",2),rep("bladder",2)),98))
f$group <- fgroups


f.or <- f %>%
  select(group,or_lung.1, or_lung.2, or_bladder.1, or_bladder.2) %>%
  tibble::rownames_to_column(var = "covariate") %>%
  pivot_longer(cols=c(or_lung.1, or_lung.2, or_bladder.1, or_bladder.2),
               names_to="model", 
               values_to="or")%>%
  arrange(group,covariate)

f.l95 <- f %>%
  select(group,l95_lung.1, l95_lung.2, l95_bladder.1, l95_bladder.2) %>%
  tibble::rownames_to_column(var = "covariate") %>%
  pivot_longer(cols=c(l95_lung.1, l95_lung.2, l95_bladder.1, l95_bladder.2),
               names_to="model", 
               values_to="l95")%>%
  arrange(group,covariate)

f.u95 <- f %>%
  select(group,u95_lung.1, u95_lung.2, u95_bladder.1, u95_bladder.2) %>%
  tibble::rownames_to_column(var = "covariate") %>%
  pivot_longer(cols=c(u95_lung.1, u95_lung.2, u95_bladder.1, u95_bladder.2),
               names_to="model", 
               values_to="u95")%>%
  arrange(group,covariate)

f.logp <- f %>%
  select(group,logp_lung.1, logp_lung.2, logp_bladder.1, logp_bladder.2) %>%
  tibble::rownames_to_column(var = "covariate") %>%
  pivot_longer(cols=c(logp_lung.1, logp_lung.2, logp_bladder.1, logp_bladder.2),
               names_to="model", 
               values_to="logp") %>%
  arrange(group,covariate)

f.logp$model <- model
f.l95$model <- model
f.u95$model <- model
f.or$model <- model


f.logp$cancer <- cancer
f.l95$cancer <- cancer
f.u95$cancer <- cancer
f.or$cancer <- cancer

forest<-right_join(f.logp, f.or, by=c("model","covariate","group","cancer")) %>%
  inner_join(f.u95) %>%
  inner_join(f.l95) 
saveRDS(forest, "../Results/forest_wk8.rds")


### OPTION 1: KEEP ALL MODELS SEPARATE ####
# use free-y and reformat what you have so every CI is included
# make sure everything is included and visible

forest = readRDS("../Results/forest_wk8.rds")

pdf("../Figures/forest_wk8_zoom.pdf", height = 10, width = 15) # increase width so you can read all covariate names
par(mar = c(13, 5, 3, 5)) 
g <- ggplot(forest, aes(y = covariate, x = or, xmin = l95, xmax = u95, color=factor(group))) +
  geom_point() +
  geom_errorbarh(height = .1) +
  scale_x_continuous(limits=c(0,2),breaks=c(0,.5,1,1.5,2))+
  geom_vline(xintercept=1, color="grey60",linetype="dashed")+
  facet_grid(group~model, scales = "free", space = "free") +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0)) +
  theme(axis.text.y = element_text(color=ifelse(forest$logp[forest$model=="lung + bmi,age,sex"] > bonf & forest$logp[forest$model=="lung + bmi,age,sex,smoking"] > bonf &
                                                  forest$logp[forest$model=="bladder + bmi,age,sex"] > bonf & forest$logp[forest$model=="bladder + bmi,age,sex,smoking"] > bonf, 
                                                yes = MyPal[6], no = "black")))  # axis labels
g
dev.off()

### OPTION 2: using plotrix ####
myspacing=6
xseq=rep(myspacing, nrow(f))
xseq=cumsum(xseq)

myvariables=tmptable[,2]
mynames=rownames(f)

mypch=18
mycex=0.45
background=TRUE
mycolours=darken(c("navy", "orange", "forestgreen", "tomato"), amount=0.2)
# horizontal
{pdf("../Figures/forest_wk8_plotrix_h.pdf", height = 10, width = 15)
  par(mar=c(17, 1, 3, 5))
  plotCI(x=xseq-1.5, y=f$or_lung.1, li=f$l95_lung.1, ui=f$u95_lung.1, 
         ylim=c(0,8), 
         xlim=c(min(xseq), max(xseq)),
         xaxt="n", yaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]), 
              y=c(-10,-10,10,10), col="grey95", border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]), 
              y=c(-10,-10,10,10), col="grey98", border=NA)
    }
    box()
  }
  plotCI(x=xseq-1.5, y=f$or_lung.1, li=f$l95_lung.1, ui=f$u95_lung.1, 
         xaxt="n", yaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex+0.2, sfrac=0.001,
         ylab="",xlab="", add=TRUE)
  plotCI(x=xseq-0.5, y=f$or_lung.2, li=f$l95_lung.2, ui=f$u95_lung.2, 
         ylim=c(0,5), #ylim=myrange,
         xaxt="n", yaxt="n", pch=15, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE, slty=2)
  plotCI(x=xseq+0.5, y=f$or_bladder.1, li=f$l95_bladder.1, ui=f$u95_bladder.1, 
         ylim=c(0,5), #ylim=myrange,
         xaxt="n", yaxt="n", pch=19, col=mycolours[3], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE, slty=3)
  plotCI(x=xseq+1.5, y=f$or_bladder.2, li=f$l95_bladder.2, ui=f$u95_bladder.2, 
         ylim=c(0,5), #ylim=myrange,
         xaxt="n", yaxt="n", pch=4, col=mycolours[4], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE, slty=4)
  axis(side=4, at=axTicks(2), cex.axis=0.7)
  mtext(side=4, text="Odds Ratio", line=2, cex.lab=0.7)
  abline(h=1, lty=2)
  for (k in 1:length(xseq)){
    mytext=mynames
    axis(side=1, at=xseq[k], labels=mytext[k], las=2, cex.axis=0.7)
  }
  legend("topright", pch = c(mypch, 15, 19, 4), col = mycolours, 
         legend = c("lung + bmi,age,sex","lung + bmi,age,sex,smoking","bladder + bmi,age,sex","bladder + bmi,age,sex,smoking"))
  dev.off()}

# vertical
{pdf("../Figures/forest_wk8_plotrix_v.pdf", height = 10, width = 15)
  par(mar=c(5, 17, 3, 1)) # bottom, left, top , right
  plotCI(f$or_lung.1, li=f$l95_lung.1, ui=f$u95_lung.1,
         xlim=c(0,8), 
         ylim=c(min(xseq), max(xseq)),
         xaxt="n", yaxt="n", pch=mypch, col=rgb(0,0,0,0), lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="")
  xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
  if (background){
    for (k in seq(1,length(xseqgreysep),by=2)){
      polygon(y=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]), 
              x=c(-10,-10,10,10), col="grey95", border=NA)
    }
    for (k in seq(2,length(xseqgreysep),by=2)){
      polygon(y=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]), 
              x=c(-10,-10,10,10), col="grey98", border=NA)
    }
    box()
  }
  
  plotCI(f$or_lung.1, y=xseq, liw=f$l95_lung.1, uiw=f$u95_lung.1, 
         xaxt="n", yaxt="n", pch=mypch, col=mycolours[1], lwd=1, cex=mycex+0.2, sfrac=0.001,
         ylab="",xlab="", add=TRUE)
  plotCI(f$or_lung.2, y=xseq-0.5, liw=f$l95_lung.2, uiw=f$u95_lung.2, 
         xlim=c(0,5), #ylim=myrange,
         xaxt="n", yaxt="n", pch=15, col=mycolours[2], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE, slty=2)
  plotCI(f$or_bladder.1, y=xseq+0.5, liw=f$l95_bladder.1, uiw=f$u95_bladder.1, 
         xlim=c(0,5), #ylim=myrange,
         xaxt="n", yaxt="n", pch=19, col=mycolours[3], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE, slty=3)
  plotCI(f$or_bladder.2, y=xseq+1.5, liw=f$l95_bladder.2, uiw=f$u95_bladder.2, 
         xlim=c(0,5), #ylim=myrange,
         xaxt="n", yaxt="n", pch=4, col=mycolours[4], lwd=1, cex=mycex, sfrac=0.001,
         ylab="",xlab="", add=TRUE, slty=4)
  axis(side=1, at=c(seq(0,8, by=0.25)), cex.axis=0.7)
  mtext(side=1, text="Odds Ratio", line=2, cex.lab=0.7)
  abline(v=1, lty=2)
  for (k in 1:length(xseq)){
    mytext=mynames
    axis(side=2, at=xseq[k], labels=mytext[k], las=2, cex.axis=0.7)
  }
  legend("topright", pch = c(mypch, 15, 19, 4), col = mycolours, 
         legend = c("lung + bmi,age,sex","lung + bmi,age,sex,smoking","bladder + bmi,age,sex","bladder + bmi,age,sex,smoking"))

  dev.off()}


#  abline(v=xseqgreysep,lty=3, col="grey")
#  xseqgrey=c(xseq[!duplicated(myvariables)]-myspacing/2, max(xseq)+myspacing/2)
#  tmpseq=xseqgrey
#  for (k in 1:(length(tmpseq)-1)){
#    if (!is.na(mynames)[!duplicated(myvariables)][k]){
#      axis(side=1, at=tmpseq[c(k,k+1)]+c(2,-2), line=7, labels=NA, cex=0.7)
#    }
#  }
# for (k in 1:(length(tmpseq)-1)){
#   if (!is.na(mynames)[!duplicated(myvariables)][k]){
#     mytmp=myvariables[!duplicated(myvariables)]
#     axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.7,
#          labels=mytmp[k], las=2)
#   }
# }
#  for (k in 1:(length(tmpseq)-1)){
#    if (!is.na(mynames)[!duplicated(myvariables)][k]){
# mytmp=eval(parse(text=paste0("expression(atop('",paste(strsplit(myvariables[!duplicated(myvariables)][k], split = "^", fixed = TRUE)[[1]], collapse="'^'"), "'))")))
#      mytext=paste(strsplit(myvariables[!duplicated(myvariables)][k], split = "^", fixed = TRUE)[[1]], collapse="'^'")
##      if (grepl("\n", mytext, fixed=TRUE)){
#        mytext=paste0("'",paste(strsplit(mytext, split="\n", fixed=TRUE)[[1]],collapse="','"),"'")
#        mytmp=eval(parse(text=paste0("expression(atop(", mytext, "))")))
#      } else {
#        mytmp=mytext
#      }
#      axis(side=1, at=mean(tmpseq[c(k,k+1)]), line=6.7, tick=FALSE, cex.axis=0.7,
#           labels=mytmp, las=2)
#    }
# }
#  xseqblack=c(xseq[!duplicated(mynames)]-myspacing/2, max(xseq)+myspacing/2)
#  abline(v=xseqblack,lty=3,col="black")
#  for (k in 1:(length(xseqblack)-1)){
#    axis(side=3, at=xseqblack[c(k,k+1)]+c(1,-1), line=0.5, labels=NA)
#  }
#  for (k in 1:(length(xseqblack)-1)){
#    axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE, labels=unique(variable_cat)[k], cex.axis=1.2)
#  }

### AGE DIAG SENSITIVITY ANALYSIS PLOTS ----

fad = readRDS("../Results/forest_plot_agediag.rds")
fad <- as.data.frame(fad)
fgroups <- c(rep("health risk",7),rep("sociodemographic", 16), rep("health risk", 21), rep("environment", 4), rep("health risk", 5),rep("environment", 7),rep("medical", 16), rep("biomarkers", 28))
model<-c(rep(c("lung + bmi,age,sex","lung + bmi,age,sex,smoking","bladder + bmi,age,sex","bladder + bmi,age,sex,smoking"),98))
cancer <-c(rep(c(rep("lung",2),rep("bladder",2)),98))
fad$group <- fgroups
order <- c("group","or_lung_old.1","l95_lung_old.1","u95_lung_old.1","logp_lung_old.1","or_lung_old.2",       
           "l95_lung_old.2","u95_lung_old.2","logp_lung_old.2","or_lung_young.1","l95_lung_young.1",    
           "u95_lung_young.1","logp_lung_young.1","or_lung_young.2","l95_lung_young.2","u95_lung_young.2",    
           "logp_lung_young.2","or_bladder_old.1","l95_bladder_old.1","u95_bladder_old.1","logp_bladder_old.1",  
           "or_bladder_old.2","l95_bladder_old.2","u95_bladder_old.2","logp_bladder_old.2","or_bladder_young.1",  
           "l95_bladder_young.1","u95_bladder_young.1","logp_bladder_young.1","or_bladder_young.2","l95_bladder_young.2", 
           "u95_bladder_young.2","logp_bladder_young.2")
fad <- fad[, order]

f.or <- f %>%
  select(group,or_lung.1, or_lung.2, or_bladder.1, or_bladder.2) %>%
  tibble::rownames_to_column(var = "covariate") %>%
  pivot_longer(cols=c(or_lung.1, or_lung.2, or_bladder.1, or_bladder.2),
               names_to="model", 
               values_to="or")%>%
  arrange(group,covariate)

f.l95 <- f %>%
  select(group,l95_lung.1, l95_lung.2, l95_bladder.1, l95_bladder.2) %>%
  tibble::rownames_to_column(var = "covariate") %>%
  pivot_longer(cols=c(l95_lung.1, l95_lung.2, l95_bladder.1, l95_bladder.2),
               names_to="model", 
               values_to="l95")%>%
  arrange(group,covariate)

f.u95 <- f %>%
  select(group,u95_lung.1, u95_lung.2, u95_bladder.1, u95_bladder.2) %>%
  tibble::rownames_to_column(var = "covariate") %>%
  pivot_longer(cols=c(u95_lung.1, u95_lung.2, u95_bladder.1, u95_bladder.2),
               names_to="model", 
               values_to="u95")%>%
  arrange(group,covariate)

f.logp <- f %>%
  select(group,logp_lung.1, logp_lung.2, logp_bladder.1, logp_bladder.2) %>%
  tibble::rownames_to_column(var = "covariate") %>%
  pivot_longer(cols=c(logp_lung.1, logp_lung.2, logp_bladder.1, logp_bladder.2),
               names_to="model", 
               values_to="logp") %>%
  arrange(group,covariate)

f.logp$model <- model
f.l95$model <- model
f.u95$model <- model
f.or$model <- model


f.logp$cancer <- cancer
f.l95$cancer <- cancer
f.u95$cancer <- cancer
f.or$cancer <- cancer

forest<-right_join(f.logp, f.or, by=c("model","covariate","group","cancer")) %>%
  inner_join(f.u95) %>%
  inner_join(f.l95) 
saveRDS(forest, "../Results/forest_wk8.rds")

pdf("../Figures/forest_wk8_zoom.pdf", height = 10, width = 15) # increase width so you can read all covariate names
par(mar = c(13, 5, 3, 5)) 
g <- ggplot(forest, aes(y = covariate, x = or, xmin = l95, xmax = u95, color=factor(group))) +
  geom_point() +
  geom_errorbarh(height = .1) +
  scale_x_continuous(limits=c(0,2),breaks=c(0,.5,1,1.5,2))+
  geom_vline(xintercept=1, color="grey60",linetype="dashed")+
  facet_grid(group~model, scales = "free", space = "free") +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0)) +
  theme(axis.text.y = element_text(color=ifelse(forest$logp[forest$model=="lung + bmi,age,sex"] > bonf & forest$logp[forest$model=="lung + bmi,age,sex,smoking"] > bonf &
                                                  forest$logp[forest$model=="bladder + bmi,age,sex"] > bonf & forest$logp[forest$model=="bladder + bmi,age,sex,smoking"] > bonf, 
                                                yes = MyPal[6], no = "black")))  # axis labels
g
dev.off()







