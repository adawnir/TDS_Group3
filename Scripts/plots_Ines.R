# manhattan plot
# by Ines on Feb 25

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(RColorBrewer)
### Manhattan Plot ----

mydata = read.csv("../Exports/univar_pval_Ines.csv", row.names=1)

MyPal = brewer.pal("Paired", n = 12)
MyPalPairsExt <- colorRampPalette(MyPal)(78) # replace num colors u need

bonf=0.05/78

dir.create("Figures", showWarnings = FALSE)

### Manhattan plots ----

# Lung
# significant = red, not significant = blue
pdf("../Figures/manhattan_lung.pdf", height = 7, width = 20)
par(mar = c(6, 5, 3, 3))
plot(-log10(mydata$lung_confound[5:78]), pch = 17, 
     col = ifelse(mydata$lung_confound[5:78] < bonf, yes = MyPal[6], no = MyPal[2]),
     xaxt = "n", ylab = expression(-log[10](italic(p))), xlab = "",
     las = 1, ylim = c(min(-log10(mydata$lung_confound[5:78])), 
                       max(-log10(mydata$lung_confound[5:78]))))
points(-log10(mydata$lung_confound_smoking[5:78]), pch = 18,
       col=ifelse(mydata$lung_confound_smoking[5:78] < bonf, yes = MyPal[6], no = MyPal[2]))
abline(h = -log10(bonf), col = "red")
abline(v = seq(1, 74), lty = 3, col = "grey")
axis(1, at = 1:74, ifelse(mydata$lung_confound[5:78] < bonf & mydata$lung_confound_smoking[5:78] < bonf,
                          yes = rownames(mydata[5:78,]), no = ""), las = 2, col.axis = MyPal[6])
axis(1, at = 1:74, ifelse(mydata$lung_confound[5:78] < bonf & mydata$lung_confound_smoking[5:78] < bonf,
                          yes = "", no = rownames(mydata[5:78,])), las = 2, col.axis = MyPal[2])
legend("topright", pch = c(17, 18, 19, 19), col = c("black", "black", MyPal[6],MyPal[2]), 
       legend = c("Lung cancer adjusted on sex, age, & bmi", "Lung cancer adjusted on 
                  sex, age, bmi, & smoking", "Significant for both models", "Not significant for both models"), cex = 0.6)

dev.off()

# Bladder
# significant = orange, not significant = green
pdf("../Figures/manhattan_bladder.pdf", height = 7, width = 20)
par(mar = c(6, 5, 3, 3))
plot(-log10(mydata$bladder_confound[4:78]), pch = 17, 
     col = ifelse(mydata$bladder_confound[4:78] < bonf, yes = MyPal[8], no = MyPal[4]), 
     xaxt = "n", ylab = expression(-log[10](italic(p))), xlab = "",
     las = 1, ylim = c(min(-log10(mydata$bladder_confound[4:78])), 
                       max(-log10(mydata$bladder_confound[4:78]))))
points(-log10(mydata$bladder_confound_smoking[5:78]), pch = 18,
       col=ifelse(mydata$bladder_confound_smoking[5:78] < bonf, yes = MyPal[8], no = MyPal[4]))
abline(h = -log10(bonf), col = "red")
abline(v = seq(1, 74), lty = 3, col = "grey")
axis(1, at = 1:74, ifelse(mydata$bladder_confound[5:78] < bonf & mydata$bladder_confound_smoking[5:78] < bonf,
                          yes = rownames(mydata[5:78,]), no = ""), las = 2, col.axis = MyPal[8])
axis(1, at = 1:74, ifelse(mydata$bladder_confound[5:78] < bonf & mydata$bladder_confound_smoking[5:78] < bonf,
                          yes = "", no = rownames(mydata[5:78,])), las = 2, col.axis = MyPal[4])
legend("topright", pch = c(17, 18, 19, 19), col = c("black", "black", MyPal[8],MyPal[4]), 
       legend = c("Bladder cancer adjusted on sex, age, & bmi", "Bladder cancer adjusted on 
                  sex, age, bmi, & smoking", "Significant for both models", "Not significant for both models"), cex=0.6)

dev.off()



### Scatter plot of log p values of lung v. bladder ----

pdf("../Figures/scatter_pval_confounders.pdf", height = 7, width = 10)
par(mar = c(6, 5, 3, 3))
plot(-log10(mydata$lung_confound[5:78]), -log10(mydata$bladder_confound[5:78]), main = "Log P Values", pch = 17, 
     col = ifelse(mydata$bladder_confound[5:78] < bonf & mydata$lung_confound[5:78] < bonf, yes = MyPal[6], no = MyPal[2]), 
     ylab = "Bladder - log10 p values", xlab = "Lung -log10 p values", las = 1)
abline(coef=c(0,1), col = "red")
abline(v = -log10(bonf), lty = 3, col = "grey")
abline(h = -log10(bonf), lty=3, col="grey")
text(-log10(mydata$lung_confound[5:78]), -log10(mydata$bladder_confound[5:78]), labels=ifelse(mydata$bladder_confound[5:78] < bonf & mydata$lung_confound[5:78] < bonf, yes = rownames(mydata), no = ""), cex=0.7, font=2)
legend("topright", pch = c(17, 17), col = c(MyPal[6],MyPal[2]), 
       legend = c("Significant for both models", "Not significant for both models"), cex=0.6)

dev.off()

pdf("../Figures/scatter_pval_smoking.pdf", height = 7, width = 10)
par(mar = c(6, 5, 3, 3))
plot(-log10(mydata$lung_confound_smoking[5:78]), -log10(mydata$bladder_confound_smoking[5:78]), main = "Log P Values for Models adjusted for Smoking", pch = 17, 
     col = ifelse(mydata$bladder_confound_smoking[5:78] < bonf & mydata$lung_confound_smoking[5:78] < bonf, yes = MyPal[6], no = MyPal[2]), 
     ylab = "Bladder - log10 p values", xlab = "Lung -log10 p values", las = 1)
abline(coef=c(0,1), col = "red")
abline(v = -log10(bonf), lty = 3, col = "grey")
abline(h = -log10(bonf), lty=3, col="grey")
text(-log10(mydata$lung_confound_smoking[5:78]), -log10(mydata$bladder_confound_smoking[5:78]), labels=ifelse(mydata$bladder_confound_smoking[5:78] < bonf & mydata$lung_confound_smoking[5:78] < bonf, yes = rownames(mydata), no = ""), cex=0.7, font=2)
legend("topright", pch = c(17, 17), col = c(MyPal[6],MyPal[2]), 
       legend = c("Significant for both models", "Not significant for both models"), cex=0.6)

dev.off()

### Forest plot comparing odds ratio and per category group ----



