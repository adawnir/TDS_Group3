
### TDS Project -- sPLS (using parameters from stability selection)
## Programme created by Vivian 

rm(list=ls())

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("mixOmics")
library(mixOmics)
install.packages('sgPLS', dependencies = T)

suppressPackageStartupMessages(library(sgPLS))
suppressPackageStartupMessages(library(RColorBrewer))

source("../pls_functions.R")

MyPal = brewer.pal("Paired", n = 12) 
MyPalPairsExt <- colorRampPalette(MyPal)(100)


##### dataset
lung.1 <- readRDS("../Results/denoised/lung.1_denoised.rds")
lung.2 <- readRDS("../Results/denoised/lung.2_denoised.rds")
bladder.1 <- readRDS("../Results/denoised/bladder.1_denoised.rds")
bladder.2 <- readRDS("../Results/denoised/bladder.2_denoised.rds")


##### sPLS---

### model 1
## bladder: 22 from stability selection
x_bladder.1 <- bladder.1[,-1]
x_bladder.1 <- cbind(x_bladder.1[,5:47], x_bladder.1[,1:4], x_bladder.1[, 48:98])
y_bladder.1 <- bladder.1[,1]

MysPLSDA_bladder.1 <- splsda(x_bladder.1, y_bladder.1, ncomp = 1, 
                             keepX = 22)
MysPLSDA_bladder.1$loadings$X[MysPLSDA_bladder.1$loadings$X != 0, ]
MysPLSDA_bladder.1$loadings$Y
MysPLSDA_bladder.1$explained_variance

## plot for loadings
Load_bladder.1 = cbind(MysPLSDA_bladder.1$loadings$X[MysPLSDA_bladder.1$loadings$X != 0, ], rep(NA, 22))
Load_bladder.1 = as.vector(t(Load_bladder.1))
Load_bladder.1 = Load_bladder.1[-length(Load_bladder.1)]

png("../../PLS_plots/sPLS_loading_bladder_1.png", width = 800, height = 675)
par(mar = c(14, 5, 6.5, 3))
plot(Load_bladder.1, col =c(rep(c(MyPal[6],NA),6), rep(c(MyPal[8],NA),6), rep(c(MyPal[4],NA),1), 
                            rep(c(MyPal[2],NA),2), rep(c(MyPal[10],NA),7)),
     xaxt = "n", ylab = "Loadings Coefficients", type = "h",
     lwd = 4, xlab = "", main = "sPLS on Bladder adjusted for age, sex, and BMI")
abline(h = 0, lty = 2)
abline(v = c(0, 6, 12, 13, 15) * 2, lty = 3, col = "black")
axis(1, at = seq(1, 22 * 2, by = 2), labels = names(MysPLSDA_bladder.1$loadings$X[MysPLSDA_bladder.1$loadings$X != 0, ]),
     las = 2, cex.axis = 0.8, font.axis=2)
axis(3, at = c(0, 6, 12, 13, 15, 22) * 2, line = 0.5, labels = NA)
axis(3, at = c(3, 9, 12.2, 14.4, 20)*2 , 
     labels = c("Social", "Health risk", "Environment", "Medical", "Biomarker"), 
     line = 0.3, tick = F, lwd = 1)
legend("topleft", legend = c("Lambda = 22"), bty = 'n', cex = 0.8, text.font=2)
dev.off() 

## stability analysis
set.seed(1)
Stab_bladder.1 = StabilityPlot(X = x_bladder.1, Y =  y_bladder.1, NIter = 100)


## lung: 36 from stability selection
x_lung.1 <- lung.1[,-1]
x_lung.1 <- cbind(x_lung.1[,5:46], x_lung.1[,1:4], x_lung.1[, 47:98])
y_lung.1 <- lung.1[,1]

MysPLSDA_lung.1 <- splsda(x_lung.1, y_lung.1, ncomp = 1, 
                             keepX = 36)
MysPLSDA_lung.1$loadings$X[MysPLSDA_lung.1$loadings$X != 0, ]
MysPLSDA_lung.1$loadings$Y
MysPLSDA_lung.1$explained_variance

## plot for loadings
Load_lung.1 = cbind(MysPLSDA_lung.1$loadings$X[MysPLSDA_lung.1$loadings$X != 0, ], rep(NA, 36))
Load_lung.1 = as.vector(t(Load_lung.1))
Load_lung.1 = Load_lung.1[-length(Load_lung.1)]

png("../../PLS_plots/sPLS_loading_lung_1.png", width = 800, height = 675)
par(mar = c(14, 5, 6.5, 3))
plot(Load_lung.1, col =c(rep(c(MyPal[6],NA),10), rep(c(MyPal[8],NA),5), rep(c(MyPal[4],NA),4), 
                            rep(c(MyPal[2],NA),4), rep(c(MyPal[10],NA),13)),
     xaxt = "n", ylab = "Loadings Coefficients", type = "h",
     lwd = 4, xlab = "", main = "sPLS on lung adjusted for age, sex, and BMI")
abline(h = 0, lty = 2)
abline(v = c(0, 10, 15, 19, 23) * 2, lty = 3, col = "black")
axis(1, at = seq(1, 36 * 2, by = 2), labels = names(MysPLSDA_lung.1$loadings$X[MysPLSDA_lung.1$loadings$X != 0, ]),
     las = 2, cex.axis = 0.8, font.axis=2)
axis(3, at = c(0, 10, 15, 19, 23, 36) * 2, line = 0.5, labels = NA)
axis(3, at = c(5, 12.5, 17, 21, 30)*2 , 
     labels = c("Social", "Health risk", "Environment", "Medical", "Biomarker"), 
     line = 0.3, tick = F, lwd = 1)
legend("bottomleft", legend = c("Lambda = 36"), bty = 'n', cex = 0.8, text.font=2)
dev.off() 




### model 2--
## bladder: 26 from balanced dataset
x_bladder.2 <- bladder.2[,-1]
y_bladder.2 <- bladder.2[,1]

MysPLSDA_bladder.2 <- splsda(x_bladder.2, y_bladder.2, ncomp = 1, 
                             keepX = 26)
MysPLSDA_bladder.2$loadings$X[MysPLSDA_bladder.2$loadings$X != 0, ]
MysPLSDA_bladder.2$loadings$Y
MysPLSDA_bladder.2$explained_variance

## plot for loadings
Load_bladder.2 = cbind(MysPLSDA_bladder.2$loadings$X[MysPLSDA_bladder.2$loadings$X != 0, ], rep(NA, 26))
Load_bladder.2 = as.vector(t(Load_bladder.2))
Load_bladder.2 = Load_bladder.2[-length(Load_bladder.2)]


png("../../PLS_plots/sPLS_loading_bladder_2.png", width = 800, height = 675)
par(mar = c(14, 5, 6.5, 3))
plot(Load_bladder.2, col =c(rep(c(MyPal[6],NA),6), rep(c(MyPal[8],NA),2), rep(c(MyPal[4],NA),3), 
                            rep(c(MyPal[2],NA),3), rep(c(MyPal[10],NA),12)),
     xaxt = "n", ylab = "Loadings Coefficients", type = "h",
     lwd = 4, xlab = "", main = "sPLS on Bladder adjusted for age, sex, BMI and smoking")
abline(h = 0, lty = 2)
abline(v = c(0, 6, 8, 11, 14) * 2, lty = 3, col = "black")
axis(1, at = seq(1, 26 * 2, by = 2), labels = names(MysPLSDA_bladder.2$loadings$X[MysPLSDA_bladder.2$loadings$X != 0, ]),
     las = 2, cex.axis = 0.8, font.axis=2)
axis(3, at = c(0, 6, 8, 11, 14, 26) * 2, line = 0.5, labels = NA)
axis(3, at = c(3, 7, 10, 12.7, 20)*2 , 
     labels = c("Social", "Health risk", "Environment", "Medical", "Biomarker"), 
     line = 0.3, tick = F, lwd = 1)
legend("topleft", legend = c("Lambda = 26"), bty = 'n', cex = 0.8, text.font=2)
dev.off() 

## stability analysis
set.seed(1)
Stab_bladder.2 = StabilityPlot(X = x_bladder.2, Y =  y_bladder.2, NIter = 100)


## lung: 38 from stability selection
x_lung.2 <- lung.2[,-1]
y_lung.2 <- lung.2[,1]

MysPLSDA_lung.2 <- splsda(x_lung.2, y_lung.2, ncomp = 1, 
                          keepX = 38)
MysPLSDA_lung.2$loadings$X[MysPLSDA_lung.2$loadings$X != 0, ]
MysPLSDA_lung.2$loadings$Y
MysPLSDA_lung.2$explained_variance

## plot for loadings
Load_lung.2 = cbind(MysPLSDA_lung.2$loadings$X[MysPLSDA_lung.2$loadings$X != 0, ], rep(NA, 38))
Load_lung.2 = as.vector(t(Load_lung.2))
Load_lung.2 = Load_lung.2[-length(Load_lung.2)]

png("../../PLS_plots/sPLS_loading_lung_2.png", width = 800, height = 675)
par(mar = c(12, 5, 6.5, 3))
plot(Load_lung.2, col =c(rep(c(MyPal[6],NA),10), rep(c(MyPal[8],NA),3), rep(c(MyPal[4],NA),4), 
                         rep(c(MyPal[2],NA),6), rep(c(MyPal[10],NA),15)),
     xaxt = "n", ylab = "Loadings Coefficients", type = "h",
     lwd = 4, xlab = "", main = "sPLS on lung adjusted for age, sex, BMI and smoking")
abline(h = 0, lty = 2)
abline(v = c(0, 10, 13, 17, 23) * 2, lty = 3, col = "black")
axis(1, at = seq(1, 38 * 2, by = 2), labels = names(MysPLSDA_lung.2$loadings$X[MysPLSDA_lung.2$loadings$X != 0, ]),
     las = 2, cex.axis = 0.8, font.axis=2)
axis(3, at = c(0, 10, 13, 17, 23, 38) * 2, line = 0.5, labels = NA)
axis(3, at = c(5, 11, 15.5, 20, 30)*2 , 
     labels = c("Social", "Health risk", "Environment", "Medical", "Biomarker"), 
     line = 0.3, tick = F, lwd = 1)
legend("topleft", legend = c("Lambda = 38"), bty = 'n', cex = 0.8, text.font=2)
dev.off() 




## plot for loadings together
a <- data.frame(MysPLSDA_bladder.1$loadings$X[MysPLSDA_bladder.1$loadings$X != 0, ])
b <- data.frame(MysPLSDA_bladder.2$loadings$X)
b <- b[row.names(b) %in% row.names(a),]
b <- c(b[1:9], rep(0,4), b[10:24])

Load_bladder = cbind(a, b, rep(NA, 28), rep(NA, 28))
Load_bladder = as.vector(t(Load_bladder))
Load_bladder = Load_bladder[-c(length(Load_bladder) - 1, length(Load_bladder))]

bladder_group <- c(7, 13, 14, 18)

png("/rds/general/user/cl420/home/TDS/PLS_plot/sPLS_bladder.png", width = 1200, height = 675)
par(mar = c(15, 5, 3, 3))
plot(Load_bladder, col = c(rep(c(MyPal[5], MyPal[6], NA, NA),7), rep(c(MyPal[7],MyPal[8], NA, NA),6), 
      rep(c(MyPal[3],MyPal[4], NA,NA),1), rep(c(MyPal[1],MyPal[2], NA,NA),4), 
     rep(c(MyPal[9],MyPal[10], NA,NA),10)),
     xaxt = "n", ylab = "Loadings Coefficients", type = "h",
     lwd = 4, xlab = "", main = "sPLS on bladder")
axis(1, at = seq(1.5, 28 * 4, by = 4), labels = row.names(a), las = 2) 
axis(1, at = c(3, 9.5, 13.5, 16 ,22.5) * 4, 
     labels = c("Social", "Health risk", "Environment", "Medical", "Biomarker"), 
     line = 13, tick = T, lwd = 2, font.axis=2)
abline(v = c(0, bladder_group, 28) * 4, lty = 3, col = "black")
abline(h = 0, lty = 2)
legend("bottomleft", legend = c("adjusted for age, sex, and BMI", "adjusted for age, sex, BMI and smoking"),
       lty = 1, lwd = 3, col = c(MyPal[5], MyPal[6]), bty = "n")
dev.off() 


