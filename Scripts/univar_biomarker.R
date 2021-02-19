
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("pcaMethods")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("impute")


install.packages("pcaMethods")
install.packages("impute")
install.packages("imputeLCMD", dependencies = T)
library(imputeLCMD)

##### imputation 
## delete people without any data
biomk_ex <- biomk_complete[-which(biomk_complete$NAs == 28),]
biomk_ex <- biomk_ex[,-30]
dim(biomk_ex)   # 490712  29
View(biomk_ex)

## distribution of NAs - for deciding the cut off of exclusion?
biomk_ex$NAs <- apply(biomk_ex[, 2:29], 1, function(x)sum(is.na(x)))
summary(biomk_ex$NAs)
hist(biomk_ex$NAs, breaks = 28)
length(which(biomk_ex$NAs >= 22))  # 80%
# cumulative
na_prop <- prop.table(table(biomk_ex$NAs))
cumsum(na_prop)
par(mfrow = c(1,1))
plot(ecdf(biomk_ex$NAs))

## delete people with more than 22 missing for knn (80%)
biomk_21 <- biomk_ex[-which(biomk_ex$NAs >= 22),]
dim(biomk_21)   # 469741     30

## doing imputation
# knn (change parameters?)
imp_knn = impute.knn(as.matrix(t(biomk_21[,2:29])))
imp_knn_data <- data.frame(t(imp_knn[[1]]))
colnames(imp_knn_data)

imp_knn_data <- cbind(biomk_21$eid, imp_knn_data)
colnames(imp_knn_data) <- c("eid", colnames(imp_knn_data[2:29]))

# impute.QRILC (how to decide tune.sigma & error)
imp.1 = impute.QRILC(t(biomk_21[,2:29]), tune.sigma = 2)



##### univariate (linear regression w complete cases for each variable) - without imputation
## check no of complete case for each variable
com_biomk <- apply(biomk_cc[,11:38], 2, function(x)sum(complete.cases(x)))

## split the data first
lung_c <- biomk_cc[which(biomk_cc$case_status == "lung" | biomk_cc$case_status == "control"),]
lung_c$status <- ifelse(lung_c$case_status == "control", 0, 1)
bladder_c <- biomk_cc[which(biomk_cc$case_status == "bladder" | biomk_cc$case_status == "control"),]
bladder_c$status <- ifelse(bladder_c$case_status == "control", 0, 1)

## get p value (need to check!)
rows <- list(names(lung_c))
pvals <- data.frame(matrix(rep(NA,56), 28,2))

for (i in 11:38) {
  fit.lung <- glm(status ~ lung_c[,i], data = lung_c, family=binomial(link="logit"))
  fit.bladder <- glm(status ~ bladder_c[,i], data = bladder_c, family=binomial(link="logit"))
  pvals[i-10, 1] <- coef(summary(fit.lung))[2,4] 
  pvals[i-10, 2] <- coef(summary(fit.bladder))[2,4] 
}

colnames(pvals) <- c("lung", "bladder")
View(pvals)
out_pvals <- as.vector(t(pvals))



##### univariate (linear regression) - with imputation
## imputed data + status
imp_knn_data.1 <- left_join(imp_knn_data, cc_data, by = "eid")
dim(imp_knn_data.1)

## split the data first
lung_c <- imp_knn_data.1[which(imp_knn_data.1$case_status == 1 | imp_knn_data.1$case_status == 0),]
lung_c$status <- factor(ifelse(lung_c$case_status == 0, 0, 1))
bladder_c <- imp_knn_data.1[which(imp_knn_data.1$case_status == 2 | imp_knn_data.1$case_status == 0),]
bladder_c$status <- factor(ifelse(bladder_c$case_status == 0, 0, 1))

## get p value (need to check!)
rows <- list(names(lung_c))
pvals <- data.frame(matrix(rep(NA,56), 28,2))

for (i in 2:29) {
  fit.lung <- lm(lung_c[,i] ~ status, data = lung_c)
  fit.bladder <- lm(bladder_c[,i] ~ status, data = bladder_c)
  pvals[i-1, 1] <- coef(summary(fit.lung))[2,4] 
  pvals[i-1, 2] <- coef(summary(fit.bladder))[2,4] 
}

colnames(pvals) <- c("lung", "bladder")
View(pvals)
out_pvals <- as.vector(t(pvals))

## check
a <- lm(lung_c$Urea ~ status, data = lung_c)
summary(a)

##### univariate (linear regression stratified by sex adjusted by age) - with imputation
## combine data
demsocial_data=readRDS("../TDS_Group3/Results/dems_social.rds")
lung_c <- left_join(lung_c, demsocial_data, by="eid")
bladder_c <- left_join(bladder_c, demsocial_data, by="eid")
colnames(lung_c)[2:29]

## subset
lung_c_M <- lung_c[which(lung_c$gender == "Male"), ]
lung_c_F <- lung_c[which(lung_c$gender == "Female"), ]
bladder_c_M <- bladder_c[which(bladder_c$gender == "Male"), ]
bladder_c_F <- bladder_c[which(bladder_c$gender == "Female"), ]

## model
pvals_con <- data.frame(matrix(rep(NA,112), 28,4))

for (i in 2:29) {
  fit.lung.M <- lm(lung_c_M[,i] ~ status + age, data = lung_c_M)
  fit.lung.F <- lm(lung_c_F[,i] ~ status + age, data = lung_c_F)
  fit.bladder.M <- lm(bladder_c_M[,i] ~ status + age, data = bladder_c_M)
  fit.bladder.F <- lm(bladder_c_F[,i] ~ status + age, data = bladder_c_F)
  pvals_con[i-1, 1] <- coef(summary(fit.lung.M))[2,4] 
  pvals_con[i-1, 2] <- coef(summary(fit.lung.F))[2,4]
  pvals_con[i-1, 3] <- coef(summary(fit.bladder.M))[2,4] 
  pvals_con[i-1, 4] <- coef(summary(fit.bladder.F))[2,4] 
}

colnames(pvals_con) <- c("lung_male", "lung_female", "bladder_male", "bladder_female")
View(pvals_con)
out_pvals_con <- as.vector(t(pvals_con))


## visaulisation as manhattan plot
par(mar = c(6, 5, 2, 2))
library(RColorBrewer)
mypal = brewer.pal(n = 12, name = "Paired")
colors = colorRampPalette(c("navy", "blue", "skyblue"))(28)
colors = colorRampPalette(mypal)(28)

pvals.1 <- as.vector(t(pvals))
pvals.1 <- cbind(pvals.1, rep(c(1:28), each = 2), rep(c(1:2),28))
pvals.1 <- data.frame(pvals.1)
colnames(pvals.1) <- c("pvalue", "annots", "type")

plot(-log10(pvals.1$pvalue), col = colors[pvals.1$annots], pch = pvals.1$type , cex = 0.5, 
     xaxt = "n", ylab = "-log10(P)",
     lwd = 3, xlab = "")
axis(1, at = seq(1.5, 28*2, by =2), labels = colnames(lung_c)[2:29],
     las = 2)

abline(v = seq(2.5, 28*2, by =2) , lty = 3, col = "grey")
abline(h = -log10(0.05/28), lty = 2, col = "darkred")
legend("topright", legend = c("Lung", "Bladder"),
       pch = pvals.1$type, 
       cex = 0.75, bty = "n")


pvals_con.1 <- as.vector(t(pvals_con))
pvals_con.1 <- cbind(pvals_con.1, rep(c(1:28), each = 4), rep(c(1:4),28))
pvals_con.1 <- data.frame(pvals_con.1)
colnames(pvals_con.1) <- c("pvalue", "annots", "type")

plot(-log10(pvals_con.1$pvalue), col = colors[pvals_con.1$annots], pch = pvals_con.1$type, 
     xaxt = "n", ylab = "-log10(P)", cex = 0.7,
     lwd = 2, xlab = "")
axis(1, at = seq(2.5, 28*4, by =4), labels = colnames(lung_c)[2:29],
     las = 2)

abline(v = seq(4.5, 28*4, by =4) , lty = 3, col = "grey")
abline(h = -log10(0.05/28), lty = 2, col = "darkred")
legend("topright", legend = c("Lung_male", "Lung_female", "Bladder_male", "Bladder_female"),
       pch = pvals_con.1$type,  
       cex = 0.75, bty = "n")

