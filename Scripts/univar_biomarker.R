
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


##### pre-processing for imputation
### delete people without any data
biomk_ex <- biomk_complete[-which(biomk_complete$NAs == 28),]
biomk_ex <- biomk_ex[,-30]
dim(biomk_ex)   # 490712  29
View(biomk_ex)

### distribution of NAs - for deciding the cut off of exclusion?
biomk_ex$NAs <- apply(biomk_ex[, 2:29], 1, function(x)sum(is.na(x)))
summary(biomk_ex$NAs)
hist(biomk_ex$NAs, breaks = 28)
length(which(biomk_ex$NAs >= 22))  # 80%
## cumulative
na_prop <- prop.table(table(biomk_ex$NAs))
cumsum(na_prop)

par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mfrow = c(1,1))
plot(ecdf(biomk_ex$NAs))

### delete people with more than 9 missing (less than 30% missing per participant)
biomk_8 <- biomk_ex[-which(biomk_ex$NAs >= 9),]
dim(biomk_8)   # 451475     30



##### imputation 
### knn (how to decide parameters?)
imp_knn = impute.knn(as.matrix(t(biomk_21[,2:29])))
imp_knn_data <- data.frame(t(imp_knn[[1]]))
colnames(imp_knn_data)

imp_knn_data <- cbind(biomk_21$eid, imp_knn_data)
colnames(imp_knn_data) <- c("eid", colnames(imp_knn_data[2:29]))

### impute.QRILC 
## included less than 9 missing
set.seed(7)
log_biomk_8 <- t(log(biomk_8[,2:29]))
imp.8 = impute.QRILC(log_biomk_8)
imp_Q_data_8 <- data.frame(t(exp(imp.8[[1]])))
dim(imp_Q_data_8)   # 451475     28
View(imp_Q_data_8)


##### check summary after imputation
View(summary(biomk_ex[,2:29]))
summary(imp_Q_data_8[, 1:4])


##### output cleaned/imputation data
saveRDS(biomk_ex, "../TDS_Group3/Results/biomarkers_cleaned.rds")

imp_Q_data_8 <- cbind(biomk_8$eid, imp_Q_data_8)
colnames(imp_Q_data_8) <- c("eid", colnames(imp_Q_data_8)[2:29])
saveRDS(imp_Q_data_8, "../TDS_Group3/Results/biomarkers_imputation.rds")
View(sort(imp_Q_data_8$eid))


##### univariate (logistic regression w complete cases for each variable) - without imputation
## check no of complete case for each variable
dim(biomk_cc)
com_biomk <- apply(biomk_cc[,11:38], 2, function(x)sum(complete.cases(x)))

## split the data first
lung_cc <- biomk_cc[which(biomk_cc$case_status == "lung" | biomk_cc$case_status == "control"),]
lung_cc$status <- ifelse(lung_cc$case_status == "control", 0, 1)
bladder_cc <- biomk_cc[which(biomk_cc$case_status == "bladder" | biomk_cc$case_status == "control"),]
bladder_cc$status <- ifelse(bladder_cc$case_status == "control", 0, 1)

## get p value (need to check!)
rows <- list(names(lung_cc))
pvals <- data.frame(matrix(rep(NA,56), 28,2))

for (i in 11:38) {
  fit.lung <- glm(status ~ lung_cc[,i], data = lung_cc, family=binomial(link="logit"))
  fit.bladder <- glm(status ~ bladder_cc[,i], data = bladder_cc, family=binomial(link="logit"))
  pvals[i-10, 1] <- coef(summary(fit.lung))[2,4] 
  pvals[i-10, 2] <- coef(summary(fit.bladder))[2,4] 
}

colnames(pvals) <- c("lung", "bladder")
View(pvals)
out_pvals <- as.vector(t(pvals))



##### univariate (logistic regression with stratified and adjusted by age) - without imputation

## combine data
lung_cc <- left_join(lung_cc, demsocial_data, by="eid")
bladder_cc <- left_join(bladder_cc, demsocial_data, by="eid")
colnames(lung_cc)[11:38]

## subset
lung_cc_M <- lung_cc[which(lung_cc$gender == "Male"), ]
lung_cc_F <- lung_cc[which(lung_cc$gender == "Female"), ]
bladder_cc_M <- bladder_cc[which(bladder_cc$gender == "Male"), ]
bladder_cc_F <- bladder_cc[which(bladder_cc$gender == "Female"), ]

## get p value 
pvals_con <- data.frame(matrix(rep(NA,112), 28,4))

for (i in 11:38) {
  fit.lung.M <- glm(status ~ lung_cc_M[,i] + age, data = lung_cc_M, family=binomial(link="logit"))
  fit.lung.F <- glm(status ~ lung_cc_F[,i] + age, data = lung_cc_F, family=binomial(link="logit"))
  fit.bladder.M <- glm(status ~ bladder_cc_M[,i] + age, data = bladder_cc_M, family=binomial(link="logit"))
  fit.bladder.F <- glm(status ~ bladder_cc_F[,i] + age, data = bladder_cc_F, family=binomial(link="logit"))
  pvals_con[i-10, 1] <- coef(summary(fit.lung.M))[2,4] 
  pvals_con[i-10, 2] <- coef(summary(fit.lung.F))[2,4]
  pvals_con[i-10, 3] <- coef(summary(fit.bladder.M))[2,4] 
  pvals_con[i-10, 4] <- coef(summary(fit.bladder.F))[2,4] 
}

colnames(pvals_con) <- c("lung_male", "lung_female", "bladder_male", "bladder_female")
View(pvals_con)
out_pvals_con <- as.vector(t(pvals_con))



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
#pvals_log <- data.frame(matrix(rep(NA,56), 28,2))

for (i in 2:29) {
  fit.lung <- lm(lung_c[,i] ~ status, data = lung_c)
  fit.bladder <- lm(bladder_c[,i] ~ status, data = bladder_c)
  pvals[i-1, 1] <- coef(summary(fit.lung))[2,4] 
  pvals[i-1, 2] <- coef(summary(fit.bladder))[2,4] 
}

colnames(pvals_log) <- c("lung", "bladder")
View(pvals_log)
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
par(mar = c(6.5, 5, 1, 1))
library(RColorBrewer)
mypal = brewer.pal(n = 12, name = "Paired")
colors = colorRampPalette(c("navy", "blue", "skyblue"))(28)
colors = colorRampPalette(mypal)(28)

pvals.1 <- as.vector(t(pvals))
pvals.1 <- cbind(pvals.1, rep(c(1:28), each = 2), rep(c(1:2),28))
pvals.1 <- data.frame(pvals.1)
pvals.1 <- cbind(rep(colnames(lung_c)[2:29], each = 2), pvals.1)
colnames(pvals.1) <- c("biomarker", "pvalue", "annots", "type")

plot(-log10(pvals.1$pvalue), col = colors[pvals.1$annots], pch = pvals.1$type , cex = 0.5, 
     xaxt = "n", ylab = "-log10(P)",
     lwd = 3, xlab = "")
axis(1, at = seq(1.5, 28*2, by =2), labels = colnames(lung_c)[2:29],
     las = 2, cex = 0.7)

abline(v = seq(2.5, 28*2, by =2) , lty = 3, col = "grey")
abline(h = -log10(0.05/(28*2)), lty = 2, col = "darkred")
legend("topright", legend = c("Lung", "Bladder"),
       pch = pvals.1$type, 
       cex = 0.75, bty = "n")


pvals_con.1 <- as.vector(t(pvals_con))
pvals_con.1 <- cbind(pvals_con.1, rep(c(1:28), each = 4), rep(c(1:4),28))
pvals_con.1 <- data.frame(pvals_con.1)
pvals_con.1 <- cbind(rep(colnames(lung_c)[2:29], each = 4), pvals_con.1)
colnames(pvals_con.1) <- c("biomarker", "pvalue", "annots", "type")

  plot(-log10(pvals_con.1$pvalue), col = colors[pvals_con.1$annots], pch = pvals_con.1$type, 
       xaxt = "n", ylab = "-log10(P)", cex = 0.7,
       lwd = 2, xlab = "")
  axis(1, at = seq(2.5, 28*4, by =4), labels = colnames(lung_c)[2:29],
       las = 2,  cex.axis = 0.6)
  
  abline(v = seq(4.5, 28*4, by =4) , lty = 3, col = "grey")
  abline(h = -log10(0.05/(28*2*2)), lty = 2, col = "darkred")
  #abline(h = -log10(0.05/(28)), lty = 2, col = "darkred")
  legend("topright", legend = c("Lung_male", "Lung_female", "Bladder_male", "Bladder_female"),
         pch = pvals_con.1$type,  
         cex = 0.75, bty = "n")


