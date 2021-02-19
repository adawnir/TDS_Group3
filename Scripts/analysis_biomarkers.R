
##########################     biomarkers  analysis      ##########################
# written by Vivian on 16th feb

### read dataset
biomk_F <- readRDS("../TDS_Group3/Results/extract_biomarkers.rds")

### check case control data
cc_data <- readRDS("../TDS_Group3/Results/case_control.rds")
dim(cc_data)  
View(cc_data)

cc_data.1 <- readRDS("../TDS_Group3/Results/case_control.rds")
dim(cc_data.1)

## tables
table(cc_data$case_status)


###### check people with all NA in biomarkers

## select people exist in case-control dataset 
biomk_complete <- biomk_F[which(biomk_F$eid %in% cc_data$eid),]
dim(biomk_complete)

## exclude two biomarkers with high missing rate
## Oestradiol/Rheumatoid_factor
excluded <- c("Oestradiol", "Rheumatoid factor")
biomk_complete <- biomk_F[, ! colnames(biomk_F) %in% excluded]

## no of NA
biomk_complete$NAs <- apply(biomk_complete, 1, function(x)sum(is.na(x)))
sum(biomk_complete$NAs == 28)
table(biomk_complete$NAs)

## extract eid for those missed every biomarkers
nobiomk <- biomk_complete$eid[which(biomk_complete$NAs == 28)]
nobiomk <- data.frame(nobiomk)
colnames(nobiomk) <- "eid"
write.csv(nobiomk, "../TDS_Group3/Dictionaries/eid_without_biomarkers.csv", row.names = F)

## distribution of NAs - for deciding the cut off of exclusion?
summary(biomk_complete$NAs)
hist(biomk_complete$NAs, breaks = 28)
# cumulative
na_prop <- prop.table(table(biomk_complete$NAs))
cumsum(na_prop)
plot(ecdf(biomk_complete$NAs))


##### combine case-control and biomarkers
## exclude people without any biomarkers (later?)
biomk_excluded <- biomk_complete[-which(biomk_complete$NAs == 28),]
dim(biomk_excluded)

## newdata
biomk_excluded <- biomk_complete

## combination
biomk_cc <- left_join(cc_data, biomk_excluded, by = "eid")
dim(biomk_cc)
View(biomk_cc)
table(biomk_cc$case_status)
levels(biomk_cc$case_status) <- c("control","lung","bladder")


##### descriptive
## mean + sd
mean_sd <- function(x){
  mean_bio <- mean(x, na.rm = T)
  sd_bio <- sd(x, na.rm = T)
  return(c(mean_bio, sd_bio))
}

biomk_des <- aggregate(biomk_cc[,11:38], by = list(biomk_cc$case_status) , FUN=mean_sd)

sd(biomk_cc$`Alanine aminotransferase`[which(biomk_cc$case_status == "lung")], na.rm = T)

## NA
biomk_na_total <- apply(biomk_cc[,11:38], 2, function(x)sum(is.na(x))/nrow(biomk_cc))
biomk_total <- apply(biomk_cc[,11:38], 2, function(x)sum(!is.na(x)))

biomk_na <- aggregate(biomk_cc[,11:38], by = list(biomk_cc$case_status) , function(x)sum(is.na(x)))
biomk_na_p <- aggregate(biomk_cc[,11:38], by = list(biomk_cc$case_status) , 
                        function(x)sum(is.na(x))/nrow(biomk_cc))


##### distribution
## lung VS. controls
par(mfrow = c(1,2))
lapply(colnames(biomk_cc[,2:29]), function(x){
  highestvalue <- max(biomk_cc[,x], na.rm = T)
  a <- hist(biomk_cc[,x][which(biomk_cc$case_status == "lung")], xlim = c(0, highestvalue), plot = F)
  b <- hist(biomk_cc[,x][which(biomk_cc$case_status == "control")], xlim = c(0, highestvalue), plot = F)
  highestDensity <- max(a$density, b$density)
  hist(biomk_cc[,x][which(biomk_cc$case_status == "lung")], main=paste(colnames(biomk_cc[x]), "for lung cases"), 
       xlab = "Value", freq = F, 
       xlim = c(0, highestvalue), ylim = c(0, highestDensity))
  hist(biomk_cc[,x][which(biomk_cc$case_status == "control")], main=paste(colnames(biomk_cc[x]), "for controls"), 
       xlab = "Value", freq = F, 
       xlim = c(0, highestvalue), ylim = c(0, highestDensity))
})

## bladder VS. controls
lapply(colnames(biomk_cc[,2:29]), function(x){
  highestvalue <- max(biomk_cc[,x], na.rm = T)
  a <- hist(biomk_cc[,x][which(biomk_cc$case_status == "bladder")], xlim = c(0, highestvalue), plot = F)
  b <- hist(biomk_cc[,x][which(biomk_cc$case_status == "control")], xlim = c(0, highestvalue), plot = F)
  highestDensity <- max(a$density, b$density)
  hist(biomk_cc[,x][which(biomk_cc$case_status == "bladder")], main=paste(colnames(biomk_cc[x]), "for bladder cases"), 
       xlab = "Value", freq = F, 
       xlim = c(0, highestvalue), ylim = c(0, highestDensity))
  hist(biomk_cc[,x][which(biomk_cc$case_status == "control")], main=paste(colnames(biomk_cc[x]), "for controls"), 
       xlab = "Value", freq = F, 
       xlim = c(0, highestvalue), ylim = c(0, highestDensity))
})

## check
hist(lung_p$Albumin, xlim = c(0,60), ylim = c(0, 0.20), freq = F)
hist(controls$Albumin, xlim = c(0,60), ylim = c(0, 0.20), freq = F)


##### correlation 
install.packages("pheatmap")
suppressPackageStartupMessages(library(pheatmap))

## pairwise.complete.obs
pheatmap(cor(biomk_cc[,2:29], use = "pairwise.complete.obs"), breaks = seq(-1, 1, length.out = 100))


## blocks zoom-in (add exact figure?)
# 1 (urea, creatinine, Cystatin C, Testosterone, Urate)
pheatmap(cor(biomk_F[, c("Urea", "Creatinine", "Cystatin C", "Testosterone", "Urate")], 
             use = "pairwise.complete.obs"), breaks = seq(-1, 1, length.out = 100))


