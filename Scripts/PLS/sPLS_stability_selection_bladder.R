
### TDS Project -- sPLS stability selection on bladder (some plots + extract loading)
## Programme created by Vivian on 8 March

rm(list=ls())

source("template/penalisation_functions.R")
source("template/functions.R")


##### load data
bladder.1 <- readRDS("../../Results/denoised/bladder.1_denoised.rds")
bladder.2 <- readRDS("../../Results/denoised/bladder.2_denoised.rds")


##### Sparse PLS-DA--

X1 <-  bladder.1[, -1]
X1 <- cbind(X1[,5:46], X1[,1:4], X1[, 47:98])
Y1 <-  bladder.1[, 1]

t0=Sys.time()
out1=CalibrateRegression(xdata = X1, ydata = Y1, Lambda=1:ncol(X1), family="binomial", 
                         implementation="SparsePLS", K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(out1, "../../Results/sPLS_stability_selection_bladder.1.rds")
out1 <- readRDS("../../Results/PLS/sPLS_stability_selection_bladder.1.rds")


##### Parameters calibrated by stability--
##### number of variables and threshold in selection proportion

#print(GetArgmax(out1)) 
hat_params1 = GetArgmax(out1)

## Calibrated selection proportions

#print(CalibratedSelectionProportionsRegression(out))
selprop1 <- CalibratedSelectionProportionsRegression(out1, plot=FALSE)

## Stably selected variables
A1 = CalibratedStableRegression(out1)


##### Visualisation of selection proportions--
png(filename = "selection_proportions_bladder.1.png", width = 1200, height = 900)
par(mar=c(15,5,1,1))
mycolours=ifelse(selprop1 >= (1-hat_params1[2]), yes=1, no=0) + ifelse(selprop1 >= hat_params1[2], yes=1, no=0)
plot(selprop1, type="h", lwd=2, xaxt="n", las=1, ylim=c(0,1),
     xlab="", ylab="Selection Proportion", cex.lab=1.5,
     col=c("darkgrey", "skyblue2", "royalblue4")[mycolours+1])
abline(h=hat_params1[2], col="red", lty=2)
abline(h=1-hat_params1[2], col="red", lty=2)
for (i in 1:length(selprop1)){
  axis(side=1, at=i, labels=names(selprop1)[i], las=2, font = 2,
       col=ifelse(selprop1[i]>= hat_params1[2], yes="royalblue4", ifelse(selprop1[i]  >= (1-hat_params1[2]), yes="skyblue2", no="grey")), 
       col.axis=ifelse(selprop1[i]>= hat_params1[2], yes="royalblue4", ifelse(selprop1[i]  >= (1-hat_params1[2]), yes="skyblue2", no="grey")))
}
dev.off()


##### Calibration plot--
pdf("../../PLS_plots/sPLS_CalibrationPlot_bladder.1.pdf", width=10, height=7)
CalibrationPlot(out1) # 2-d version with lambda on the X-axis and threshold on the Y-axis
dev.off()

png(filename = "../../PLS_plots/Cali_plot_bladder.1.png", width = 1200, height = 900)
CalibrationPlot(out1, bi_dim=FALSE) # stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
dev.off()


##### Visualising selection proportion paths--
stab_mat1=out1$selprop[,sort.list(apply(out1$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat1)=1:nrow(stab_mat1)
pheatmap(stab_mat1, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
pheatmap(stab_mat1, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, 
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/sPLS_heatmap_selection_bladder.1.pdf")
dev.off()


##### Checking consistency in sign of the loading coefficients for the variables with high selprop
hat_lambda_id1 <- GetArgmaxId(out1)[1]
selprop1 <- CalibratedSelectionProportionsRegression(out1)
a1 <- apply(out1$Beta[hat_lambda_id1,,],1,FUN=function(x){sum(x>0)})
b1 <- apply(out1$Beta[hat_lambda_id1,,],1,FUN=function(x){sum(x<0)})

a1 <- data.frame(Var = names(a1), a1)
b1 <- data.frame(Var = names(b1), b1)
selprop1 <- data.frame(Var = names(selprop1), selprop1)
check1 <- merge(a1, b1, by = "Var") %>%
  merge(selprop1, by = "Var") %>%
  mutate(pos_b = a1/(a1+b1))

pdf("../../PLS_plots/sPLS_check_selprop_bladder.1.pdf", width=10, height=7)
ggplot(check1, aes(pos_b, selprop1,
                    label = ifelse((selprop1 >= 0.9 & pos_b>0.4 & pos_b<0.6),
                                   paste0(gsub(":", ":\n", Var),"\n",signif(abs(myloadings1),3)),""))) +
  geom_point(color = ifelse((check1$selprop1>=0.9 & check1$pos_b>0.4 & check1$pos_b<0.6),"tomato","navy"),
             position = "jitter") +
  geom_text_repel(box.padding = 0.5, max.overlaps = Inf) +
  labs(title = "Base model")+
  xlab("Proportion of positive loading coefficient among loading coefficients")+
  ylab("Selection Proportion") +
  theme_bw()
dev.off()


##### Extract loadings--
hat_pi1=GetArgmax(out1)[2]
myselprop1 = out1$selprop[which.max(out1$S),]
myloadings1 = out1$Beta[which.max(out1$S),,]
myloadings1 = apply(myloadings1,1,FUN=function(x){mean(x[x!=0])})
myloadings1[is.na(myloadings1)] <- 0
myloadings1[which(!(names(myloadings1) %in% names(myselprop1)[myselprop1>=hat_pi1]))] <- 0

plot(myloadings1)
     
saveRDS(myloadings1, "loadings_bladder_1.rds")




##### Sparse PLS-DA--

X2 <- bladder.2[, -1]
Y2 <- bladder.2[, 1]

t0=Sys.time()
out2 = CalibrateRegression(xdata = X2, ydata = Y2, Lambda=1:ncol(X2), family="binomial", 
                           implementation="SparsePLS", K=100)
t1=Sys.time()
print(as.numeric(difftime(t1,t0, units="secs")))

saveRDS(out2, "../../Results/sPLS_stability_selection_bladder.2.rds")
out2 <- readRDS("../../Results/PLS/sPLS_stability_selection_bladder.2.rds")


##### Parameters calibrated by stability--
##### number of variables and threshold in selection proportion

#print(GetArgmax(out2)) 
hat_params2 = GetArgmax(out2)

## Calibrated selection proportions

#print(CalibratedSelectionProportionsRegression(out))
selprop2 <- CalibratedSelectionProportionsRegression(out2, plot=FALSE)

## Stably selected variables
A2 = CalibratedStableRegression(out2)


##### Visualisation of selection proportions--
png(filename = "selection_proportions_bladder.2.png", width = 1200, height = 900)
par(mar=c(14,5,1,1))
mycolours=ifelse(selprop2 >= (1-hat_params2[2]), yes=1, no=0) + ifelse(selprop2 >= hat_params2[2], yes=1, no=0)
plot(selprop2, type="h", lwd=2, xaxt="n", las=1, ylim=c(0,1),
     xlab="", ylab="Selection Proportion", cex.lab=1.5,
     col=c("darkgrey", "skyblue2", "royalblue4")[mycolours+1])
abline(h=hat_params2[2], col="red", lty=2)
abline(h=1-hat_params2[2], col="red", lty=2)
for (i in 1:length(selprop2)){
  axis(side=1, at=i, labels=names(selprop2)[i], las=2, font = 2,
       col=ifelse(selprop2[i]>= hat_params2[2], yes="royalblue4", ifelse(selprop2[i]  >= (1-hat_params2[2]), yes="skyblue2", no="grey")), 
       col.axis=ifelse(selprop2[i]>= hat_params2[2], yes="royalblue4", ifelse(selprop2[i]  >= (1-hat_params2[2]), yes="skyblue2", no="grey")))
}
dev.off()


##### Calibration plot--
pdf("../../PLS_plots/sPLS_CalibrationPlot_bladder.2.pdf", width=10, height=7)
CalibrationPlot(out2) # 2-d version with lambda on the X-axis and threshold on the Y-axis
dev.off()

png(filename = "../../PLS_plots/Cali_plot_bladder.2.png", width = 1200, height = 900)
CalibrationPlot(out2, bi_dim=FALSE) # stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
dev.off()


##### Visualising selection proportion paths--
stab_mat2=out2$selprop[,sort.list(apply(out2$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat2)=1:nrow(stab_mat2)
pheatmap(stab_mat2, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
pheatmap(stab_mat2, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, 
         height = 30, width = 60, fontsize = 25, 
         filename = "../../PLS_plots/sPLS_heatmap_selection_bladder.2.pdf")
dev.off()


##### Checking consistency in sign of the loading coefficients for the variables with high selprop
hat_lambda_id2 <- GetArgmaxId(out2)[1]
selprop2 <- CalibratedSelectionProportionsRegression(out2, plot = F)
a2 <- apply(out2$Beta[hat_lambda_id2,,],1,FUN=function(x){sum(x>0)})
b2 <- apply(out2$Beta[hat_lambda_id2,,],1,FUN=function(x){sum(x<0)})

a2 <- data.frame(Var = names(a2), a2)
b2 <- data.frame(Var = names(b2), b2)
selprop2 <- data.frame(Var = names(selprop2), selprop2)
check2 <- merge(a2, b2, by = "Var") %>%
  merge(selprop2, by = "Var") %>%
  mutate(pos_b = a2/(a2+b2))

pdf("../../PLS_plots/sPLS_check_selprop_bladder.2.pdf", width=10, height=7)
ggplot(check2, aes(pos_b, selprop2,
                    label = ifelse((selprop2 >= 0.86 & pos_b>0.4 & pos_b<0.6),
                                   paste0(gsub(":", ":\n", Var),"\n",signif(abs(myloadings2),3)),""))) +
  geom_point(color = ifelse((check2$selprop2>=0.86 & check2$pos_b>0.4 & check2$pos_b<0.6),"tomato","navy"),
             position = "jitter") +
  geom_text_repel(box.padding = 0.5, max.overlaps = Inf) +
  labs(title = "Model adjusted on smoking status")+
  xlab("Proportion of positive loading coefficient among loading coefficients")+
  ylab("Selection Proportion") +
  theme_bw()
dev.off()


pdf("../../PLS_plots/sPLS_check_selprop_bladder.2.pdf", width=10, height=7)
par(mar=c(5,5,1,1))
plot(check2$a2/(check2$a2+check2$b2), check2$selprop2, las=1, pch=19, col="navy", cex.lab=1.5,
     xlab="Proportion of positive loading coefficients among non-zero loading coefficients", 
     ylab="Selection Proportion") 
dev.off()


##### Extract loadings--
hat_pi2=GetArgmax(out2)[2]
myselprop2 = out2$selprop[which.max(out2$S),]
myloadings2 = out2$Beta[which.max(out2$S),,]
myloadings2 = apply(myloadings2,1,FUN=function(x){mean(x[x!=0])})
myloadings2[is.na(myloadings2)] <- 0
myloadings2[which(!(names(myloadings2) %in% names(myselprop2)[myselprop2>=hat_pi2]))] <- 0

saveRDS(myloadings2, "loadings_bladder_2.rds")

