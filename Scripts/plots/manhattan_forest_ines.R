# manhattan + forest plots, wk 7
# by ines on march 2

# to do:
# update p value plots
# add variables to highlight as vector

# recode forest plot input

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(RColorBrewer)
### Manhattan Plot ----

mydata = readRDS("../Results/manhattan_plot.rds")
mydata<-as.data.frame(mydata)
MyPal = brewer.pal("Paired", n = 12)

bonf=-log10(0.05/72)

### Manhattan plots ----

# Lung
# significant = red (6), not significant = blue (2)
pdf("../Figures/manhattan_lung_wk7.pdf", height = 8, width = 20) # increase width so you can read all covariate names
par(mar = c(13, 5, 3, 3)) # sets the margins of the graphs
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

dev.off()

# Bladder
# significant = orange (8), not significant = green (4)
pdf("../Figures/manhattan_bladder_wk7.pdf", height = 8, width = 20) # increase width so you can read all covariate names
par(mar = c(13, 5, 3, 3)) # sets the margins of the graphs
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
                  sex, age, bmi, & smoking", "Significant for both models", "Not significant for both models"))

dev.off()



### Scatter plot of log p values of lung v. bladder ----

# make vector of group values for mydata
groups <- c("health risk", "demographic", rep("social", 7), rep("health risk", 13), rep("environment", 7), rep("medical", 15), rep("biomarkers", 28))

# add group variable to data
# group the different things together by color
# scale the plots so they make sense

pdf("../Figures/scatter_pval_confounders.pdf", height = 7, width = 10)
par(mar = c(6, 5, 3, 3))
plot((mydata$lung.1), (mydata$bladder.1), main = expression(-Log[10](italic(p))~"values"), pch = 17, 
     col = ifelse(mydata$bladder.1 > bonf & mydata$lung.1 > bonf, yes = MyPal[6], no = MyPal[2]), 
     ylab =expression("Bladder"~-log[10](italic(p))), xlab = expression("Lung"~-log[10](italic(p))), las = 1,
     xlim=c(0,1.25),ylim=c(0,1.25))
abline(coef=c(0,1), col = "red")
abline(v=1,lty=2,col="grey") # add grey boundaries to show x = 1
abline(h=1,lty=2,col="grey") # add grey boundaries to show y = 1
abline(v = (bonf), lty = 3, col = "grey") # add lines to show log bonferroni (3.15)
abline(h = (bonf), lty=3, col="grey") # add lines to show log bonferroni (3.15)
#text((mydata$lung.1), (mydata$bladder.1), labels=ifelse(mydata$bladder.1 > bonf & mydata$lung.1 > bonf, yes = rownames(mydata), no = ""), cex=0.6, font=2)
text((mydata$lung.1), (mydata$bladder.1), labels=rownames(mydata), cex=0.6, font=2)
legend("topright", pch = c(17, 17), col = c(MyPal[6],MyPal[2]), 
       legend = c("Significant for both models", "Not significant for both models"), cex=0.6)

dev.off()

pdf("../Figures/scatter_pval_smoking.pdf", height = 7, width = 10)
par(mar = c(6, 5, 3, 3))
plot((mydata$lung.2), (mydata$bladder.2), main = expression(-Log[10](italic(p))~"values adjusted for smoking"), pch = 17, 
     col = ifelse(mydata$bladder.2 > bonf & mydata$lung.2 > bonf, yes = MyPal[6], no = MyPal[2]), 
     ylab =expression("Bladder"~-log[10](italic(p))), xlab = expression("Lung"~-log[10](italic(p))), las = 1,
     xlim=c(0,1.25),ylim=c(0,1.25))
abline(coef=c(0,1), col = "red")
abline(v=1,lty=2,col="grey") # add grey boundaries to show x = 1
abline(h=1,lty=2,col="grey") # add grey boundaries to show y = 1
abline(v = (bonf), lty = 3, col = "grey")
abline(h = (bonf), lty=3, col="grey")
# text((mydata$lung.2), (mydata$bladder.2), labels=ifelse(mydata$bladder.2 > bonf & mydata$lung.2 > bonf, yes = rownames(mydata), no = ""), cex=0.6, font=2)
text((mydata$lung.2), (mydata$bladder.2), labels=rownames(mydata), cex=0.6, font=2)
legend("topright", pch = c(17, 17), col = c(MyPal[6],MyPal[2]), 
       legend = c("Significant for both models", "Not significant for both models"), cex=0.6)

dev.off()

### Forest plot comparing odds ratio and per category group ----

f = readRDS("../Results/forest_plot.rds")
f <- as.data.frame(f)

### data processing forest ----
fgroups <- c(rep("Health behaviour",4),rep("Socio-demographic", 18), rep("Health behaviour", 24), rep("Environment", 7), rep("Medical", 17), rep("Biomarkers", 28))
model<-c(rep(c("lung.1","lung.2","bladder.1","bladder.2"),98))
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
saveRDS(forest, "../Results/forest.rds")
### forest plot ----

# I want Y to be the covariates - which are the rownames
# x is the OR, xmin - lower bound of confidence interval, xmax - upper bound of confidence interval
# shape will probably be lung vs. bladder
# i could have another grouping factor that is .1 vs. .2

MyPal2 = brewer.pal("Spectral", n = 11)


# horizontal
pdf("../Figures/forest_wk7_h_cancergrouped.pdf", height = 10, width = 15) # increase width so you can read all covariate names
par(mar = c(13, 5, 3, 5)) 
g <- ggplot(forest, aes(y = covariate, x = or, xmin = l95, xmax = u95, color=factor(group), shape=model)) +
  geom_point() +
  geom_errorbarh(height = .1) +
  scale_x_continuous(limits=c(0,2),breaks=c(0,.5,1,1.5,2))+
  geom_vline(xintercept=1, color="grey60",linetype="dashed")+
  facet_grid(group~cancer, scales = "free", space = "free") +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0)) +
  theme(axis.text.y = element_text(color=ifelse(forest$logp[forest$model=="lung.1"] > bonf & forest$logp[forest$model=="lung.2"] > bonf &
                                                  forest$logp[forest$model=="bladder.1"] > bonf & forest$logp[forest$model=="bladder.2"] > bonf, 
                                                  yes = MyPal[6], no = "black")))  # axis labels
g
dev.off()

# ROTATE COVARIATE NAMES
# rotated
pdf("../Figures/forest_wk7.pdf", height = 8, width = 15) # increase width so you can read all covariate names

par(mar = c(13, 5, 3, 3)) 
p <- ggplot(forest, aes(y = or, ymin = l95, ymax = u95, x= covariate, color=factor(group))) +
     geom_point() +
     geom_errorbar(height = .1) +
     scale_y_continuous(limits=c(0,2),breaks=c(0,.5,1,1.5,2))+
     geom_hline(yintercept=1, color="grey60",linetype="dashed")+
     facet_grid(model~group, scales = "free", space = "free") +
     theme_minimal() +
     theme(strip.text.x = element_text(angle = 0)) + # group labels on top of graph
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, 
                                      color=ifelse(forest$logp[forest$model=="lung.1"] > bonf & forest$logp[forest$model=="lung.2"] > bonf &
                                                   forest$logp[forest$model=="bladder.1"] > bonf & forest$logp[forest$model=="bladder.2"] > bonf, 
                                                   yes = MyPal[6], no = "black")))  # axis labels

p
dev.off()