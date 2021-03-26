# Creating the plots for the sensitivity analysis
#Fergal

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)

#Load in the data
age_median_OR <- readRDS('../Results/sensitivity/ageatdiag_median_ORs.rds')
age_median_pval <- readRDS('../Results/sensitivity/ageatdiag_median_pvals.rds')
age_quartile_OR <- readRDS('../Results/sensitivity/ageatdiag_quartile_ORs.rds')
age_quartile_pval <- readRDS('../Results/sensitivity/ageatdiag_quartile_pvals.rds')
time_median_OR <- readRDS('../Results/sensitivity/timetodiag_median_ORs.rds')
time_median_pval <- readRDS('../Results/sensitivity/timetodiag_median_pvals.rds')
time_quartile_OR <- readRDS('../Results/sensitivity/timetodiag_quartile_ORs.rds')
time_quartile_pval <- readRDS('../Results/sensitivity/timetodiag_quartile_pvals.rds')

library(ggplot2)
library(ggrepel)

OR_rownames <- rownames(age_median_OR)
print(OR_rownames)
pval_rownames <- rownames(age_median_pval)
print(pval_rownames)

#Plot naming convention: cancer_model_OR/p_strat_age/time

#Plot for: lung, base model, medians, ORs, age at diag

rownames(age_median_OR) <- 1:nrow(age_median_OR)

age_median_OR$var_group <- NA
age_median_OR$var_group[1:22] <- 'Sociodemographic'
age_median_OR$var_group[23:47] <- 'Health risk'
age_median_OR$var_group[48:55] <- 'Environmental'
age_median_OR$var_group[56:70] <- 'Medical'
age_median_OR$var_group[71:98] <- 'Biomarkers'

dir.create('../Figures/sensitivity')

pdf('../Figures/sensitivity/lung_base_OR_med_age.pdf')
plot1 <- ggplot(age_median_OR, aes(x=young_median_or_lung.1, y=old_median_or_lung.1, 
                color = var_group)) + 
            geom_point(size=2) + geom_text_repel(label=rownames(age_median_OR)) + 
            geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=old_median_l95_lung.1, ymax=old_median_u95_lung.1)) +
            geom_errorbarh(aes(xmin=young_median_l95_lung.1, xmax=young_median_u95_lung.1)) +
            ylim(0, 7.5) + xlim(0, 7.5) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
            ylab('OR (AaD > median)') + xlab('OR (AaD < median)') +
            ggtitle('Lung cancer, base model') + theme_bw()

lung_base_OR_med_age <- plot1 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_base_OR_med_age.pdf')
lung_base_OR_med_age
dev.off()



#Plot for: lung, smoking model, medians, ORs, age at diag

plot2 <- ggplot(age_median_OR, aes(x=young_median_or_lung.2, y=old_median_or_lung.2, 
                                   color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_median_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=old_median_l95_lung.2, ymax=old_median_u95_lung.2)) +
        geom_errorbarh(aes(xmin=young_median_l95_lung.2, xmax=young_median_u95_lung.2)) +
        ylim(0, 4) + xlim(0, 4) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (AaD > median)') + xlab('OR (AaD < median)') +
        ggtitle('Lung cancer, adjusted model') + theme_bw()

lung_smoke_OR_med_age <- plot2 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_smoke_OR_med_age.pdf')
lung_smoke_OR_med_age
dev.off()




#Plot for: bladder, base model, medians, ORs, age at diag

plot1 <- ggplot(age_median_OR, aes(x=young_median_or_bladder.1, y=old_median_or_bladder.1, 
                                   color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_median_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=old_median_l95_bladder.1, ymax=old_median_u95_bladder.1)) +
        geom_errorbarh(aes(xmin=young_median_l95_bladder.1, xmax=young_median_u95_bladder.1)) +
        ylim(0, 4) + xlim(0, 4) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (AaD > median)') + xlab('OR (AaD < median)') +
        ggtitle('Bladder cancer, base model') + theme_bw()

bladder_base_OR_med_age <- plot1 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_base_OR_med_age.pdf')
bladder_base_OR_med_age
dev.off()



#Plot for: bladder, smoking model, medians, ORs, age at diag

plot2 <- ggplot(age_median_OR, aes(x=young_median_or_bladder.2, y=old_median_or_bladder.2, 
                                   color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_median_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=old_median_l95_bladder.2, ymax=old_median_u95_bladder.2)) +
        geom_errorbarh(aes(xmin=young_median_l95_bladder.2, xmax=young_median_u95_bladder.2)) +
        ylim(0, 2.5) + xlim(0, 2.5) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (AaD > median)') + xlab('OR (AaD < median)') +
        ggtitle('Bladder cancer, adjusted model') + theme_bw()

bladder_smoke_OR_med_age <- plot2 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_smoke_OR_med_age.pdf')
bladder_smoke_OR_med_age
dev.off()



#Plot for: lung, base model, quartiles, ORs, age at diag

rownames(age_quartile_OR) <- 1:nrow(age_quartile_OR)

age_quartile_OR$var_group <- NA
age_quartile_OR$var_group[1:22] <- 'Sociodemographic'
age_quartile_OR$var_group[23:47] <- 'Health risk'
age_quartile_OR$var_group[48:55] <- 'Environmental'
age_quartile_OR$var_group[56:70] <- 'Medical'
age_quartile_OR$var_group[71:98] <- 'Biomarkers'


plot1 <- ggplot(age_quartile_OR, aes(x=young_quartile_or_lung.1, y=old_quartile_or_lung.1, 
                                     color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_quartile_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=old_quartile_l95_lung.1, ymax=old_quartile_u95_lung.1)) +
        geom_errorbarh(aes(xmin=young_quartile_l95_lung.1, xmax=young_quartile_u95_lung.1)) +
        ylim(0, 7.5) + xlim(0, 7.5) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (AaD > Q3)') + xlab('OR (AaD < Q1)') +
        ggtitle('Lung cancer, base model') + theme_bw()

lung_base_OR_quar_age <- plot1 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_base_OR_quar_age.pdf')
lung_base_OR_quar_age
dev.off



#Plot for: lung, smoking model, quartiles, ORs, age at diag

plot2 <- ggplot(age_quartile_OR, aes(x=young_quartile_or_lung.2, y=old_quartile_or_lung.2, 
                                     color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_quartile_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=old_quartile_l95_lung.2, ymax=old_quartile_u95_lung.2)) +
        geom_errorbarh(aes(xmin=young_quartile_l95_lung.2, xmax=young_quartile_u95_lung.2)) +
        ylim(0, 5) + xlim(0, 5) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (AaD > Q3)') + xlab('OR (AaD < Q1)') +
        ggtitle('Lung cancer, adjusted model') + theme_bw()

lung_smoke_OR_quar_age <- plot2 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_smoke_OR_quar_age.pdf')
lung_smoke_OR_quar_age
dev.off()




#Plot for: bladder, base model, quartiles, ORs, age at diag

plot1 <- ggplot(age_quartile_OR, aes(x=young_quartile_or_bladder.1, y=old_quartile_or_bladder.1, 
                                     color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_quartile_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=old_quartile_l95_bladder.1, ymax=old_quartile_u95_bladder.1)) +
        geom_errorbarh(aes(xmin=young_quartile_l95_bladder.1, xmax=young_quartile_u95_bladder.1)) +
        ylim(0, 5) + xlim(0, 5) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (AaD > Q3)') + xlab('OR (AaD < Q1)') +
        ggtitle('Bladder cancer, base model') + theme_bw()

bladder_base_OR_quar_age <- plot1 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_base_OR_quar_age.pdf')
bladder_base_OR_quar_age
dev.off()



#Plot for: bladder, smoking model, quartiles, ORs, age at diag

plot2 <- ggplot(age_quartile_OR, aes(x=young_quartile_or_bladder.2, y=old_quartile_or_bladder.2, 
                                     color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_quartile_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=old_quartile_l95_bladder.2, ymax=old_quartile_u95_bladder.2)) +
        geom_errorbarh(aes(xmin=young_quartile_l95_bladder.2, xmax=young_quartile_u95_bladder.2)) +
        ylim(0, 4) + xlim(0, 4) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (AaD > Q3)') + xlab('OR (AaD < Q1)') +
        ggtitle('Bladder cancer, adjusted model') + theme_bw()

bladder_smoke_OR_quar_age <- plot2 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_smoke_OR_quar_age.pdf')
bladder_smoke_OR_quar_age
dev.off()





rownames(time_median_OR) <- 1:nrow(time_median_OR)

time_median_OR$var_group <- NA
time_median_OR$var_group[1:22] <- 'Sociodemographic'
time_median_OR$var_group[23:47] <- 'Health risk'
time_median_OR$var_group[48:55] <- 'Environmental'
time_median_OR$var_group[56:70] <- 'Medical'
time_median_OR$var_group[71:98] <- 'Biomarkers'

#Plot for lung, base, median, ORs, time to diag

plot1 <- ggplot(time_median_OR, aes(x=early_median_or_lung.1, y=late_median_or_lung.1, 
                                    color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(time_median_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=late_median_l95_lung.1, ymax=late_median_u95_lung.1)) +
        geom_errorbarh(aes(xmin=early_median_l95_lung.1, xmax=early_median_u95_lung.1)) +
        ylim(0, 7.5) + xlim(0, 7.5) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (TtD > median)') + xlab('OR (TtD < median)') +
        ggtitle('Lung cancer, base model') + theme_bw()

lung_base_OR_med_time <- plot1 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_base_OR_med_time.pdf')
lung_base_OR_med_time
dev.off()



#Plot for: lung, smoking model, medians, ORs, time to diag

plot2 <- ggplot(time_median_OR, aes(x=early_median_or_lung.2, y=late_median_or_lung.2, 
                                    color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(time_median_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=late_median_l95_lung.2, ymax=late_median_u95_lung.2)) +
        geom_errorbarh(aes(xmin=early_median_l95_lung.2, xmax=early_median_u95_lung.2)) +
        ylim(0, 4) + xlim(0, 4) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (TtD > median)') + xlab('OR (TtD < median)') +
        ggtitle('Lung cancer, adjusted model') + theme_bw()

lung_smoke_OR_med_time <- plot2 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_smoke_OR_med_time.pdf')
lung_smoke_OR_med_time
dev.off()




#Plot for: bladder, base model, medians, ORs, time to diag

plot1 <- ggplot(time_median_OR, aes(x=early_median_or_bladder.1, y=late_median_or_bladder.1, 
                                    color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(time_median_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=late_median_l95_bladder.1, ymax=late_median_u95_bladder.1)) +
        geom_errorbarh(aes(xmin=early_median_l95_bladder.1, xmax=early_median_u95_bladder.1)) +
        ylim(0, 4) + xlim(0, 4) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (TtD > median)') + xlab('OR (TtD < median)') +
        ggtitle('Bladder cancer, base model') + theme_bw()

bladder_base_OR_med_time <- plot1 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_base_OR_med_time.pdf')
bladder_base_OR_med_time
dev.off()



#Plot for: bladder, smoking model, medians, ORs, time to diag

plot2 <- ggplot(time_median_OR, aes(x=early_median_or_bladder.2, y=late_median_or_bladder.2, 
                                    color = var_group)) + 
        geom_point(size=2, shape=17) + geom_text_repel(label=rownames(time_median_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=late_median_l95_bladder.2, ymax=late_median_u95_bladder.2)) +
        geom_errorbarh(aes(xmin=early_median_l95_bladder.2, xmax=early_median_u95_bladder.2)) +
        ylim(0, 2.5) + xlim(0, 2.5) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (TtD > median)') + xlab('OR (TtD < median)') +
        ggtitle('Bladder cancer, adjusted model') + theme_bw()

bladder_smoke_OR_med_time <- plot2 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_smoke_OR_med_time.pdf')
bladder_smoke_OR_med_time
dev.off



#Plot for: lung, base model, quartiles, ORs, time to diag

rownames(time_quartile_OR) <- 1:nrow(time_quartile_OR)

time_quartile_OR$var_group <- NA
time_quartile_OR$var_group[1:22] <- 'Sociodemographic'
time_quartile_OR$var_group[23:47] <- 'Health risk'
time_quartile_OR$var_group[48:55] <- 'Environmental'
time_quartile_OR$var_group[56:70] <- 'Medical'
time_quartile_OR$var_group[71:98] <- 'Biomarkers'


plot1 <- ggplot(time_quartile_OR, aes(x=early_quartile_or_lung.1, y=late_quartile_or_lung.1, 
                                      color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(time_quartile_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=late_quartile_l95_lung.1, ymax=late_quartile_u95_lung.1)) +
        geom_errorbarh(aes(xmin=early_quartile_l95_lung.1, xmax=early_quartile_u95_lung.1)) +
        ylim(0, 8) + xlim(0, 8) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (TtD > Q3)') + xlab('OR (TtD < Q1)') +
        ggtitle('Lung cancer, base model') + theme_bw()

lung_base_OR_quar_time <- plot1 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_base_OR_quar_time.pdf')
lung_base_OR_quar_time
dev.off()



#Plot for: lung, smoking model, quartiles, ORs, time to diag

plot2 <- ggplot(time_quartile_OR, aes(x=early_quartile_or_lung.2, y=late_quartile_or_lung.2, 
                                      color = var_group)) + 
        geom_point(size=2, shape=17) + geom_text_repel(label=rownames(time_quartile_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=late_quartile_l95_lung.2, ymax=late_quartile_u95_lung.2)) +
        geom_errorbarh(aes(xmin=early_quartile_l95_lung.2, xmax=early_quartile_u95_lung.2)) +
        ylim(0, 4) + xlim(0, 4) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (TtD > Q3)') + xlab('OR (TtD < Q1)') +
        ggtitle('Lung cancer, adjusted model') + theme_bw()

lung_smoke_OR_quar_time <- plot2 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_smoke_OR_quar_time.pdf')
lung_smoke_OR_quar_time
dev.off()




#Plot for: bladder, base model, quartiles, ORs, time to diag

plot1 <- ggplot(time_quartile_OR, aes(x=early_quartile_or_bladder.1, y=late_quartile_or_bladder.1, 
                                      color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(time_quartile_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=late_quartile_l95_bladder.1, ymax=late_quartile_u95_bladder.1)) +
        geom_errorbarh(aes(xmin=early_quartile_l95_bladder.1, xmax=early_quartile_u95_bladder.1)) +
        ylim(0, 4.5) + xlim(0, 4.5) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (TtD > Q3)') + xlab('OR (TtD < Q1)') +
        ggtitle('Bladder cancer, base model') + theme_bw()

bladder_base_OR_quar_time <- plot1 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_base_OR_quar_time.pdf')
bladder_base_OR_quar_time
dev.off()



#Plot for: bladder, smoking model, quartiles, ORs, time to diag

plot2 <- ggplot(time_quartile_OR, aes(x=early_quartile_or_bladder.2, y=late_quartile_or_bladder.2, 
                                      color = var_group)) + 
        geom_point(size=2, shape=17) + geom_text_repel(label=rownames(time_quartile_OR)) + 
        geom_abline(linetype = 'dashed') + geom_errorbar(aes(ymin=late_quartile_l95_bladder.2, ymax=late_quartile_u95_bladder.2)) +
        geom_errorbarh(aes(xmin=early_quartile_l95_bladder.2, xmax=early_quartile_u95_bladder.2)) +
        ylim(0, 4) + xlim(0, 4) + geom_hline(yintercept = 1, linetype = 'dashed') + geom_vline(xintercept = 1, linetype = 'dashed') +
        ylab('OR (TtD > Q3)') + xlab('OR (TtD < Q1)') +
        ggtitle('Bladder cancer, adjusted model') + theme_bw()

bladder_smoke_OR_quar_time <- plot2 + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_smoke_OR_quar_time.pdf')
bladder_smoke_OR_quar_time
dev.off()


#Now the p value plots

rownames(age_median_pval) <- 1:nrow(age_median_pval)
pval_rownames

age_median_pval$var_group <- NA
age_median_pval$var_group[1:9] <- 'Sociodemographic'
age_median_pval$var_group[10:22] <- 'Health risk'
age_median_pval$var_group[23:29] <- 'Environmental'
age_median_pval$var_group[30:44] <- 'Medical'
age_median_pval$var_group[45:72] <- 'Biomarkers'

bonf = -log10(0.05/78)

#Plot for: lung, base model, medians, pvals, age at diag

plot <- ggplot(age_median_pval, aes(x=young_median_lung.1, y=old_median_lung.1, 
                                   color = var_group)) + 
            geom_point(size=2) + geom_text_repel(label=rownames(age_median_pval)) + 
            geom_abline(linetype = 'dashed') + ylim(0, 65) + xlim(0, 65) + geom_hline(yintercept = bonf, linetype = 'dashed') +
            geom_vline(xintercept = bonf, linetype = 'dashed') +
            ylab('-log[10](p) (Aad > median)') + xlab('-log[10](p) (Aad < median)') +
            ggtitle('Lung cancer, base model') + theme_bw()

lung_base_p_med_age <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_base_p_med_age.pdf')
lung_base_p_med_age
dev.off()



#Plot for: lung, adjusted model, medians, pvals, age at diag

plot <- ggplot(age_median_pval, aes(x=young_median_lung.2, y=old_median_lung.2, 
                                    color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_median_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 25) + xlim(0, 25) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Aad > median)') + xlab('-log[10](p) (Aad < median)') +
        ggtitle('Lung cancer, adjusted model') + theme_bw()

lung_smoke_p_med_age <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_smoke_p_med_age.pdf')
lung_smoke_p_med_age
dev.off()



#Plot for: bladder, base model, medians, pvals, age at diag

plot <- ggplot(age_median_pval, aes(x=young_median_bladder.1, y=old_median_bladder.1, 
                                    color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_median_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 12) + xlim(0, 12) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Aad > median)') + xlab('-log[10](p) (Aad < median)') +
        ggtitle('bladder cancer, base model') + theme_bw()

bladder_base_p_med_age <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_base_p_med_age.pdf')
bladder_base_p_med_age
dev.off()



#Plot for: bladder, adjusted model, medians, pvals, age at diag

plot <- ggplot(age_median_pval, aes(x=young_median_bladder.2, y=old_median_bladder.2, 
                                    color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_median_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 7.5) + xlim(0, 7.5) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Aad > median)') + xlab('-log[10](p) (Aad < median)') +
        ggtitle('bladder cancer, adjusted model') + theme_bw()

bladder_smoke_p_med_age <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_smoke_p_med_age.pdf')
bladder_smoke_p_med_age
dev.off()





rownames(age_quartile_pval) <- 1:nrow(age_quartile_pval)
pval_rownames

age_quartile_pval$var_group <- NA
age_quartile_pval$var_group[1:9] <- 'Sociodemographic'
age_quartile_pval$var_group[10:22] <- 'Health risk'
age_quartile_pval$var_group[23:29] <- 'Environmental'
age_quartile_pval$var_group[30:44] <- 'Medical'
age_quartile_pval$var_group[45:72] <- 'Biomarkers'

bonf = -log10(0.05/78)

#Plot for: lung, base model, quartiles, pvals, age at diag

plot <- ggplot(age_quartile_pval, aes(x=young_quartile_lung.1, y=old_quartile_lung.1, 
                                      color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_quartile_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 45) + xlim(0, 45) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Aad > Q3)') + xlab('-log[10](p) (Aad < Q1)') +
        ggtitle('Lung cancer, base model') + theme_bw()

lung_base_p_quar_age <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_base_p_quar_age.pdf')
lung_base_p_quar_age
dev.off()



#Plot for: lung, adjusted model, quartiles, pvals, age at diag

plot <- ggplot(age_quartile_pval, aes(x=young_quartile_lung.2, y=old_quartile_lung.2, 
                                      color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_quartile_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 12.5) + xlim(0, 12.5) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Aad > Q3)') + xlab('-log[10](p) (Aad < Q1)') +
        ggtitle('Lung cancer, adjusted model') + theme_bw()

lung_smoke_p_quar_age <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_smoke_p_quar_age.pdf')
lung_smoke_p_quar_age
dev.off()



#Plot for: bladder, base model, quartiles, pvals, age at diag

plot <- ggplot(age_quartile_pval, aes(x=young_quartile_bladder.1, y=old_quartile_bladder.1, 
                                      color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_quartile_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 5) + xlim(0, 5) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Aad > Q3)') + xlab('-log[10](p) (Aad < Q1)') +
        ggtitle('bladder cancer, base model') + theme_bw()

bladder_base_p_quar_age <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_base_p_quar_age.pdf')
bladder_base_p_quar_age
dev.off



#Plot for: bladder, adjusted model, quartiles, pvals, age at diag

plot <- ggplot(age_quartile_pval, aes(x=young_quartile_bladder.2, y=old_quartile_bladder.2, 
                                      color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(age_quartile_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 5) + xlim(0, 5) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Aad > Q3)') + xlab('-log[10](p) (Aad < Q1)') +
        ggtitle('bladder cancer, adjusted model') + theme_bw()

bladder_smoke_p_quar_age <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_smoke_p_quar_age.pdf')
bladder_smoke_p_quar_age
dev.off()




rownames(time_median_pval) <- 1:nrow(time_median_pval)
pval_rownames

time_median_pval$var_group <- NA
time_median_pval$var_group[1:9] <- 'Sociodemographic'
time_median_pval$var_group[10:22] <- 'Health risk'
time_median_pval$var_group[23:29] <- 'Environmental'
time_median_pval$var_group[30:44] <- 'Medical'
time_median_pval$var_group[45:72] <- 'Biomarkers'

bonf = -log10(0.05/78)

#Plot for: lung, base model, medians, pvals, time to diag

plot <- ggplot(time_median_pval, aes(x=early_median_lung.1, y=late_median_lung.1, 
                                     color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(time_median_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 60) + xlim(0, 60) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Ttd > median)') + xlab('-log[10](p) (Ttd < median)') +
        ggtitle('Lung cancer, base model') + theme_bw()

lung_base_p_med_time <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_base_p_med_time.pdf')
lung_base_p_med_time
dev.off()



#Plot for: lung, adjusted model, medians, pvals, time to diag

plot <- ggplot(time_median_pval, aes(x=early_median_lung.2, y=late_median_lung.2, 
                                     color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(time_median_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 25) + xlim(0, 25) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Ttd > median)') + xlab('-log[10](p) (Ttd < median)') +
        ggtitle('Lung cancer, adjusted model') + theme_bw()

lung_smoke_p_med_time <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_base_p_med_time.pdf')
lung_base_p_med_time
dev.off()



#Plot for: bladder, base model, medians, pvals, time to diag

plot <- ggplot(time_median_pval, aes(x=early_median_bladder.1, y=late_median_bladder.1, 
                                     color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(time_median_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 7.5) + xlim(0, 7.5) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Ttd > median)') + xlab('-log[10](p) (Ttd < median)') +
        ggtitle('bladder cancer, base model') + theme_bw()

bladder_base_p_med_time <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_base_p_med_time.pdf')
bladder_base_p_med_time
dev.off()



#Plot for: bladder, adjusted model, medians, pvals, time to diag

plot <- ggplot(time_median_pval, aes(x=early_median_bladder.2, y=late_median_bladder.2, 
                                     color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(time_median_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 6) + xlim(0, 6) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Ttd > median)') + xlab('-log[10](p) (Ttd < median)') +
        ggtitle('bladder cancer, adjusted model') + theme_bw()

bladder_smoke_p_med_time <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_smoke_p_med_time.pdf')
bladder_smoke_p_med_time
dev.off()





rownames(time_quartile_pval) <- 1:nrow(time_quartile_pval)
pval_rownames

time_quartile_pval$var_group <- NA
time_quartile_pval$var_group[1:9] <- 'Sociodemographic'
time_quartile_pval$var_group[10:22] <- 'Health risk'
time_quartile_pval$var_group[23:29] <- 'Environmental'
time_quartile_pval$var_group[30:44] <- 'Medical'
time_quartile_pval$var_group[45:72] <- 'Biomarkers'

bonf = -log10(0.05/78)

#Plot for: lung, base model, quartiles, pvals, time to diag

plot <- ggplot(time_quartile_pval, aes(x=early_quartile_lung.1, y=late_quartile_lung.1, 
                                       color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(time_quartile_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 27.5) + xlim(0, 27.5) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Ttd > Q3)') + xlab('-log[10](p) (Ttd < Q1)') +
        ggtitle('Lung cancer, base model') + theme_bw()

lung_base_p_quar_time <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_base_p_quar_time.pdf')
lung_base_p_quar_time
dev.off()



#Plot for: lung, adjusted model, quartiles, pvals, time to diag

plot <- ggplot(time_quartile_pval, aes(x=early_quartile_lung.2, y=late_quartile_lung.2, 
                                       color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(time_quartile_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 12.5) + xlim(0, 12.5) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Ttd > Q3)') + xlab('-log[10](p) (Ttd < Q1)') +
        ggtitle('Lung cancer, adjusted model') + theme_bw()

lung_smoke_p_quar_time <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/lung_smoke_p_quar_time.pdf')
lung_smoke_p_quar_time
dev.off()



#Plot for: bladder, base model, quartiles, pvals, time to diag

plot <- ggplot(time_quartile_pval, aes(x=early_quartile_bladder.1, y=late_quartile_bladder.1, 
                                       color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(time_quartile_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 5) + xlim(0, 5) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Ttd > Q3)') + xlab('-log[10](p) (Ttd < Q1)') +
        ggtitle('bladder cancer, base model') + theme_bw()

bladder_base_p_quar_time <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_base_p_quar_time.pdf')
bladder_base_p_quar_time
dev.off()



#Plot for: bladder, adjusted model, quartiles, pvals, time to diag

plot <- ggplot(time_quartile_pval, aes(x=early_quartile_bladder.2, y=late_quartile_bladder.2, 
                                       color = var_group)) + 
        geom_point(size=2) + geom_text_repel(label=rownames(time_quartile_pval)) + 
        geom_abline(linetype = 'dashed') + ylim(0, 5) + xlim(0, 5) + geom_hline(yintercept = bonf, linetype = 'dashed') +
        geom_vline(xintercept = bonf, linetype = 'dashed') +
        ylab('-log[10](p) (Ttd > Q3)') + xlab('-log[10](p) (Ttd < Q1)') +
        ggtitle('bladder cancer, adjusted model') + theme_bw()

bladder_smoke_p_quar_time <- plot + scale_color_manual(values=c("gold", "forestgreen", "tomato", 'royalblue', 'grey50'))

pdf('../Figures/sensitivity/bladder_smoke_p_quar_time.pdf')
bladder_smoke_p_quar_time
dev.off()




#Combine the plots

install.packages('gridExtra')
library(gridExtra)


age_median <- ggarrange(lung_base_OR_med_age, lung_base_p_med_age, lung_smoke_OR_med_age, lung_smoke_p_med_age,
                        bladder_base_OR_med_age, bladder_base_p_med_age, bladder_smoke_OR_med_age, bladder_smoke_p_med_age,
                        common.legend = TRUE, legend = "right", nrow = 2, ncol = 4)
pdf('../Figures/sensitivity/age_median.pdf', height = 10, width = 20)
age_median
dev.off()


age_quartiles <- ggarrange(lung_base_OR_quar_age, lung_base_p_quar_age, lung_smoke_OR_quar_age, lung_smoke_p_quar_age,
                           bladder_base_OR_quar_age, bladder_base_p_quar_age, bladder_smoke_OR_quar_age, bladder_smoke_p_quar_age,
                           common.legend = TRUE, legend = "right", nrow = 2, ncol = 4)
pdf('../Figures/sensitivity/age_quartiles.pdf', height = 10, width = 20)
age_quartiles
dev.off()


time_median <- ggarrange(lung_base_OR_med_time, lung_base_p_med_time, lung_smoke_OR_med_time, lung_smoke_p_med_time,
                        bladder_base_OR_med_time, bladder_base_p_med_time, bladder_smoke_OR_med_time, bladder_smoke_p_med_time,
                        common.legend = TRUE, legend = "right", nrow = 2, ncol = 4)
pdf('../Figures/sensitivity/time_median.pdf', height = 10, width = 20)
time_median
dev.off()


time_quartiles <- ggarrange(lung_base_OR_quar_time, lung_base_p_quar_time, lung_smoke_OR_quar_time, lung_smoke_p_quar_time,
                           bladder_base_OR_quar_time, bladder_base_p_quar_time, bladder_smoke_OR_quar_time, bladder_smoke_p_quar_time,
                           common.legend = TRUE, legend = "right", nrow = 2, ncol = 4)
pdf('../Figures/sensitivity/time_quartiles.pdf', height = 7, width = 20)
time_quartiles
dev.off()