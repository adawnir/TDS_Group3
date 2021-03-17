README

Recommended workflow:
	1. recoding_covar.R -- Extracting covariates and recoding raw data
	2. master_recoding_outcome.R -- Defining inclusion/exclusion criteria and outcome
	3. recoding_comorbid.R (Run on HPC) -- Extracting HES data and recoding into binary variables
	4. mca_meat.R (Run on HPC) -- Generating composite scores
	5. master_recoding_rfvar.R -- Recoding covariate variables
	6. extract_biomarker.R -- Extracting biomarkers of interest from raw data
	7. master_biomarker.R -- Processing bioamarkers for descriptive analysis
	8. master_imputing_biomarker.R (Run on HPC) -- Imputing biomarkers for regression analysis
	9. check_complete_cases.R -- Generating dataset for multivariate analysis
	10. Where is the script for table 1?
	11. master_univar.R + univ_forest_manhat.R -- Univariate logistic regression
	12. age_diag_Ines.R + time_to_diag_univ.R -- Sensitivity analysis on age at diagnosis and time to diagnosis
	13. master_denoise.R (Run on HPC) -- One-hot encoding and Denoising multivariate data
	14. stab_select_lasso.R (Run on HPC) + lasso_viz.R (+ lasso_viz_panels.R) -- Stability selection logistic LASSO
	15. Targeted analyses: subtype_LASSO (HPC)
	16. ./strat_sex/strat_sex_univar.R + strat_sex_univ_viz.R -- Stratified univariate by sex
	17. ./strat_sex/strat_sex_denoise.R + strat_sex_lasso.R + strat_sex_lasso_viz.R -- Stratified LASSO by sex
	18. PLS

Source:
* penalisation_function.R -- Functions for stability selection logistic LASSO