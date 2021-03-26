README

Recommended workflow:

DATA PROCESSING:
	1. recoding_covar.R -- Extracting covariates and recoding raw data
	2. master_recoding_outcome.R -- Defining inclusion/exclusion criteria and outcome
	3. recoding_comorbid.R (Run on HPC) -- Extracting HES data and recoding into binary variables
	4. mca_meat.R (Run on HPC) -- Generating composite scores
	5. master_recoding_rfvar.R -- Recoding covariate variables
	6. extract_biomarker.R -- Extracting biomarkers of interest from raw data
	7. master_biomarker.R -- Processing bioamarkers for descriptive analysis
	8. master_imputing_biomarker.R (Run on HPC) -- Imputing biomarkers for regression analysis
	9. make_multivar_data.R -- Generating dataset for multivariate analysis
	
UNIVARIATE ANALYSIS:
	10. table1_creation2.0.R -- Table1
	11. master_univar.R + master_univ_viz.R -- Univariate logistic regression
	12. age_diag_univ.R -- Age at diagnosis analysis by medians
	12. time_to_diag_univ.R -- Time to diagnosis analysis by medians
	12. sensitivity_analysis_quartiles.R -- Age and time to diagnosis analysis by quartiles
	12. sensitivity_plots.R -- OR plots for univariate sensitivity analyses
	13. agetimetodiag_histograms.R -- Histograms for age and time to diagnosis
	
MULTIVARIATE ANALYSES:
	14. master_denoise.R (Run on HPC) -- One-hot encoding and Denoising multivariate data
	15. stab_select_lasso.R + stab_select_lasso_force.R (Run on HPC) -- Stability selection logistic LASSO + Sensitivity analysis
	16. split_train_test.R + stab_select_spls_TT.R (Run on HPC) -- Stabiliy selection sPLS
	17. lasso_spls_viz_TT.R + lass_spls_pred_perf.R -- LASSO and sPLS visualisations
	18. stab_select_gpls.R (Run on HPC)  -- Stabiliy selection gPLS
	19. stab_select_sgpls.R (Run on HPC) -- Stabiliy selection sgPLS
	20. gPLS_sgPLS_viz.R -- gPLS and sgPLS selection visualisations

TARGETED + STRATIFIED ANALYSES:
	21. ./strat_sex/ Stratification analysis by sex
	22. ./subtype/ Targeted analysis of cancer site
	23. ./nvsm/ Targeted analysis of never-smokers
	24. ./strat_townsend/ Stratification analysis by townsend deprivation index

Source:
* penalisation_functions.R -- Functions for stability selection logistic LASSO
* pls_functions.R -- Functions for stability selection PLS
* pls_functions_cali.R -- Functions for stability heatmap sPLS from calibration 

Necessary packages:
QUIC - (conda install -c r_test r-quic)