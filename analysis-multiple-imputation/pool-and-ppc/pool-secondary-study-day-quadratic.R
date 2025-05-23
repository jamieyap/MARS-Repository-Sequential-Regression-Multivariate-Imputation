###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Prepare for pooling
###############################################################################
source("paths.R")
source(file = file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-utils.R"))
library(tidyverse)
library(mice)

# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
# When running this script within imputation-pipeline.R, the package MASS may still be loaded in the global environment
select <- dplyr::select 

dat_secondary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds"))
all_ids <- unique(dat_secondary_aim[["mars_id"]])
num_participants <- length(all_ids)

###############################################################################
# Workflow: Pool results for secondary aim
###############################################################################

results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", 1, "results_obj_secondary_study_day_quadratic.rds"))
num_terms <- nrow(results_obj)

list_pooled_est <- list()
list_pooled_std_err <- list()
list_pool_manual_output <- list()
list_pool_stats <- list()

for(j in 1:num_terms){
  list_Q <- list()
  list_U <- list()
  for(mi_dataset_num in 1:.__total_imputed_datasets){
    results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "results_obj_secondary_study_day_quadratic.rds"))
    
    list_Q[[mi_dataset_num]] <- results_obj[j,"est"]
    list_U[[mi_dataset_num]] <- (results_obj[j,"std_err"])^2 
  }
  pool_manual <- pool.scalar(Q = unlist(list_Q), U = unlist(list_U), n = num_participants)
  pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participants - num_terms, pool_manual = pool_manual)
  list_pool_stats <- append(list_pool_stats, list(pool_stats))
  
  list_pool_manual_output <- append(list_pool_manual_output, list(pool_manual))
  list_pooled_est <- append(list_pooled_est, pool_manual$qbar)
  list_pooled_std_err <- append(list_pooled_std_err, sqrt(pool_manual$t))
}

fit_pooled <- data.frame(Estimate = unlist(list_pooled_est), StdErr = unlist(list_pooled_std_err), LCL = NA_real_, UCL = NA_real_, LCL90 = NA_real_, UCL90 = NA_real_, p_value = NA_real_)
fit_pooled[["LCL"]] <- fit_pooled[["Estimate"]] - fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["UCL"]] <- fit_pooled[["Estimate"]] + fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["LCL90"]] <- fit_pooled[["Estimate"]] - fit_pooled[["StdErr"]] * qnorm(0.95)
fit_pooled[["UCL90"]] <- fit_pooled[["Estimate"]] + fit_pooled[["StdErr"]] * qnorm(0.95)
fit_pooled[["p_value"]] <- 2*pnorm(abs(fit_pooled[["Estimate"]]/fit_pooled[["StdErr"]]), lower.tail = FALSE)
row.names(fit_pooled) <- row.names(results_obj)

dat_pool_stats <- bind_rows(list_pool_stats)
row.names(dat_pool_stats) <- row.names(results_obj)

###############################################################################
# Workflow: Posterior predictive check for secondary aim
###############################################################################
list_all_comparisons_est <- list()
list_all_comparisons_stderr <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "results_obj_secondary_study_day_quadratic.rds"))
  
  for(idx_replicate in 1:.__par_total_replicates){
    results_obj_rep <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("results_obj_secondary_study_day_quadratic", "_replicate_", idx_replicate, ".rds", sep = "")))
    
    dat_this_comparison <- (results_obj_rep >= results_obj)
    dat_this_comparison <- as.data.frame(dat_this_comparison)
    
    these_cols <- c("est")
    dat_this_comparison_est <- dat_this_comparison %>% select(all_of(these_cols))
    list_all_comparisons_est <- append(list_all_comparisons_est, list(dat_this_comparison_est))
    
    these_cols <- c("std_err")
    dat_this_comparison_stderr <- dat_this_comparison %>% select(all_of(these_cols))
    list_all_comparisons_stderr <- append(list_all_comparisons_stderr, list(dat_this_comparison_stderr))
  }
}

dat_all_comparisons_est <- bind_cols(list_all_comparisons_est)
dat_all_comparisons_stderr <- bind_cols(list_all_comparisons_stderr)

pbcom_est <- rowMeans(dat_all_comparisons_est)
pbcom_stderr <- rowMeans(dat_all_comparisons_stderr)

dat_pbcom <- data.frame(pbcom_est, pbcom_stderr)
row.names(dat_pbcom) <- row.names(results_obj_rep)

###############################################################################
# Save output
###############################################################################
dat_pbcom <- dat_pbcom[5:nrow(fit_pooled),]

fit_pooled_control <- fit_pooled[1:4,]
fit_pooled_causal <- fit_pooled[5:nrow(fit_pooled),]

fit_pooled_causal_formatted <- format(round(fit_pooled_causal, 10), nsmall = 10)
fit_pooled_control_formatted <- format(round(fit_pooled_control, 10), nsmall = 10)
dat_pbcom_formatted <- format(round(dat_pbcom, 3), nsmall = 3)
dat_pool_stats_formatted <- format(round(dat_pool_stats, 5), nsmall = 5)

write.csv(fit_pooled_causal_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_secondary_causal_study_day_quadratic.csv"), row.names = TRUE)
write.csv(fit_pooled_control_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_secondary_control_study_day_quadratic.csv"), row.names = TRUE)
write.csv(dat_pbcom_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "pbcom_secondary_causal_study_day_quadratic.csv"), row.names = TRUE)
write.csv(dat_pool_stats_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "pool_stats_secondary_causal_study_day_quadratic.csv"), row.names = TRUE)
