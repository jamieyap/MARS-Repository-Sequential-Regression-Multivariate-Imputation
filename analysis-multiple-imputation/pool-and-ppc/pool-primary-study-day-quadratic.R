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
select <- dplyr::select 

dat_primary_aim <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_for_new_pipeline.rds"))
all_ids <- unique(dat_primary_aim[["mars_id"]])
num_participants <- length(all_ids)

###############################################################################
# Workflow: Pool results for moderated effect
###############################################################################

# Causal part of the analysis model -------------------------------------------
results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", 1, "results_obj_primary_study_day_quadratic.rds"))
num_terms <- nrow(results_obj$causal_excursion_effect)

list_pooled_est <- list()
list_pooled_std_err <- list()
list_pool_manual_output <- list()
list_pool_stats <- list()

for(j in 1:num_terms){
  list_Q <- list()
  list_U <- list()
  for(mi_dataset_num in 1:.__total_imputed_datasets){
    results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "results_obj_primary_study_day_quadratic.rds"))
    
    if(class(results_obj) == "character"){
      list_Q[[mi_dataset_num]] <- NULL
      list_U[[mi_dataset_num]] <- NULL
    }else{
      list_Q[[mi_dataset_num]] <- results_obj$causal_excursion_effect[j,"Estimate"]
      list_U[[mi_dataset_num]] <- (results_obj$causal_excursion_effect[j,"StdErr"])^2
    }
  }
  pool_manual <- pool.scalar(Q = unlist(list_Q), U = unlist(list_U), n = num_participants, k = 1)
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

row.names(fit_pooled) <- c("Treatment (Prompt = 1, No Prompt = 0)", "Treatment x Day", "Treatment x Day Squared", paste("Treatment Effect on Day ", 1:8, sep = ""))
fit_pooled_causal <- fit_pooled
print(fit_pooled_causal$causal_excursion_effect)

# Control part of the analysis model ------------------------------------------
results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", 1, "results_obj_primary_study_day_quadratic.rds"))
num_terms <- nrow(results_obj$control_variables)

list_pooled_est <- list()
list_pooled_std_err <- list()
list_pool_manual_output <- list()

for(j in 1:num_terms){
  list_Q <- list()
  list_U <- list()
  for(mi_dataset_num in 1:.__total_imputed_datasets){
    results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "results_obj_primary_study_day_quadratic.rds"))
    
    if(class(results_obj) == "character"){
      list_Q[[mi_dataset_num]] <- NULL
      list_U[[mi_dataset_num]] <- NULL
    }else{
      list_Q[[mi_dataset_num]] <- results_obj$control_variables[j,"Estimate"]
      list_U[[mi_dataset_num]] <- (results_obj$control_variables[j,"StdErr"])^2 
    }
  }
  pool_manual <- pool.scalar(Q = unlist(list_Q), U = unlist(list_U), n = num_participants, k = 1)
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

row.names(fit_pooled) <- row.names(results_obj$control_variables)
fit_pooled_control <- fit_pooled

dat_pool_stats <- bind_rows(list_pool_stats)
row.names(dat_pool_stats) <- row.names(results_obj$causal_excursion_effect)

###############################################################################
# Workflow: Posterior predictive check for moderated effect
###############################################################################
list_all_comparisons_est <- list()
list_all_comparisons_stderr <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "results_obj_primary_study_day_quadratic.rds"))
  
  if(class(results_obj) == "character"){
    list_all_comparisons_est <- append(list_all_comparisons_est, list(NULL))
    list_all_comparisons_stderr <- append(list_all_comparisons_stderr, list(NULL))
    next
  }
  
  for(idx_replicate in 1:.__par_total_replicates){
    results_obj_rep <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("results_obj_primary_study_day_quadratic", "_replicate_", idx_replicate, ".rds", sep = "")))
    
    if((class(results_obj) == "character")| (class(results_obj_rep) == "character")){
      list_all_comparisons_est <- append(list_all_comparisons_est, list(NULL))
      list_all_comparisons_stderr <- append(list_all_comparisons_stderr, list(NULL)) 
    }else{
      dat_this_comparison <- (results_obj_rep$causal_excursion_effect >= results_obj$causal_excursion_effect)
      dat_this_comparison <- as.data.frame(dat_this_comparison)
      
      these_cols <- c("Estimate")
      dat_this_comparison_est <- dat_this_comparison %>% select(all_of(these_cols))
      list_all_comparisons_est <- append(list_all_comparisons_est, list(dat_this_comparison_est))
      
      these_cols <- c("StdErr")
      dat_this_comparison_stderr <- dat_this_comparison %>% select(all_of(these_cols))
      list_all_comparisons_stderr <- append(list_all_comparisons_stderr, list(dat_this_comparison_stderr)) 
    }
  }
}

dat_all_comparisons_est <- bind_cols(list_all_comparisons_est)
dat_all_comparisons_stderr <- bind_cols(list_all_comparisons_stderr)

pbcom_est <- rowMeans(dat_all_comparisons_est)
pbcom_stderr <- rowMeans(dat_all_comparisons_stderr)

dat_pbcom <- data.frame(pbcom_est, pbcom_stderr)

###############################################################################
# Save output
###############################################################################
fit_pooled_causal_formatted <- format(round(fit_pooled_causal, 5), nsmall = 5)
fit_pooled_control_formatted <- format(round(fit_pooled_control, 5), nsmall = 5)
dat_pbcom_formatted <- format(round(dat_pbcom, 5), nsmall = 5)
dat_pool_stats_formatted <- format(round(dat_pool_stats, 5), nsmall = 5)

write.csv(fit_pooled_causal_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_primary_causal_study_day_quadratic.csv"), row.names = TRUE)
write.csv(fit_pooled_control_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_primary_control_study_day_quadratic.csv"), row.names = TRUE)
write.csv(dat_pbcom_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "pbcom_primary_study_day_quadratic.csv"), row.names = TRUE)
write.csv(dat_pool_stats_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "pool_stats_primary_study_day_quadratic.csv"), row.names = TRUE)

