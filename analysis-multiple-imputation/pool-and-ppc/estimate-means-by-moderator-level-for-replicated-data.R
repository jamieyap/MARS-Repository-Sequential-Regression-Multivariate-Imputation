rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Load packages
###############################################################################
source("paths.R")
library(geepack)
library(tidyverse)
# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 

###############################################################################
# Mean among eligible decision points micro-randomized to no prompt
###############################################################################
list_all_estimates_by_dp <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))
  
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 1) %>% filter((eligibility == 1) & (eligibility_lag1 == 1)) %>% filter(coinflip == 0)
  list_current_fit_by_dp <- dat_long_completed %>% group_by(self_efficacy_cig_lag1) %>% group_map(~ geeglm(self_efficacy_cig ~ 1, data = .x, id = participant_id, family = gaussian))
  list_current_estimates_by_dp <- lapply(list_current_fit_by_dp,
                                         function(current_fit){
                                           results <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                           est <- results[["Estimate"]]
                                           var <- current_fit %>% vcov(.) %>% c(.)
                                           results <- tibble(est = est, var = var)
                                           return(results)
                                         })
  list_all_estimates_by_dp <- append(list_all_estimates_by_dp, list(list_current_estimates_by_dp))
}

all_est <- matrix(rep(NA, .__total_imputed_datasets * 5), nrow = .__total_imputed_datasets, ncol = 5)
all_var <- matrix(rep(NA, .__total_imputed_datasets * 5), nrow = .__total_imputed_datasets, ncol = 5)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  for(idx_decision_point in 1:5){
    all_est[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["est"]]
    all_var[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["var"]]
  }
}

saveRDS(all_est, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_no_prompt_by_prior_self_efficacy.rds"))
saveRDS(all_var, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_no_prompt_by_prior_self_efficacy.rds"))

###############################################################################
# Mean among eligible decision points micro-randomized to high effort prompt
###############################################################################
list_all_estimates_by_dp <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))
  
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 1) %>% filter((eligibility == 1) & (eligibility_lag1 == 1)) %>% filter(is_high_effort == 1)
  list_current_fit_by_dp <- dat_long_completed %>% group_by(self_efficacy_cig_lag1) %>% group_map(~ geeglm(self_efficacy_cig ~ 1, data = .x, id = participant_id, family = gaussian))
  list_current_estimates_by_dp <- lapply(list_current_fit_by_dp,
                                         function(current_fit){
                                           results <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                           est <- results[["Estimate"]]
                                           var <- current_fit %>% vcov(.) %>% c(.)
                                           results <- tibble(est = est, var = var)
                                           return(results)
                                         })
  list_all_estimates_by_dp <- append(list_all_estimates_by_dp, list(list_current_estimates_by_dp))
}

all_est <- matrix(rep(NA, .__total_imputed_datasets * 5), nrow = .__total_imputed_datasets, ncol = 5)
all_var <- matrix(rep(NA, .__total_imputed_datasets * 5), nrow = .__total_imputed_datasets, ncol = 5)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  for(idx_decision_point in 1:5){
    all_est[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["est"]]
    all_var[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["var"]]
  }
}

saveRDS(all_est, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_high_effort_prompt_by_prior_self_efficacy.rds"))
saveRDS(all_var, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_high_effort_prompt_by_prior_self_efficacy.rds"))

###############################################################################
# Mean among eligible decision points micro-randomized to low effort prompt
###############################################################################

list_all_estimates_by_dp <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))
  
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 1) %>% filter((eligibility == 1) & (eligibility_lag1 == 1)) %>% filter(is_low_effort == 1)
  list_current_fit_by_dp <- dat_long_completed %>% group_by(self_efficacy_cig_lag1) %>% group_map(~ geeglm(self_efficacy_cig ~ 1, data = .x, id = participant_id, family = gaussian))
  list_current_estimates_by_dp <- lapply(list_current_fit_by_dp,
                                         function(current_fit){
                                           results <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                           est <- results[["Estimate"]]
                                           var <- current_fit %>% vcov(.) %>% c(.)
                                           results <- tibble(est = est, var = var)
                                           return(results)
                                         })
  list_all_estimates_by_dp <- append(list_all_estimates_by_dp, list(list_current_estimates_by_dp))
}

all_est <- matrix(rep(NA, .__total_imputed_datasets * 5), nrow = .__total_imputed_datasets, ncol = 5)
all_var <- matrix(rep(NA, .__total_imputed_datasets * 5), nrow = .__total_imputed_datasets, ncol = 5)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  for(idx_decision_point in 1:5){
    all_est[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["est"]]
    all_var[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["var"]]
  }
}

saveRDS(all_est, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_low_effort_prompt_by_prior_self_efficacy.rds"))
saveRDS(all_var, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_low_effort_prompt_by_prior_self_efficacy.rds"))

