rm(list = ls())

################################################################################
# Load packages
################################################################################
source("paths.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
dat_primary_aim <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_for_new_pipeline_replicated.rds"))

################################################################################
# How many replicates do we have?
################################################################################
all_replicate_ids <- unique(dat_primary_aim[["replicate_id"]])
maximum_replicate_id <- max(all_replicate_ids)

################################################################################
# Variables assessed at baseline which will be included in the imputation or
# data analysis models
################################################################################

these_vars <- c("age", "is_male", "has_partner", "is_latino", "is_not_latino_and_black", "is_not_latino_and_other", # demographic variables
                "baseline_tobacco_history", "Nicotine_dep", # baseline tobacco dependence
                "income_val", "FinancialStrain", "nd_mean", "food_security_mean", "SSSladders", "pp1_1", # baseline socio-economic status
                "srq_mean", # baseline self-regulatory capacity
                "SE_total", # baseline self-efficacy,
                "TSAM_Total", # baseline motivation
                "sni_count", "sni_active", "sni_people", "isel_belonging", "isel_appraisal", "isel_tangible") # baseline social support

dat_baseline_wide <- dat_primary_aim %>%
  select(replicate_id, participant_id, all_of(these_vars)) %>%  
  arrange(replicate_id, participant_id) %>% unique(.)

if(maximum_replicate_id > 0){
  dat_original <- dat_baseline_wide %>% filter(replicate_id == 0)
  dat_replicate <- dat_baseline_wide %>% filter(replicate_id > 0)
  
  dat_replicate <- dat_replicate %>% 
    mutate(across(.cols = all_of(these_vars), 
                  .fns = function(curr_col){
                    curr_col = NA_real_
                    return(curr_col)
                  }
    )
    )
  
  dat_baseline_wide <- rbind(dat_original, dat_replicate)
}

################################################################################
# Time-varying variables which have missing values and will be imputed
################################################################################
these_vars_will_be_imputed <- c("self_efficacy_cig", "cigarette_counts", "motivation_cig",
                                "self_efficacy_cig_lag1", "cigarette_counts_lag1", "motivation_cig_lag1",
                                "self_efficacy_cig_mean_past24hrs", "cigarette_counts_sum_past24hrs", "motivation_cig_mean_past24hrs")

dat_timevarying_long_with_missing <- dat_primary_aim %>%
  select(replicate_id, participant_id, decision_point, all_of(these_vars_will_be_imputed)) %>% 
  arrange(replicate_id, participant_id, decision_point)

if(maximum_replicate_id > 0){
  dat_original <- dat_timevarying_long_with_missing %>% filter(replicate_id == 0)
  dat_replicate <- dat_timevarying_long_with_missing %>% filter(replicate_id > 0)
  
  dat_replicate <- dat_replicate %>% 
    mutate(across(.cols = all_of(these_vars_will_be_imputed), 
                  .fns = function(curr_col){
                    curr_col = NA_real_
                    return(curr_col)
                  }))
  
  dat_timevarying_long_with_missing <- rbind(dat_original, dat_replicate)
}

################################################################################
# Time-varying variables which DO NOT have missing values and hence will NOT be 
# imputed
################################################################################
these_vars_will_not_be_imputed <- c("any_recent_eligible_dp", "eligibility", "eligibility_lag1", "elig24hrs", 
                                    "coinflip", "is_high_effort", "is_low_effort", 
                                    "coinflip_lag1", "is_high_effort_lag1", "is_low_effort_lag1", 
                                    "matched_24hrs", "matched_recent",
                                    "days_between_v1_and_coinflip_local", "days_between_v1_and_coinflip_local_squared", "hours_elapsed_since_most_recent_eligible", "hour_coinflip_local",
                                    "any_response_2qs", "any_response_2qs_lag1", 
                                    "completed_app_usage_preblock", "ms_spent_preblock",
                                    "Y_nreported_past24hrs", "emi_resp_indicator_sum_past24hrs",
                                    "Y_nreported_past24hrs_lag1", "emi_resp_indicator_sum_past24hrs_lag1")

dat_timevarying_long_without_missing <- dat_primary_aim %>%
  select(replicate_id, participant_id, decision_point, all_of(these_vars_will_not_be_imputed)) %>% 
  arrange(replicate_id, participant_id, decision_point)

################################################################################
# Merge data frame containing time-varying variables with missing data
# and data frame containing time-varying variables without missing data
################################################################################
dat_timevarying_long <- full_join(x = dat_timevarying_long_without_missing,
                                  y = dat_timevarying_long_with_missing,
                                  by = join_by(replicate_id == replicate_id,
                                               participant_id == participant_id,
                                               decision_point == decision_point))

################################################################################
# Transform dataset with time-varying covariates from long to wide format
################################################################################
vars_all <- c(these_vars_will_be_imputed,
              these_vars_will_not_be_imputed)

spec1 <- dat_timevarying_long %>% 
  build_wider_spec(names_from = decision_point, 
                   names_prefix = "dp",
                   values_from = all_of(vars_all))

dat_timevarying_wide <- dat_timevarying_long %>% pivot_wider_spec(spec1, id_cols = c("replicate_id", "participant_id"))

################################################################################
# What are the variable names in each dataset that will have to eventually be 
# transformed from wide to long format at a later step in the pipeline?
################################################################################
cols_to_drop <- c("replicate_id", "participant_id", "decision_point")
cols_to_keep_baseline <- colnames(dat_baseline_wide)[!(colnames(dat_baseline_wide) %in% cols_to_drop)]
cols_to_keep_timevarying <- colnames(dat_timevarying_long)[!(colnames(dat_timevarying_long) %in% cols_to_drop)]

saveRDS(cols_to_drop, file = file.path(path_multiple_imputation_pipeline_data, "cols_to_drop.rds"))
saveRDS(cols_to_keep_baseline, file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_baseline.rds"))
saveRDS(cols_to_keep_timevarying, file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_timevarying.rds"))

################################################################################
# Save datasets
################################################################################
saveRDS(dat_baseline_wide, file = file.path(path_multiple_imputation_pipeline_data, "dat_baseline_wide.rds"))
saveRDS(dat_timevarying_wide, file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))

