rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

################################################################################
# Load packages
################################################################################
source("paths.R")
library(tidyverse)

total_replicates <- .__par_total_replicates

################################################################################
# Load datasets
################################################################################
dat_primary_aim <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_for_new_pipeline.rds"))
all_ids <- unique(dat_primary_aim[["mars_id"]])

################################################################################
# Construct new variables using tracked variables
################################################################################
dat_primary_aim <- dat_primary_aim %>%
  mutate(days_between_v1_and_coinflip_local_squared = days_between_v1_and_coinflip_local * days_between_v1_and_coinflip_local,
         completed_app_usage_preblock = if_else((activ_done_preblock == 1) | (read_tips_preblock == 1), 1, 0),
         ms_spent_preblock = time_spent_preblock + time_spent_tips_preblock,
         emi_resp_indicator_sum_past24hrs = if_else((eligibility == 1) & (elig24hrs == 0), 0, emi_resp_indicator_sum_past24hrs)) 

################################################################################
# Create lagged variables
################################################################################
dat_primary_aim <- dat_primary_aim %>% 
  group_by(participant_id) %>%
  mutate(eligibility_lag1 = lag(eligibility)) %>%
  mutate(eligibility_lag1 = replace(eligibility_lag1, decision_point == 1, 0)) %>%
  mutate(self_efficacy_cig_lag1 = lag(self_efficacy_cig),
         cigarette_counts_lag1 = lag(cigarette_counts),
         motivation_cig_lag1 = lag(motivation_cig)) %>%
  mutate(any_response_2qs_lag1 = lag(any_response_2qs),
         coinflip_lag1 = lag(coinflip),
         is_high_effort_lag1 = lag(is_high_effort),
         is_low_effort_lag1 = lag(is_low_effort),
         Y_nreported_past24hrs_lag1 = lag(Y_nreported_past24hrs),
         emi_resp_indicator_sum_past24hrs_lag1 = lag(emi_resp_indicator_sum_past24hrs)) %>%
  mutate(any_response_2qs_lag1 = replace(any_response_2qs, decision_point == 1, 0),
         is_high_effort_lag1 = replace(is_high_effort, decision_point == 1, 0),
         is_low_effort_lag1 = replace(is_low_effort, decision_point == 1, 0)) %>%
  ungroup(.)

################################################################################
# Create replicates
################################################################################
dat_primary_aim <- dat_primary_aim %>% 
  mutate(replicate_id = 0) %>% 
  select(replicate_id, everything())

if(total_replicates > 0){
  list_dat_all <- list()
  list_dat_all <- append(list_dat_all, list(dat_primary_aim))
  
  for(idx in 1:total_replicates){
    dat_replicate <- dat_primary_aim %>% 
      mutate(replicate_id = idx) %>%
      mutate(self_efficacy_cig = NA,
             cigarette_counts = NA,
             motivation_cig = NA)
    list_dat_all <- append(list_dat_all, list(dat_replicate))
  }
  
  dat_primary_aim <- bind_rows(list_dat_all)
}

################################################################################
# Save
################################################################################
saveRDS(dat_primary_aim, file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_for_new_pipeline_replicated.rds"))
