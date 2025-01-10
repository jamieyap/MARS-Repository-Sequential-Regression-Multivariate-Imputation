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
         completed_app_usage_preblock = if_else((activ_done_preblock == 1) | (read_tips_preblock == 1), 1, 0)) 

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
  mutate(completed_app_usage_preblock_lag1 = lag(completed_app_usage_preblock)) %>%
  mutate(completed_app_usage_preblock_lag1 = replace(completed_app_usage_preblock_lag1, decision_point == 1, 0)) %>%
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
