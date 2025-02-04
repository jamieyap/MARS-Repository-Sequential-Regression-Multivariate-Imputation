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
source(file = file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-utils.R"))
library(mice)
library(geepack)
library(tidyverse)
# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 

dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", 1, paste("dat_long_completed", ".rds", sep = "")))
use_alpha <- 0.10/2

###############################################################################
# Mean among eligible decision points micro-randomized to no prompt
###############################################################################
mi_est <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_no_prompt_by_prior_self_efficacy.rds"))
mi_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_var_no_prompt_by_prior_self_efficacy.rds"))
replicates_est <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_no_prompt_by_prior_self_efficacy.rds"))
replicates_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_no_prompt_by_prior_self_efficacy.rds"))

comparison_est <- replicates_est >= mi_est
ppc_est <- colMeans(comparison_est)
dat_ppc <- tibble(prior_self_efficacy = 0:4, ppc_est = ppc_est)

list_all_pool_stats <- list()

for(idx_decision_point in 1:5){
  dp <- idx_decision_point
  
  num_participants <- dat_long_completed %>% filter(decision_point == dp) %>% filter(replicate_id == 0) %>% filter((eligibility == 1) & (eligibility_lag1 == 1)) %>% filter(coinflip == 0) %>% nrow(.)
  pool_manual <- pool.scalar(Q = mi_est[, idx_decision_point], 
                             U = mi_var[, idx_decision_point], 
                             n = num_participants, 
                             k = 1)
  
  pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participants - 1, pool_manual = pool_manual)
  pool_stats$n <- num_participants
  pool_stats$Qbar <- mean(mi_est[, idx_decision_point])
  pool_stats$pooled_stderr <- sqrt(pool_stats$total_var)
  pool_stats$conf_int_lb <- pool_stats$Qbar - qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  pool_stats$conf_int_ub <- pool_stats$Qbar + qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  
  list_all_pool_stats <- append(list_all_pool_stats, list(pool_stats))
}

dat_all_pool_stats <- bind_rows(list_all_pool_stats)
dat_all_pool_stats <- dat_all_pool_stats %>% 
  mutate(decision_point = 0:4) %>% 
  select(decision_point, n, Qbar, pooled_stderr, conf_int_lb, conf_int_ub, everything())

dat_ppc <- dat_ppc %>% round(., digits = 3)
dat_all_pool_stats <- dat_all_pool_stats %>% round(., digits = 3)

write.csv(dat_ppc, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_no_prompt_by_prior_self_efficacy.csv"), row.names = FALSE)
write.csv(dat_all_pool_stats, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_no_prompt_by_prior_self_efficacy.csv"), row.names = FALSE)

###############################################################################
# Mean among eligible decision points micro-randomized to high effort prompt
###############################################################################
mi_est <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_high_effort_prompt_by_prior_self_efficacy.rds"))
mi_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_var_high_effort_prompt_by_prior_self_efficacy.rds"))
replicates_est <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_high_effort_prompt_by_prior_self_efficacy.rds"))
replicates_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_high_effort_prompt_by_prior_self_efficacy.rds"))

comparison_est <- replicates_est >= mi_est
ppc_est <- colMeans(comparison_est)
dat_ppc <- tibble(prior_self_efficacy = 0:4, ppc_est = ppc_est)

list_all_pool_stats <- list()

for(idx_decision_point in 1:5){
  dp <- idx_decision_point
  
  num_participants <- dat_long_completed %>% filter(decision_point == dp) %>% filter(replicate_id == 0) %>% filter((eligibility == 1) & (eligibility_lag1 == 1)) %>% filter(is_high_effort == 1) %>% nrow(.)
  pool_manual <- pool.scalar(Q = mi_est[, idx_decision_point], 
                             U = mi_var[, idx_decision_point], 
                             n = num_participants, 
                             k = 1)
  
  pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participants - 1, pool_manual = pool_manual)
  pool_stats$n <- num_participants
  pool_stats$Qbar <- mean(mi_est[, idx_decision_point])
  pool_stats$pooled_stderr <- sqrt(pool_stats$total_var)
  pool_stats$conf_int_lb <- pool_stats$Qbar - qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  pool_stats$conf_int_ub <- pool_stats$Qbar + qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  
  list_all_pool_stats <- append(list_all_pool_stats, list(pool_stats))
}

dat_all_pool_stats <- bind_rows(list_all_pool_stats)
dat_all_pool_stats <- dat_all_pool_stats %>% 
  mutate(decision_point = 0:4) %>% 
  select(decision_point, n, Qbar, pooled_stderr, conf_int_lb, conf_int_ub, everything())

dat_ppc <- dat_ppc %>% round(., digits = 3)
dat_all_pool_stats <- dat_all_pool_stats %>% round(., digits = 3)

write.csv(dat_ppc, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_high_effort_prompt_by_prior_self_efficacy.csv"), row.names = FALSE)
write.csv(dat_all_pool_stats, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_high_effort_prompt_by_prior_self_efficacy.csv"), row.names = FALSE)

###############################################################################
# Mean among eligible decision points micro-randomized to low effort prompt
###############################################################################
mi_est <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_low_effort_prompt_by_prior_self_efficacy.rds"))
mi_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_var_low_effort_prompt_by_prior_self_efficacy.rds"))
replicates_est <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_low_effort_prompt_by_prior_self_efficacy.rds"))
replicates_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_low_effort_prompt_by_prior_self_efficacy.rds"))

comparison_est <- replicates_est >= mi_est
ppc_est <- colMeans(comparison_est)
dat_ppc <- tibble(prior_self_efficacy = 0:4, ppc_est = ppc_est)

list_all_pool_stats <- list()

for(idx_decision_point in 1:5){
  dp <- idx_decision_point
  
  num_participants <- dat_long_completed %>% filter(decision_point == dp) %>% filter(replicate_id == 0) %>% filter((eligibility == 1) & (eligibility_lag1 == 1)) %>% filter(is_low_effort == 1) %>% nrow(.)
  pool_manual <- pool.scalar(Q = mi_est[, idx_decision_point], 
                             U = mi_var[, idx_decision_point], 
                             n = num_participants, 
                             k = 1)
  
  pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participants - 1, pool_manual = pool_manual)
  pool_stats$n <- num_participants
  pool_stats$Qbar <- mean(mi_est[, idx_decision_point])
  pool_stats$pooled_stderr <- sqrt(pool_stats$total_var)
  pool_stats$conf_int_lb <- pool_stats$Qbar - qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  pool_stats$conf_int_ub <- pool_stats$Qbar + qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  
  list_all_pool_stats <- append(list_all_pool_stats, list(pool_stats))
}

dat_all_pool_stats <- bind_rows(list_all_pool_stats)
dat_all_pool_stats <- dat_all_pool_stats %>% 
  mutate(decision_point = 0:4) %>% 
  select(decision_point, n, Qbar, pooled_stderr, conf_int_lb, conf_int_ub, everything())

dat_ppc <- dat_ppc %>% round(., digits = 3)
dat_all_pool_stats <- dat_all_pool_stats %>% round(., digits = 3)


write.csv(dat_ppc, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_low_effort_prompt_by_prior_self_efficacy.csv"), row.names = FALSE)
write.csv(dat_all_pool_stats, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_low_effort_prompt_by_prior_self_efficacy.csv"), row.names = FALSE)

