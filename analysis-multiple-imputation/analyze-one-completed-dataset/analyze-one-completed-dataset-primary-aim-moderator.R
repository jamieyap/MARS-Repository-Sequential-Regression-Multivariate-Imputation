###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Simulation parameters for generating the current completed dataset
###############################################################################
mi_dataset_num <- .__current_idx

source("paths.R")
library(tidyverse)
library(mice)
library(MRTAnalysis)

# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
# When running this script within imputation-pipeline.R, the package MASS may still be loaded in the global environment
select <- dplyr::select 

dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_long_completed.rds"))
max_replicate_id <- max(dat_long_completed[["replicate_id"]])

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results"))
}

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num))
}

###############################################################################
# Take the subset of decision points which were eligible for
# micro-randomization at the current decision point and the prior
# decision point.
# Also, only retain decision points between days 2-9.
###############################################################################
dat_long_completed <- dat_long_completed %>% 
  filter((eligibility == 1) & (eligibility_lag1 == 1)) %>%
  filter((decision_point >= 7) & (decision_point <= 54))

###############################################################################
# Analysis with completed dataset
###############################################################################
dat_for_analysis <- dat_long_completed %>% filter(replicate_id == 0)

fit1 <- tryCatch(expr = {wcls(
  data = dat_for_analysis,
  id = "participant_id",  
  outcome = "self_efficacy_cig",
  treatment = "coinflip",
  rand_prob = 0.5,
  moderator_formula = ~ self_efficacy_cig_lag1,  
  control_formula = ~ 1 + days_between_v1_and_coinflip_local + self_efficacy_cig_lag1, 
  availability = "eligibility"
)},
warning = function(w){"Hey, a warning"})

if(sum(class(fit1) == "character") > 0){
  results_obj <- "Hey, a warning"
}else{
  Lmat <- matrix(c(rep(1,5), 0:4), ncol = 2, byrow = FALSE)
  results_obj <- summary(fit1, show_control_fit = TRUE, lincomb = Lmat)
}

print(results_obj)

saveRDS(fit1, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("fit_obj_primary_moderator.rds", sep = "")))
saveRDS(results_obj, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("results_obj_primary_moderator.rds", sep = "")))

###############################################################################
# Analysis with replicated dataset
###############################################################################
max_replicate_id <- max(dat_long_completed[["replicate_id"]])

for(idx_replicate in 1:max_replicate_id){
  dat_for_analysis <- dat_long_completed %>% filter(replicate_id == idx_replicate)
  
  fit1 <- tryCatch(expr = {wcls(
    data = dat_for_analysis,
    id = "participant_id",  
    outcome = "self_efficacy_cig",
    treatment = "coinflip",
    rand_prob = 0.5,
    moderator_formula = ~ self_efficacy_cig_lag1,  
    control_formula = ~ 1 + days_between_v1_and_coinflip_local + self_efficacy_cig_lag1, 
    availability = "eligibility"
  )},
  warning = function(w){"Hey, a warning"})
  
  if(sum(class(fit1) == "character") > 0){
    results_obj <- "Hey, a warning"
  }else{
    Lmat <- matrix(c(rep(1,5), 0:4), ncol = 2, byrow = FALSE)
    results_obj <- summary(fit1, show_control_fit = TRUE, lincomb = Lmat)
  }
  
  saveRDS(fit1, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("fit_obj_primary_moderator", "_replicate_", idx_replicate, ".rds", sep = "")))
  saveRDS(results_obj, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("results_obj_primary_moderator", "_replicate_", idx_replicate, ".rds", sep = "")))
}

