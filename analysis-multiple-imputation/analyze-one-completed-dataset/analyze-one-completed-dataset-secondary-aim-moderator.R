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
library(geepack)

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
dat_for_analysis_elig <- dat_for_analysis %>% filter(eligibility == 1)

fit1 <- geeglm(self_efficacy_cig ~ days_between_v1_and_coinflip_local + self_efficacy_cig_lag1 + is_high_effort + is_low_effort + I(is_high_effort*self_efficacy_cig_lag1) + I(is_low_effort*self_efficacy_cig_lag1), 
               family = gaussian, 
               data = dat_for_analysis_elig, 
               id = participant_id, 
               waves = decision_point)

Lmat <- matrix(c(0, 0, 0, 1,-1, 0, 0,
                 0, 0, 0, 0, 0, 1, -1), 
               ncol = 7, byrow = TRUE)
est_contrast <- Lmat %*% fit1$coefficients
est_std_err_contrast <- sqrt(diag(Lmat %*% vcov(fit1) %*% t(Lmat)))

dat_results <- summary(fit1)[["coefficients"]]
dat_results[["Wald"]] <- sqrt(dat_results[["Wald"]])
colnames(dat_results) <- c("est", "std_err", "Z_statistic", "p_value")

dat_contrast <- data.frame(est = est_contrast,
                           std_err = est_std_err_contrast,
                           Z_statistic = NA,
                           p_value = NA)
dat_contrast[["Z_statistic"]] <- dat_contrast[["est"]]/dat_contrast[["std_err"]]
dat_contrast[["p_value"]] <- 2*pnorm(abs(dat_contrast[["Z_statistic"]]), lower.tail = FALSE)
row.names(dat_contrast) <- c("High Effort Prompt vs Low Effort Prompt", "High Effort Prompt vs Low Effort Prompt x Moderator")

results_obj <- rbind(dat_results, dat_contrast)

print(results_obj)

saveRDS(fit1, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("fit_obj_secondary_moderator.rds", sep = "")))
saveRDS(results_obj, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("results_obj_secondary_moderator.rds", sep = "")))

###############################################################################
# Analysis with replicated dataset
###############################################################################
max_replicate_id <- max(dat_long_completed[["replicate_id"]])

for(idx_replicate in 1:max_replicate_id){
  dat_for_analysis <- dat_long_completed %>% filter(replicate_id == idx_replicate)
  dat_for_analysis_elig <- dat_for_analysis %>% filter(eligibility == 1)
  
  fit1 <- geeglm(self_efficacy_cig ~ days_between_v1_and_coinflip_local + self_efficacy_cig_lag1 + is_high_effort + is_low_effort + I(is_high_effort*self_efficacy_cig_lag1) + I(is_low_effort*self_efficacy_cig_lag1), 
                 family = gaussian, 
                 data = dat_for_analysis_elig, 
                 id = participant_id, 
                 waves = decision_point)
  
  Lmat <- matrix(c(0, 0, 0, 1,-1, 0, 0,
                   0, 0, 0, 0, 0, 1, -1), 
                 ncol = 7, byrow = TRUE)
  est_contrast <- Lmat %*% fit1$coefficients
  est_std_err_contrast <- sqrt(diag(Lmat %*% vcov(fit1) %*% t(Lmat)))
  
  dat_results <- summary(fit1)[["coefficients"]]
  dat_results[["Wald"]] <- sqrt(dat_results[["Wald"]])
  colnames(dat_results) <- c("est", "std_err", "Z_statistic", "p_value")
  
  dat_contrast <- data.frame(est = est_contrast,
                             std_err = est_std_err_contrast,
                             Z_statistic = NA,
                             p_value = NA)
  dat_contrast[["Z_statistic"]] <- dat_contrast[["est"]]/dat_contrast[["std_err"]]
  dat_contrast[["p_value"]] <- 2*pnorm(abs(dat_contrast[["Z_statistic"]]), lower.tail = FALSE)
  row.names(dat_contrast) <- c("High Effort Prompt vs Low Effort Prompt", "High Effort Prompt vs Low Effort Prompt x Moderator")
  
  results_obj <- rbind(dat_results, dat_contrast)
  
  saveRDS(fit1, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("fit_obj_secondary_moderator", "_replicate_", idx_replicate, ".rds", sep = "")))
  saveRDS(results_obj, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("results_obj_secondary_moderator", "_replicate_", idx_replicate, ".rds", sep = "")))
}

