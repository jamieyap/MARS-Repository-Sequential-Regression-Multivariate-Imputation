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

# This is the data frame that is the output of the very last step of our data pipeline
dat_primary_aim <- readRDS(file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_for_new_pipeline.rds"))
dat_primary_aim <- dat_primary_aim %>% filter((decision_point >= 7) & (decision_point <= 54))

use_alpha <- 0.10/2

###############################################################################
# Mean among eligible decision points micro-randomized to no prompt
###############################################################################
dat <- dat_primary_aim %>% filter(eligibility == 1) %>% filter(coinflip == 0)
dat_count_total <- dat %>% 
  group_by(decision_point) %>% 
  summarise(n_total = n(),
            n_observed_among_total = sum((!is.na(status_survey_ema_collapsed)) & (status_survey_ema_collapsed == "fully_completed")))

list_fit_by_dp <- dat %>% group_by(decision_point) %>% group_map(~ geeglm(self_efficacy_cig ~ 1, data = .x, id = participant_id, family = gaussian))
list_estimates_by_dp_mu_scale <- lapply(list_fit_by_dp, 
                                        function(current_fit, a = use_alpha){
                                          results <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                          est <- results[["Estimate"]]
                                          var <- current_fit %>% vcov(.) %>% c(.)
                                          stderr <- sqrt(var)
                                          conf_int_lb <- est - qnorm(a, lower.tail = FALSE) * stderr
                                          conf_int_ub <- est + qnorm(a, lower.tail = FALSE) * stderr
                                          results_mu_scale <- tibble(est = est, conf_int_lb = conf_int_lb, conf_int_ub = conf_int_ub)
                                          return(results_mu_scale)
                                        })
dat_estimates_by_dp_mu_scale <- bind_rows(list_estimates_by_dp_mu_scale)
dat_estimates_by_dp_mu_scale$decision_point <- 7:54
dat_estimates_by_dp_mu_scale <- left_join(x = dat_count_total, y = dat_estimates_by_dp_mu_scale, by = join_by(decision_point == decision_point))
dat_estimates_by_dp_mu_scale <- dat_estimates_by_dp_mu_scale %>% round(., digits = 3)

write.csv(dat_estimates_by_dp_mu_scale, file = file.path("analysis-complete-case", "formatted-output", "cc_est_no_prompt_by_dp.csv"), row.names = FALSE)

###############################################################################
# Mean among eligible decision points micro-randomized to high effort prompt
###############################################################################
dat <- dat_primary_aim %>% filter(eligibility == 1) %>% filter(is_high_effort == 1)
dat_count_total <- dat %>% 
  group_by(decision_point) %>% 
  summarise(n_total = n(),
            n_observed_among_total = sum((!is.na(status_survey_ema_collapsed)) & (status_survey_ema_collapsed == "fully_completed")))

list_fit_by_dp <- dat %>% group_by(decision_point) %>% group_map(~ geeglm(self_efficacy_cig ~ 1, data = .x, id = participant_id, family = gaussian))
list_estimates_by_dp_mu_scale <- lapply(list_fit_by_dp, 
                                        function(current_fit, a = use_alpha){
                                          results <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                          est <- results[["Estimate"]]
                                          var <- current_fit %>% vcov(.) %>% c(.)
                                          stderr <- sqrt(var)
                                          conf_int_lb <- est - qnorm(a, lower.tail = FALSE) * stderr
                                          conf_int_ub <- est + qnorm(a, lower.tail = FALSE) * stderr
                                          results_mu_scale <- tibble(est = est, conf_int_lb = conf_int_lb, conf_int_ub = conf_int_ub)
                                          return(results_mu_scale)
                                        })
dat_estimates_by_dp_mu_scale <- bind_rows(list_estimates_by_dp_mu_scale)
dat_estimates_by_dp_mu_scale$decision_point <- 7:54
dat_estimates_by_dp_mu_scale <- left_join(x = dat_count_total, y = dat_estimates_by_dp_mu_scale, by = join_by(decision_point == decision_point))
dat_estimates_by_dp_mu_scale <- dat_estimates_by_dp_mu_scale %>% round(., digits = 3)

write.csv(dat_estimates_by_dp_mu_scale, file = file.path("analysis-complete-case", "formatted-output", "cc_est_high_effort_prompt_by_dp.csv"), row.names = FALSE)

###############################################################################
# Mean among eligible decision points micro-randomized to low effort prompt
###############################################################################
dat <- dat_primary_aim %>% filter(eligibility == 1) %>% filter(is_low_effort == 1)
dat_count_total <- dat %>% 
  group_by(decision_point) %>% 
  summarise(n_total = n(),
            n_observed_among_total = sum((!is.na(status_survey_ema_collapsed)) & (status_survey_ema_collapsed == "fully_completed")))

list_fit_by_dp <- dat %>% group_by(decision_point) %>% group_map(~ geeglm(self_efficacy_cig ~ 1, data = .x, id = participant_id, family = gaussian))
list_estimates_by_dp_mu_scale <- lapply(list_fit_by_dp, 
                                        function(current_fit, a = use_alpha){
                                          results <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                          est <- results[["Estimate"]]
                                          var <- current_fit %>% vcov(.) %>% c(.)
                                          stderr <- sqrt(var)
                                          conf_int_lb <- est - qnorm(a, lower.tail = FALSE) * stderr
                                          conf_int_ub <- est + qnorm(a, lower.tail = FALSE) * stderr
                                          results_mu_scale <- tibble(est = est, conf_int_lb = conf_int_lb, conf_int_ub = conf_int_ub)
                                          return(results_mu_scale)
                                        })
dat_estimates_by_dp_mu_scale <- bind_rows(list_estimates_by_dp_mu_scale)
dat_estimates_by_dp_mu_scale$decision_point <- 7:54
dat_estimates_by_dp_mu_scale <- left_join(x = dat_count_total, y = dat_estimates_by_dp_mu_scale, by = join_by(decision_point == decision_point))
dat_estimates_by_dp_mu_scale <- dat_estimates_by_dp_mu_scale %>% round(., digits = 3)

write.csv(dat_estimates_by_dp_mu_scale, file = file.path("analysis-complete-case", "formatted-output", "cc_est_low_effort_prompt_by_dp.csv"), row.names = FALSE)

