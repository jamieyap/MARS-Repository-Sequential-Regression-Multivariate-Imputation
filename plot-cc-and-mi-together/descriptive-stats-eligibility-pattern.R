rm(list = ls())

###############################################################################
# Load packages and datasets
###############################################################################
source("paths.R")
library(tidyverse)
dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds")) 

###############################################################################
# Data preparation steps
###############################################################################
dat_primary_aim <- dat_primary_aim %>%
  group_by(participant_id) %>%
  mutate(eligibility_lag1 = lag(eligibility)) %>%
  mutate(eligibility_lag1 = replace(eligibility_lag1, decision_point==1, 0)) %>%
  ungroup(.)

dat_primary_aim <- dat_primary_aim %>%
  mutate(which_pattern = case_when(
    eligibility == 1 & eligibility_lag1 == 1 ~ 1,
    eligibility == 1 & eligibility_lag1 == 0 & any_recent_eligible_dp == 1 ~ 2,
    eligibility == 1 & eligibility_lag1 == 0 & any_recent_eligible_dp == 0 ~ 3,
    eligibility == 0 ~ 4,
    .default = NULL
  ))

dat_primary_aim <- dat_primary_aim %>%
  select(mars_id, participant_id, cluster_id, decision_point,
         eligibility, eligibility_lag1, any_recent_eligible_dp, which_pattern, everything())

###############################################################################
# Create descriptive statistics marginal over decision point
###############################################################################
dat_stats <- dat_primary_aim %>%
  group_by(which_pattern) %>%
  summarise(count = n(),
            n_observed = sum((!is.na(status_survey_ema_collapsed)) & (status_survey_ema_collapsed == "fully_completed"))) %>%
  mutate(which_pattern_factor = case_when(
    which_pattern == 1 ~ "Participant-decision points eligible at t and were also eligible at t-1",
    which_pattern == 2 ~ "Participant-decision points eligible at t, ineligible at t-1, but had at least one eligible decision point prior to t",
    which_pattern == 3 ~ "Participant-decision points which were eligible at t but had no eligible decision point prior to t since the start of the MRT",
    which_pattern == 4 ~ "Participant-decision points not eligible at t",
    .default = NULL
  )) %>%
  mutate(which_pattern_factor = as_factor(which_pattern_factor)) %>%
  select(which_pattern_factor, which_pattern, count, n_observed)

write.csv(dat_stats, file = file.path("plot-cc-and-mi-together", "eligibility_pattern_descriptive_stats.csv"))

###############################################################################
# Create descriptive statistics conditionally on decision point
###############################################################################
dat_stats_by_dp <- dat_primary_aim %>%
  group_by(decision_point, which_pattern) %>%
  summarise(count = n(),
            n_observed = sum((!is.na(status_survey_ema_collapsed)) & (status_survey_ema_collapsed == "fully_completed")))

dat_all <- expand.grid(decision_point = 1:60, which_pattern = 1:4)

dat_stats_by_dp <- full_join(x = dat_all, 
                             y = dat_stats_by_dp, 
                             by = join_by(decision_point == decision_point, 
                                          which_pattern == which_pattern))

dat_stats_by_dp <- dat_stats_by_dp %>% 
  mutate(decision_point_factor = as_factor(decision_point)) %>%
  mutate(count = replace(count, is.na(count), 0),
         n_observed = replace(n_observed, count == 0, 0))

dat_stats_by_dp <- dat_stats_by_dp %>%
  mutate(which_pattern_factor = case_when(
    which_pattern == 1 ~ "Eligible at t and t-1",
    which_pattern == 2 ~ "Eligible at t, not eligible at t-1 but had at least one eligible decision point prior to t-1",
    which_pattern == 3 ~ "Eligible at t but had no eligible decision point prior to t",
    which_pattern == 4 ~ "Not eligible at t",
    .default = NULL
  )) %>%
  mutate(which_pattern_code_factor = case_when(
    which_pattern == 1 ~ "Pattern 1",
    which_pattern == 2 ~ "Pattern 2",
    which_pattern == 3 ~ "Pattern 3",
    which_pattern == 4 ~ "Pattern 4",
    .default = NULL
  )) %>%
  mutate(which_pattern_factor = as_factor(which_pattern_factor),
         which_pattern_code_factor = as_factor(which_pattern_code_factor))

group_colors <- c("Pattern 1" = "darkviolet", 
                  "Pattern 2" = "darkviolet", 
                  "Pattern 3" = "darkviolet", 
                  "Pattern 4" = "darkviolet")

ggplot(dat_stats_by_dp, aes(x = decision_point, y = count, color = which_pattern_code_factor)) +
  scale_y_continuous(name = "No. of participants", limits = c(-5,100), breaks = seq(0,1000,20)) +
  scale_x_continuous(name = "Decision Point", limits = c(0,60), breaks = seq(0,60,6)) + 
  geom_line(linewidth = 1.5, alpha = 0.4) +
  geom_point(size = 4, alpha = 0.4) +
  scale_color_manual(values=group_colors) +
  theme(axis.text = element_text(size = 18), legend.position = "none", axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)) +
  facet_wrap(~which_pattern_code_factor) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0)) +
  geom_line(aes(x = decision_point, y = n_observed, color = which_pattern_code_factor), linewidth = 1.5, alpha = 1) + 
  geom_point(aes(x = decision_point, y = n_observed, color = which_pattern_code_factor), size = 4, alpha = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, colour = "red", alpha = 1) +
  geom_jitter(position = position_jitter(width = 0, height = 0.50, seed = 100))
  
ggsave(filename = file.path("plot-cc-and-mi-together", "eligibility_pattern_descriptive_stats_totalcount_by_dp.png"), width = 20, height = 12, units = "in", dpi = 1000)

if(file.exists("plot-cc-and-mi-together/Thumbs.db")){
  file.remove("plot-cc-and-mi-together/Thumbs.db")
}

