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
  mutate(which_pattern_factor = as_factor(which_pattern_factor))

write.csv(dat_stats, file = file.path("plot-cc-and-mi-together", "eligibility_pattern_descriptive_stats_day0_to_day9.csv"))

dat_stats <- dat_primary_aim %>%
  filter((decision_point >= 7) & (decision_point <= 54)) %>%
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
  mutate(which_pattern_factor = as_factor(which_pattern_factor))

write.csv(dat_stats, file = file.path("plot-cc-and-mi-together", "eligibility_pattern_descriptive_stats_day1_to_day8.csv"))

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
    which_pattern == 1 ~ "Stratum 3 only",
    which_pattern == 2 ~ "Stratum 2 only",
    which_pattern == 3 ~ "Stratum 1 only",
    which_pattern == 4 ~ "Stratum 4 only",
    .default = NULL
  )) %>%
  mutate(which_pattern_factor = as_factor(which_pattern_factor),
         which_pattern_code_factor = as_factor(which_pattern_code_factor))

dat_stats_by_dp_subset <- dat_stats_by_dp %>% filter(which_pattern !=4)

ggplot(dat_stats_by_dp_subset, aes(x = decision_point, y = count, group = which_pattern_code_factor)) +
  scale_y_continuous(name = "No. of participants", limits = c(-5,100), breaks = seq(0,1000,20)) +
  scale_x_continuous(name = "Decision Point Number in the Study", limits = c(0,60), breaks = seq(0,60,20)) + 
  geom_line(linewidth = 1.5, alpha = 1, color = "firebrick") +
  geom_point(shape = 24, colour = "firebrick", fill = "pink", size = 3, stroke = 3) +
  theme(axis.text = element_text(size = 18), legend.position = "none", axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)) +
  facet_wrap(~which_pattern_code_factor) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0)) +
  geom_line(aes(x = decision_point, y = n_observed), color = "black", linewidth = 1.5, alpha = 1) + 
  geom_point(aes(x = decision_point, y = n_observed), color = "black", shape = 21, fill = "gray", size = 3, stroke = 3, alpha = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, colour = "red", alpha = 1)
  
ggsave(filename = file.path("plot-cc-and-mi-together", "eligibility_pattern_descriptive_stats_totalcount_by_dp.png"), width = 24, height = 8, units = "in", dpi = 1000)

dat_stats_by_dp_collapsed <- dat_stats_by_dp %>%
  group_by(decision_point) %>%
  summarise(count_ineligible = sum(1*(which_pattern ==4)*count),
            count_eligible = sum(1*(which_pattern !=4)*count),
            n_observed_eligible = sum(1*(which_pattern !=4)*n_observed)) %>%
  mutate(agg_pattern = "Strata 1-3 Combined vs. Stratum 4")

ggplot(dat_stats_by_dp_collapsed, aes(x = decision_point, y = count_eligible)) +
  geom_line(aes(x = decision_point, y = count_ineligible), color = "darkgreen", linewidth = 1.5, alpha = 1) + 
  geom_point(aes(x = decision_point, y = count_ineligible), color = "darkgreen", shape = 22, fill = "green", size = 3, stroke = 3, alpha = 1) +
  scale_y_continuous(name = "No. of participants", limits = c(-5,100), breaks = seq(0,1000,20)) +
  scale_x_continuous(name = "Decision Point Number in the Study", limits = c(0,60), breaks = seq(0,60,20)) + 
  geom_line(linewidth = 1.5, alpha = 1, color = "firebrick") +
  geom_point(shape = 24, colour = "firebrick", fill = "pink", size = 3, stroke = 3) +
  geom_line(aes(x = decision_point, y = n_observed_eligible), color = "black", linewidth = 1.5, alpha = 1) + 
  geom_point(aes(x = decision_point, y = n_observed_eligible), color = "black", shape = 21, fill = "gray", size = 3, stroke = 3, alpha = 1) +
  theme(axis.text = element_text(size = 18), legend.position = "none", axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)) +
  facet_wrap(~agg_pattern) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, colour = "red", alpha = 1)

ggsave(filename = file.path("plot-cc-and-mi-together", "eligibility_pattern_descriptive_stats_aggregate_totalcount_by_dp.png"), width = 8, height = 8, units = "in", dpi = 1000)


if(file.exists("plot-cc-and-mi-together/Thumbs.db")){
  file.remove("plot-cc-and-mi-together/Thumbs.db")
}

