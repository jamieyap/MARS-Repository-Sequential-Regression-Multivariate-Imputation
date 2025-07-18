rm(list = ls())

library(ggplot2)
library(dplyr)

cc_est_no_prompt_by_dp <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_no_prompt_by_dp.csv"))
cc_est_high_effort_prompt_by_dp <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_high_effort_prompt_by_dp.csv"))
cc_est_low_effort_prompt_by_dp <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_low_effort_prompt_by_dp.csv"))

dat_all_pool_stats_no_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_no_prompt_by_dp.csv"))
dat_all_pool_stats_high_effort_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_high_effort_prompt_by_dp.csv"))
dat_all_pool_stats_low_effort_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_low_effort_prompt_by_dp.csv"))

dat_ppc_no_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_no_prompt_by_dp.csv"))
dat_ppc_high_effort_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_high_effort_prompt_by_dp.csv"))
dat_ppc_low_effort_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_low_effort_prompt_by_dp.csv"))

###############################################################################
# Visualize discrepancy between CC and MI estimates of the mean of the
# proximal outcome at each decision point
###############################################################################

cc_results <- data.frame(where_from = "CC", 
                         decision_point = c(7:54, 7:54, 7:54),
                         what = c(rep("(a) More Effortful Prompt", 48), 
                                  rep("(b) Low Effort Prompt", 48), 
                                  rep("(c) No Prompt", 48)), 
                         est = c(cc_est_high_effort_prompt_by_dp$est, cc_est_low_effort_prompt_by_dp$est, cc_est_no_prompt_by_dp$est), 
                         conf_int_lb = c(cc_est_high_effort_prompt_by_dp$conf_int_lb, cc_est_low_effort_prompt_by_dp$conf_int_lb, cc_est_no_prompt_by_dp$conf_int_lb), 
                         conf_int_ub = c(cc_est_high_effort_prompt_by_dp$conf_int_ub, cc_est_low_effort_prompt_by_dp$conf_int_ub, cc_est_no_prompt_by_dp$conf_int_ub))

mi_results <- data.frame(where_from = "MI", 
                         decision_point = c(7:54, 7:54, 7:54),
                         what = c(rep("(a) More Effortful Prompt", 48), 
                                  rep("(b) Low Effort Prompt", 48), 
                                  rep("(c) No Prompt", 48)), 
                         est = c(dat_all_pool_stats_high_effort_prompt_by_dp$Qbar, dat_all_pool_stats_low_effort_prompt_by_dp$Qbar, dat_all_pool_stats_no_prompt_by_dp$Qbar), 
                         conf_int_lb = c(dat_all_pool_stats_high_effort_prompt_by_dp$conf_int_lb, dat_all_pool_stats_low_effort_prompt_by_dp$conf_int_lb, dat_all_pool_stats_no_prompt_by_dp$conf_int_lb), 
                         conf_int_ub = c(dat_all_pool_stats_high_effort_prompt_by_dp$conf_int_ub, dat_all_pool_stats_low_effort_prompt_by_dp$conf_int_ub, dat_all_pool_stats_no_prompt_by_dp$conf_int_ub))

all_results <- rbind(cc_results, mi_results)

group_colors <- c(CC = "black", MI = "blue")
group_colors_fill <- c(CC = "grey50", MI = "skyblue")

ggplot(all_results, aes(x = decision_point, y = est, ymin = conf_int_lb, ymax = conf_int_ub, color = where_from, fill = where_from)) +
  scale_y_continuous(name = "Self-Efficacy", limits = c(0,4), breaks = seq(0,4,0.5)) +
  scale_x_continuous(name = "Decision Point", limits = c(6,54), breaks = seq(6,54,6)) + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  scale_color_manual(values=group_colors) +
  scale_fill_manual(values=group_colors_fill) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  facet_grid(~ what) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0)) +
  theme(strip.text.y = element_text(size = 18, colour = "black", angle = 0))

ggsave(filename = file.path("plot-cc-and-mi-together", "time_specific_means_CI90.png"), width = 20, height = 12, units = "in", dpi = 1000)

ggplot(all_results, aes(x = decision_point, y = est, ymin = conf_int_lb, ymax = conf_int_ub, color = where_from, fill = where_from)) +
  scale_y_continuous(name = "Self-Efficacy", limits = c(0,4), breaks = seq(0,4,0.5)) +
  scale_x_continuous(name = "Decision Point", limits = c(6,54), breaks = seq(6,54,6)) + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  scale_color_manual(values=group_colors) +
  scale_fill_manual(values=group_colors_fill) +
  geom_smooth(linewidth = 1, se = TRUE, span = 0.3, level = 0.90) +
  facet_grid(~ what) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0)) +
  theme(strip.text.y = element_text(size = 18, colour = "black", angle = 0))

ggsave(filename = file.path("plot-cc-and-mi-together", "time_specific_means_CI90_loess.png"), width = 20, height = 6, units = "in", dpi = 1000)

ppc_results <- data.frame(where_from = "MI", 
                          decision_point = c(7:54, 7:54, 7:54),
                          what = c(rep("(a) More Effortful Prompt", 48), 
                                   rep("(b) Low Effort Prompt", 48), 
                                   rep("(c) No Prompt", 48)), 
                          ppc_est = c(dat_ppc_high_effort_prompt_by_dp$ppc_est, dat_ppc_low_effort_prompt_by_dp$ppc_est, dat_ppc_no_prompt_by_dp$ppc_est),
                          fmi = c(dat_all_pool_stats_high_effort_prompt_by_dp$gamma, dat_all_pool_stats_low_effort_prompt_by_dp$gamma, dat_all_pool_stats_no_prompt_by_dp$gamma))

ggplot(ppc_results, aes(x = decision_point, y = ppc_est)) +
  scale_y_continuous(name = "Posterior Predictive P-Value", limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_x_continuous(name = "Decision Point", limits = c(6,54), breaks = seq(6,54,6)) + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  facet_grid(~ what) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0)) +
  theme(strip.text.y = element_text(size = 18, colour = "black", angle = 0)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", linewidth = 1, colour = "red") +
  geom_hline(yintercept = 0.05, linetype = "dashed", linewidth = 1, colour = "red")

ggsave(filename = file.path("plot-cc-and-mi-together", "ppc_time_specific_means.png"), width = 20, height = 6, units = "in", dpi = 1000)

ggplot(ppc_results, aes(x = decision_point, y = fmi)) +
  scale_y_continuous(name = "Fraction of Missing Information", limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_x_continuous(name = "Decision Point", limits = c(6,54), breaks = seq(6,54,6)) + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  facet_grid(~ what) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0)) +
  theme(strip.text.y = element_text(size = 18, colour = "black", angle = 0)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", linewidth = 1, colour = "red")

ggsave(filename = file.path("plot-cc-and-mi-together", "fmi_time_specific_means.png"), width = 20, height = 6, units = "in", dpi = 1000)

###############################################################################
# What is the discrepancy between CC and MI estimates of the mean of the
# proximal outcome at each decision point?
###############################################################################

merged_est_no_prompt_by_dp <- left_join(x = cc_est_no_prompt_by_dp, y = dat_all_pool_stats_no_prompt_by_dp, by = join_by(decision_point == decision_point))
merged_est_high_effort_prompt_by_dp <- left_join(x = cc_est_high_effort_prompt_by_dp, y = dat_all_pool_stats_high_effort_prompt_by_dp, by = join_by(decision_point == decision_point))
merged_est_low_effort_prompt_by_dp <- left_join(x = cc_est_low_effort_prompt_by_dp, y = dat_all_pool_stats_low_effort_prompt_by_dp, by = join_by(decision_point == decision_point))

merged_est_no_prompt_by_dp <- merged_est_no_prompt_by_dp %>%
  mutate(discrepancy = est - Qbar)

merged_est_high_effort_prompt_by_dp <- merged_est_high_effort_prompt_by_dp %>%
  mutate(discrepancy = est - Qbar)

merged_est_low_effort_prompt_by_dp <- merged_est_low_effort_prompt_by_dp %>%
  mutate(discrepancy = est - Qbar)

all_results <- data.frame(decision_point = c(7:54, 7:54, 7:54),
                          what = c(rep("(a) More Effortful Prompt", 48), 
                          rep("(b) Low Effort Prompt", 48), 
                          rep("(c) No Prompt", 48)), 
                          discrepancy = c(merged_est_high_effort_prompt_by_dp$discrepancy,
                                          merged_est_low_effort_prompt_by_dp$discrepancy,
                                          merged_est_no_prompt_by_dp$discrepancy))

ggplot(all_results, aes(x = decision_point, y = discrepancy)) +
  scale_y_continuous(name = "Discrepancy (CC estimate minus MI estimate)", limits = c(-1,1), breaks = seq(-1,1,0.2)) +
  scale_x_continuous(name = "Decision Point", limits = c(6,54), breaks = seq(6,54,6)) + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, colour = "red") +
  facet_grid(~ what) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0)) +
  theme(strip.text.y = element_text(size = 18, colour = "black", angle = 0))

ggsave(filename = file.path("plot-cc-and-mi-together", "discrepancy_time_specific_means.png"), width = 20, height = 10, units = "in", dpi = 1000)

if(file.exists("plot-cc-and-mi-together/Thumbs.db")){
  file.remove("plot-cc-and-mi-together/Thumbs.db")
}

