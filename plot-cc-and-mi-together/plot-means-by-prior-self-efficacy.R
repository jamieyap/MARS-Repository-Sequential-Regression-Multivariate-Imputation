rm(list = ls())

library(ggplot2)

cc_est_no_prompt_by_dp <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_no_prompt_by_prior_self_efficacy.csv"))
cc_est_high_effort_prompt_by_dp <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_high_effort_prompt_by_prior_self_efficacy.csv"))
cc_est_low_effort_prompt_by_dp <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_low_effort_prompt_by_prior_self_efficacy.csv"))

dat_all_pool_stats_no_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_no_prompt_by_prior_self_efficacy.csv"))
dat_all_pool_stats_high_effort_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_high_effort_prompt_by_prior_self_efficacy.csv"))
dat_all_pool_stats_low_effort_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_low_effort_prompt_by_prior_self_efficacy.csv"))

dat_ppc_no_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_no_prompt_by_prior_self_efficacy.csv"))
dat_ppc_high_effort_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_high_effort_prompt_by_prior_self_efficacy.csv"))
dat_ppc_low_effort_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_low_effort_prompt_by_prior_self_efficacy.csv"))

cc_results <- data.frame(where_from = "CC", 
                         decision_point = c(0:4, 0:4, 0:4),
                         what = c(rep("(a) More Effortful Prompt", 5), 
                                  rep("(b) Low Effort Prompt", 5), 
                                  rep("(c) No Prompt", 5)), 
                         est = c(cc_est_high_effort_prompt_by_dp$est, cc_est_low_effort_prompt_by_dp$est, cc_est_no_prompt_by_dp$est), 
                         conf_int_lb = c(cc_est_high_effort_prompt_by_dp$conf_int_lb, cc_est_low_effort_prompt_by_dp$conf_int_lb, cc_est_no_prompt_by_dp$conf_int_lb), 
                         conf_int_ub = c(cc_est_high_effort_prompt_by_dp$conf_int_ub, cc_est_low_effort_prompt_by_dp$conf_int_ub, cc_est_no_prompt_by_dp$conf_int_ub))

mi_results <- data.frame(where_from = "MI", 
                         decision_point = c(0:4, 0:4, 0:4),
                         what = c(rep("(a) More Effortful Prompt", 5), 
                                  rep("(b) Low Effort Prompt", 5), 
                                  rep("(c) No Prompt", 5)), 
                         est = c(dat_all_pool_stats_high_effort_prompt_by_dp$Qbar, dat_all_pool_stats_low_effort_prompt_by_dp$Qbar, dat_all_pool_stats_no_prompt_by_dp$Qbar), 
                         conf_int_lb = c(dat_all_pool_stats_high_effort_prompt_by_dp$conf_int_lb, dat_all_pool_stats_low_effort_prompt_by_dp$conf_int_lb, dat_all_pool_stats_no_prompt_by_dp$conf_int_lb), 
                         conf_int_ub = c(dat_all_pool_stats_high_effort_prompt_by_dp$conf_int_ub, dat_all_pool_stats_low_effort_prompt_by_dp$conf_int_ub, dat_all_pool_stats_no_prompt_by_dp$conf_int_ub))

all_results <- rbind(cc_results, mi_results)

group_colors <- c(CC = "black", MI = "blue")
group_colors_fill <- c(CC = "grey50", MI = "skyblue")

ggplot(all_results, aes(x = decision_point, y = est, ymin = conf_int_lb, ymax = conf_int_ub, color = where_from, fill = where_from)) +
  scale_y_continuous(name = "Self-Efficacy", limits = c(0,4), breaks = seq(0,4,0.5)) +
  scale_x_continuous(name = "Prior Self-efficacy", limits = c(0,4), breaks = 0:4) + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  scale_color_manual(values=group_colors) +
  scale_fill_manual(values=group_colors_fill) +
  geom_ribbon(alpha = 0.5, color = NA) +
  geom_line(linewidth = 1) + geom_point(size = 4) +
  facet_grid(where_from ~ what) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0)) +
  theme(strip.text.y = element_text(size = 18, colour = "black", angle = 0))

ggsave(filename = file.path("plot-cc-and-mi-together", "means_by_prior_self_efficacy_CI90.png"), width = 20, height = 12, units = "in", dpi = 1000)

ggplot(all_results, aes(x = decision_point, y = est, ymin = conf_int_lb, ymax = conf_int_ub, color = where_from, fill = where_from)) +
  scale_y_continuous(name = "Self-Efficacy", limits = c(0,6), breaks = seq(0,4,0.5)) +
  scale_x_continuous(name = "Prior Self-efficacy", limits = c(0,4), breaks = 0:4) + 
  coord_cartesian(ylim = c(0,4), xlim = c(0,4)) +
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  scale_color_manual(values=group_colors) +
  scale_fill_manual(values=group_colors_fill) +
  geom_smooth(linewidth = 1, se = TRUE, span = 1.2, level = 0.90) +
  facet_grid(~ what) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0)) +
  theme(strip.text.y = element_text(size = 18, colour = "black", angle = 0))

ggsave(filename = file.path("plot-cc-and-mi-together", "means_by_prior_self_efficacy_CI90_loess.png"), width = 20, height = 6, units = "in", dpi = 1000)

ppc_results <- data.frame(where_from = "MI", 
                          decision_point = c(0:4, 0:4, 0:4),
                          what = c(rep("(a) More Effortful Prompt", 5), 
                                   rep("(b) Low Effort Prompt", 5), 
                                   rep("(c) No Prompt", 5)), 
                          ppc_est = c(dat_ppc_high_effort_prompt_by_dp$ppc_est, dat_ppc_low_effort_prompt_by_dp$ppc_est, dat_ppc_no_prompt_by_dp$ppc_est),
                          fmi = c(dat_all_pool_stats_high_effort_prompt_by_dp$gamma, dat_all_pool_stats_low_effort_prompt_by_dp$gamma, dat_all_pool_stats_no_prompt_by_dp$gamma))

ggplot(ppc_results, aes(x = decision_point, y = ppc_est)) +
  scale_y_continuous(name = "Posterior Predictive P-Value", limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_x_continuous(name = "Prior Self-Efficacy", limits = c(0,4), breaks = seq(0,4,1)) + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_line(linewidth = 1) + geom_point(size = 4) +
  facet_grid(~ what) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0)) +
  theme(strip.text.y = element_text(size = 18, colour = "black", angle = 0)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", linewidth = 1, colour = "red") +
  geom_hline(yintercept = 0.05, linetype = "dashed", linewidth = 1, colour = "red")

ggsave(filename = file.path("plot-cc-and-mi-together", "ppc_means_by_prior_self_efficacy.png"), width = 20, height = 6, units = "in", dpi = 1000)

ggplot(ppc_results, aes(x = decision_point, y = fmi)) +
  scale_y_continuous(name = "Fraction of Missing Information", limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_x_continuous(name = "Prior Self-Efficacy", limits = c(0,4), breaks = seq(0,4,1)) + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_line(linewidth = 1) + geom_point(size = 4) +
  facet_grid(~ what) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0)) +
  theme(strip.text.y = element_text(size = 18, colour = "black", angle = 0)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", linewidth = 1, colour = "red")

ggsave(filename = file.path("plot-cc-and-mi-together", "fmi_means_by_prior_self_efficacy.png"), width = 20, height = 6, units = "in", dpi = 1000)

if(file.exists("plot-cc-and-mi-together/Thumbs.db")){
  file.remove("plot-cc-and-mi-together/Thumbs.db")
}

