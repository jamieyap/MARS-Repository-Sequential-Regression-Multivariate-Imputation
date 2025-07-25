###############################################################################
# Workflow: Marginal Effect
###############################################################################

rm(list = ls())

source("paths.R")

library(ggplot2)

cc_marginal <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "primary_marginal.csv"))
mi_marginal <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_primary_causal.csv"))

group_colors <- c(CC = "black", MI = "blue")

cc_results <- data.frame(where_from = "CC", est = cc_marginal$Estimate[1], lb = cc_marginal$LCL90[1], ub = cc_marginal$UCL90[1])
mi_results <- data.frame(where_from = "MI", est = mi_marginal$Estimate[1], lb = mi_marginal$LCL90[1], ub = mi_marginal$UCL90[1])

all_results <- rbind(cc_results, mi_results)

ggplot(all_results, aes(x = where_from, y = est, color = where_from)) +
  geom_point(size = 10) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.4, linewidth = 1) +
  scale_y_continuous(name = "Estimated Difference in Means", limits = c(-0.5,0.5), breaks = seq(-0.5,0.5,0.10)) +
  scale_x_discrete(name = "") + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_text(aes(label=round(est,3)), hjust = -0.4, size=8) +
  scale_color_manual(values=group_colors) +
  ggtitle("Prompt vs. No Prompt") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, colour = "red")

ggsave(filename = file.path("plot-cc-and-mi-together", "primary_marginal_CI90.png"), width = 6, height = 8, units = "in", dpi = 1000)

if(file.exists("plot-cc-and-mi-together/Thumbs.db")){
  file.remove("plot-cc-and-mi-together/Thumbs.db")
}

# cc_linear_day <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "primary_study_day_linear.csv"))
# cc_quadratic_day <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "primary_study_day_quadratic.csv"))
# cc_moderator <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "primary_moderator.csv"))
# 
# mi_linear_day <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_primary_causal_study_day_linear.csv"))
# mi_quadratic_day <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_primary_causal_study_day_quadratic.csv"))
# mi_moderator <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_primary_causal_moderator.csv"))
# 

###############################################################################
# Workflow: Moderator - study day quadratic
###############################################################################

rm(list = ls())

source("paths.R")

library(ggplot2)

cc_moderator <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "primary_study_day_quadratic.csv"))
mi_moderator <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_primary_causal_study_day_quadratic.csv"))

group_colors <- c(CC = "black", MI = "blue")
group_colors_fill <- c(CC = "grey50", MI = "skyblue")

cc_results <- data.frame(where_from = "CC", 
                         what = c("Prompt vs. No Prompt"), 
                         est = cc_moderator$Estimate[c(3)], lb = cc_moderator$LCL90[c(3)], ub = cc_moderator$UCL90[c(3)])

mi_results <- data.frame(where_from = "MI", 
                         what = c("Prompt vs. No Prompt"),
                         est = mi_moderator$Estimate[c(3)], lb = mi_moderator$LCL90[c(3)], ub = mi_moderator$UCL90[c(3)])

all_results <- rbind(cc_results, mi_results)

ggplot(all_results, aes(x = where_from, y = est, color = where_from)) +
  geom_point(size = 10) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.4, linewidth = 1) +
  scale_y_continuous(name = "Interaction estimate", limits = c(-0.2,0.2), breaks = seq(-0.2,0.2,0.05)) +
  scale_x_discrete(name = "") + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_text(aes(label=round(est,3)), hjust = -0.4, size=8) +
  scale_color_manual(values=group_colors) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, colour = "red") +
  ggtitle("") +
  facet_wrap(~what) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0))

ggsave(filename = file.path("plot-cc-and-mi-together", "primary_study_day_quadratic_CI90.png"), width = 6, height = 8, units = "in", dpi = 1000)

rm(list = ls())

source("paths.R")

library(ggplot2)

cc_moderator <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "primary_study_day_quadratic.csv"))
mi_moderator <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_primary_causal_study_day_quadratic.csv"))

group_colors <- c(CC = "black", MI = "blue")
group_colors_fill <- c(CC = "grey50", MI = "skyblue")

cc_results <- data.frame(where_from = "CC", 
                         response = c(seq(1,8),
                                      seq(1,8),
                                      seq(1,8)),
                         what = c(rep("Prompt vs. No Prompt", 8)), 
                         est = cc_moderator$Estimate[4:11], lb = cc_moderator$LCL90[4:11], ub = cc_moderator$UCL90[4:11])

mi_results <- data.frame(where_from = "MI", 
                         response = c(seq(1:8),
                                      seq(1:8),
                                      seq(1:8)),
                         what = c(rep("Prompt vs. No Prompt", 8)), 
                         est = mi_moderator$Estimate[4:11], lb = mi_moderator$LCL90[4:11], ub = mi_moderator$UCL90[4:11])

all_results <- rbind(cc_results, mi_results)

ggplot(all_results, aes(x = response, y = est, ymin = lb, ymax = ub, color = where_from, fill = where_from)) +
  scale_y_continuous(name = "Estimated Difference in Means", limits = c(-0.75,0.75), breaks = seq(-0.75,0.75,0.25)) +
  scale_x_continuous(name = "Day since start of MRT", limits = c(1,8), breaks = seq(1,8,1)) + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  scale_fill_manual(values=group_colors_fill) +
  geom_ribbon(alpha = 0.5, color = NA) +
  scale_color_manual(values=group_colors) +
  geom_line(linewidth = 1) + geom_point(size = 8) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, colour = "red") +
  ggtitle("") +
  facet_grid(where_from ~ what) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0),
        strip.text.y = element_text(size = 18, colour = "black", angle = 0))

ggsave(filename = file.path("plot-cc-and-mi-together", "primary_study_day_quadratic_contrasts_CI90.png"), width = 8, height = 8, units = "in", dpi = 1000)

if(file.exists("plot-cc-and-mi-together/Thumbs.db")){
  file.remove("plot-cc-and-mi-together/Thumbs.db")
}
