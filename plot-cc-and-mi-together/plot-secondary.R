###############################################################################
# Workflow: Marginal Effect
###############################################################################

rm(list = ls())

source("paths.R")

library(ggplot2)

cc_marginal <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "secondary_marginal.csv"))
mi_marginal <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_secondary_causal.csv"))

group_colors <- c(CC = "black", MI = "blue")

cc_results <- data.frame(where_from = "CC", 
                         what = c("(ii) More Effortful Prompt vs. No Prompt", 
                                  "(iii) Low Effort Prompt vs. No Prompt", 
                                  "(i) More Effortful Prompt vs. Low Effort Prompt"), 
                         est = cc_marginal$Estimate[4:6], lb = cc_marginal$LCL90[4:6], ub = cc_marginal$UCL90[4:6])

mi_results <- data.frame(where_from = "MI", 
                         what = c("(ii) More Effortful Prompt vs. No Prompt", 
                                  "(iii) Low Effort Prompt vs. No Prompt", 
                                  "(i) More Effortful Prompt vs. Low Effort Prompt"),
                         est = mi_marginal$Estimate, lb = mi_marginal$LCL90, ub = mi_marginal$UCL90)

all_results <- rbind(cc_results, mi_results)

ggplot(all_results, aes(x = where_from, y = est, color = where_from)) +
  geom_point(size = 10) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.4, linewidth = 1) +
  scale_y_continuous(name = "Estimated Difference in Means", limits = c(-0.5,0.5), breaks = seq(-0.5,0.5,0.10)) +
  scale_x_discrete(name = "") + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_text(aes(label=round(est,3)), hjust = -0.4, size=8) +
  scale_color_manual(values=group_colors) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, colour = "red") +
  ggtitle("") +
  facet_wrap(~what) +
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0))

ggsave(filename = file.path("plot-cc-and-mi-together", "secondary_marginal_CI90.png"), width = 20, height = 8, units = "in", dpi = 1000)

if(file.exists("plot-cc-and-mi-together/Thumbs.db")){
  file.remove("plot-cc-and-mi-together/Thumbs.db")
}

###############################################################################
# Workflow: Moderator - prior self-efficacy
###############################################################################

rm(list = ls())

source("paths.R")

library(ggplot2)

cc_moderator <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "secondary_moderator.csv"))
mi_moderator <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_secondary_causal_moderator.csv"))

group_colors <- c(CC = "black", MI = "blue")
group_colors_fill <- c(CC = "grey50", MI = "skyblue")

cc_results <- data.frame(where_from = "CC", 
                         what = c("(ii) More Effortful Prompt vs. No Prompt", 
                                  "(iii) Low Effort Prompt vs. No Prompt", 
                                  "(i) More Effortful Prompt vs. Low Effort Prompt"), 
                         est = cc_moderator$Estimate[c(6,7,9)], lb = cc_moderator$LCL90[c(6,7,9)], ub = cc_moderator$UCL90[c(6,7,9)])

mi_results <- data.frame(where_from = "MI", 
                         what = c("(ii) More Effortful Prompt vs. No Prompt", 
                                  "(iii) Low Effort Prompt vs. No Prompt", 
                                  "(i) More Effortful Prompt vs. Low Effort Prompt"),
                         est = mi_moderator$Estimate[c(3,4,6)], lb = mi_moderator$LCL90[c(3,4,6)], ub = mi_moderator$UCL90[c(3,4,6)])

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


ggsave(filename = file.path("plot-cc-and-mi-together", "secondary_moderator_CI90.png"), width = 20, height = 8, units = "in", dpi = 1000)

rm(list = ls())

source("paths.R")

library(ggplot2)

cc_moderator <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "secondary_moderator.csv"))
mi_moderator <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_secondary_causal_moderator.csv"))

group_colors <- c(CC = "black", MI = "blue")
group_colors_fill <- c(CC = "grey50", MI = "skyblue")

cc_results <- data.frame(where_from = "CC", 
                         response = c(seq(0,4),
                                      seq(0,4),
                                      seq(0,4)),
                         what = c(rep("(i) More Effortful Prompt vs. Low Effort Prompt", 5),
                                  rep("(iii) Low Effort Prompt vs. No Prompt", 5),
                                  rep("(ii) More Effortful Prompt vs. No Prompt", 5)), 
                         est = cc_moderator$Estimate[10:24], lb = cc_moderator$LCL90[10:24], ub = cc_moderator$UCL90[10:24])

mi_results <- data.frame(where_from = "MI", 
                         response = c(seq(0,4),
                                      seq(0,4),
                                      seq(0,4)),
                         what = c(rep("(i) More Effortful Prompt vs. Low Effort Prompt", 5),
                                  rep("(iii) Low Effort Prompt vs. No Prompt", 5),
                                  rep("(ii) More Effortful Prompt vs. No Prompt", 5)), 
                         est = mi_moderator$Estimate[7:21], lb = mi_moderator$LCL90[7:21], ub = mi_moderator$UCL90[7:21])

all_results <- rbind(cc_results, mi_results)

ggplot(all_results, aes(x = response, y = est, ymin = lb, ymax = ub, color = where_from, fill = where_from)) +
  scale_y_continuous(name = "Estimated Difference in Means", limits = c(-0.75,0.75), breaks = seq(-0.75,0.75,0.25)) +
  scale_x_continuous(name = "Prior Self-Efficacy", limits = c(0,4), breaks = c(0,1,2,3,4)) + 
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

ggsave(filename = file.path("plot-cc-and-mi-together", "secondary_moderator_contrasts_CI90.png"), width = 20, height = 12, units = "in", dpi = 1000)

if(file.exists("plot-cc-and-mi-together/Thumbs.db")){
  file.remove("plot-cc-and-mi-together/Thumbs.db")
}

###############################################################################
# Workflow: Moderator - study day quadratic
###############################################################################

rm(list = ls())

source("paths.R")

library(ggplot2)

cc_moderator <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "secondary_study_day_quadratic.csv"))
mi_moderator <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_secondary_causal_study_day_quadratic.csv"))

group_colors <- c(CC = "black", MI = "blue")
group_colors_fill <- c(CC = "grey50", MI = "skyblue")

cc_results <- data.frame(where_from = "CC", 
                         what = c("(ii) More Effortful Prompt vs. No Prompt", 
                                  "(iii) Low Effort Prompt vs. No Prompt", 
                                  "(i) More Effortful Prompt vs. Low Effort Prompt"), 
                         est = cc_moderator$Estimate[c(9,10,13)], lb = cc_moderator$LCL90[c(9,10,13)], ub = cc_moderator$UCL90[c(9,10,13)])

mi_results <- data.frame(where_from = "MI", 
                         what = c("(ii) More Effortful Prompt vs. No Prompt", 
                                  "(iii) Low Effort Prompt vs. No Prompt", 
                                  "(i) More Effortful Prompt vs. Low Effort Prompt"),
                         est = mi_moderator$Estimate[c(5,6,9)], lb = mi_moderator$LCL90[c(5,6,9)], ub = mi_moderator$UCL90[c(5,6,9)])

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

ggsave(filename = file.path("plot-cc-and-mi-together", "secondary_study_day_quadratic_CI90.png"), width = 20, height = 8, units = "in", dpi = 1000)

rm(list = ls())

source("paths.R")

library(ggplot2)

cc_moderator <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "secondary_study_day_quadratic.csv"))
mi_moderator <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_secondary_causal_study_day_quadratic.csv"))

group_colors <- c(CC = "black", MI = "blue")
group_colors_fill <- c(CC = "grey50", MI = "skyblue")

cc_results <- data.frame(where_from = "CC", 
                         response = c(seq(1,8),
                                      seq(1,8),
                                      seq(1,8)),
                         what = c(rep("(i) More Effortful Prompt vs. Low Effort Prompt", 8),
                                  rep("(iii) Low Effort Prompt vs. No Prompt", 8),
                                  rep("(ii) More Effortful Prompt vs. No Prompt", 8)), 
                         est = cc_moderator$Estimate[14:37], lb = cc_moderator$LCL90[14:37], ub = cc_moderator$UCL90[14:37])

mi_results <- data.frame(where_from = "MI", 
                         response = c(seq(1:8),
                                      seq(1:8),
                                      seq(1:8)),
                         what = c(rep("(i) More Effortful Prompt vs. Low Effort Prompt", 8),
                                  rep("(iii) Low Effort Prompt vs. No Prompt", 8),
                                  rep("(ii) More Effortful Prompt vs. No Prompt", 8)), 
                         est = mi_moderator$Estimate[10:33], lb = mi_moderator$LCL90[10:33], ub = mi_moderator$UCL90[10:33])

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

ggsave(filename = file.path("plot-cc-and-mi-together", "secondary_study_day_quadratic_contrasts_CI90.png"), width = 20, height = 12, units = "in", dpi = 1000)

if(file.exists("plot-cc-and-mi-together/Thumbs.db")){
  file.remove("plot-cc-and-mi-together/Thumbs.db")
}



