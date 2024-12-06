################################################################################
# Load packages
################################################################################
rm(list = ls())
source("paths.R")
library(tidyverse)

################################################################################
# Load curated datasets
################################################################################
dat_master_ema_questions <- readRDS(file = file.path(path_manipulated_data, "dat_master_ema_questions.rds"))
dat_master_ema_response_options <- readRDS(file = file.path(path_manipulated_data, "dat_master_ema_response_options.rds"))
dat_matched_to_decision_points <- readRDS(file = file.path(path_manipulated_data, "dat_matched_to_decision_points.rds"))
mars_ids_excluded_from_all_analytic_datasets <- readRDS(file = file.path(path_manipulated_data, "mars_ids_excluded_from_all_analytic_datasets.rds"))
scanned_decision_points_within_range <- readRDS(file = file.path(path_manipulated_data, "scanned_decision_points_within_range.rds"))
dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds"))

dat_analysis <- dat_matched_to_decision_points %>% filter(!(mars_id %in% mars_ids_excluded_from_all_analytic_datasets))
all_ids <- unique(dat_analysis[["mars_id"]])
n_ids <- length(all_ids)
dat_analysis <- left_join(x = dat_analysis,
                          y = scanned_decision_points_within_range,
                          by = join_by(mars_id == mars_id, decision_point == decision_point))

################################################################################
# Keep track of column names you will want to keep
################################################################################
keep_these_columns_for_analysis <- list()

################################################################################
# Create new variables
################################################################################

# -----------------------------------------------------------------------------
# Q32. RIGHT NOW, I am motivated to be abstinent from cigarettes
# -----------------------------------------------------------------------------
column_names_original_items <- c("Q32_response")
lookup <- c(motivation_cig = "Q32_response_cleaned")

dat_analysis <- dat_analysis %>%
  mutate(across(.cols = all_of(column_names_original_items), 
                .fns = ~ case_when(
                  .x == "0 Not at all" ~ 0,
                  .x == "1" ~ 1,
                  .x == "2" ~ 2,
                  .x == "3" ~ 3,
                  .x == "4 Extremely" ~ 4,
                  .default = NULL
                ),
                .names = "{.col}_cleaned")
  )

dat_analysis <- rename(dat_analysis, all_of(lookup))
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

# -----------------------------------------------------------------------------
# Q33. RIGHT NOW, I am motivated to be abstinent from vape pen/e-cigarette/JUUL
# -----------------------------------------------------------------------------
column_names_original_items <- c("Q33_response")
lookup <- c(motivation_vape = "Q33_response_cleaned")

dat_analysis <- dat_analysis %>%
  mutate(across(.cols = all_of(column_names_original_items), 
                .fns = ~ case_when(
                  .x == "0 Not at all" ~ 0,
                  .x == "1" ~ 1,
                  .x == "2" ~ 2,
                  .x == "3" ~ 3,
                  .x == "4 Extremely" ~ 4,
                  .default = NULL
                ),
                .names = "{.col}_cleaned")
  )

dat_analysis <- rename(dat_analysis, all_of(lookup))
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

# -----------------------------------------------------------------------------
# Q39. RIGHT NOW, I am confident in my ability to be abstinent from cigarettes
# -----------------------------------------------------------------------------
column_names_original_items <- c("Q39_response")
lookup <- c(self_efficacy_cig = "Q39_response_cleaned")

dat_analysis <- dat_analysis %>%
  mutate(across(.cols = all_of(column_names_original_items), 
                .fns = ~ case_when(
                  .x == "0 Not at all" ~ 0,
                  .x == "1" ~ 1,
                  .x == "2" ~ 2,
                  .x == "3" ~ 3,
                  .x == "4 Extremely" ~ 4,
                  .default = NULL
                ),
                .names = "{.col}_cleaned")
  )

dat_analysis <- rename(dat_analysis, all_of(lookup))
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

# -----------------------------------------------------------------------------
# Q40. RIGHT NOW, I am confident in my ability to be abstinent from vape pen/
# e-cigarette-JUUL use
# -----------------------------------------------------------------------------
column_names_original_items <- c("Q40_response")
lookup <- c(self_efficacy_vape = "Q40_response_cleaned")

dat_analysis <- dat_analysis %>%
  mutate(across(.cols = all_of(column_names_original_items), 
                .fns = ~ case_when(
                  .x == "0 Not at all" ~ 0,
                  .x == "1" ~ 1,
                  .x == "2" ~ 2,
                  .x == "3" ~ 3,
                  .x == "4 Extremely" ~ 4,
                  .default = NULL
                ),
                .names = "{.col}_cleaned")
  )

dat_analysis <- rename(dat_analysis, all_of(lookup))
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

# -----------------------------------------------------------------------------
# Q36. RIGHT NOW, Smoking a cigarette would improve my mood, 
# make me feel good, or help me cope with this situation.
# -----------------------------------------------------------------------------
column_names_original_items <- c("Q36_response")
lookup <- c(expectancy_cig = "Q36_response_cleaned")

dat_analysis <- dat_analysis %>%
  mutate(across(.cols = all_of(column_names_original_items), 
                .fns = ~ case_when(
                  .x == "0 Not at all" ~ 0,
                  .x == "1" ~ 1,
                  .x == "2" ~ 2,
                  .x == "3" ~ 3,
                  .x == "4 Very much" ~ 4,
                  .default = NULL
                ),
                .names = "{.col}_cleaned")
  )

dat_analysis <- rename(dat_analysis, all_of(lookup))
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

# -----------------------------------------------------------------------------
# Q37. RIGHT NOW, Using a vape pen/e-cigarette/JUUL would improve my mood, 
# make me feel good, or help me cope with this situation
# -----------------------------------------------------------------------------
column_names_original_items <- c("Q37_response")
lookup <- c(expectancy_vape = "Q37_response_cleaned")

dat_analysis <- dat_analysis %>%
  mutate(across(.cols = all_of(column_names_original_items), 
                .fns = ~ case_when(
                  .x == "0 Not at all" ~ 0,
                  .x == "1" ~ 1,
                  .x == "2" ~ 2,
                  .x == "3" ~ 3,
                  .x == "4 Very much" ~ 4,
                  .default = NULL
                ),
                .names = "{.col}_cleaned")
  )

dat_analysis <- rename(dat_analysis, all_of(lookup))
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

# -----------------------------------------------------------------------------
# Q38. RIGHT NOW, Something other than smoking a cigarette 
# or using a vape pen/e-cigarette/JUUL would improve my mood, 
# make me feel good, or help me cope with this situation
# -----------------------------------------------------------------------------
column_names_original_items <- c("Q38_response")
lookup <- c(expectancy_cig_or_vape = "Q38_response_cleaned")

dat_analysis <- dat_analysis %>%
  mutate(across(.cols = all_of(column_names_original_items), 
                .fns = ~ case_when(
                  .x == "0 Not at all" ~ 0,
                  .x == "1" ~ 1,
                  .x == "2" ~ 2,
                  .x == "3" ~ 3,
                  .x == "4 Very much" ~ 4,
                  .default = NULL
                ),
                .names = "{.col}_cleaned")
  )

dat_analysis <- rename(dat_analysis, all_of(lookup))
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Aggregate over the past 24 hours
################################################################################
lookup <- c(motivation_cig_mean_past24hrs = "motivation_cig_mean_past24hrs",
            motivation_vape_mean_past24hrs = "motivation_vape_mean_past24hrs",
            self_efficacy_cig_mean_past24hrs = "self_efficacy_cig_mean_past24hrs",
            self_efficacy_vape_mean_past24hrs = "self_efficacy_vape_mean_past24hrs",
            expectancy_cig_mean_past24hrs = "expectancy_cig_mean_past24hrs",
            expectancy_vape_mean_past24hrs = "expectancy_vape_mean_past24hrs",
            expectancy_cig_or_vape_mean_past24hrs = "expectancy_cig_or_vape_mean_past24hrs")

list_all_dat <- list()
dat_analysis[["motivation_cig_mean_past24hrs"]] <- NA
dat_analysis[["motivation_vape_mean_past24hrs"]] <- NA
dat_analysis[["self_efficacy_cig_mean_past24hrs"]] <- NA
dat_analysis[["self_efficacy_vape_mean_past24hrs"]] <- NA
dat_analysis[["expectancy_cig_mean_past24hrs"]] <- NA
dat_analysis[["expectancy_vape_mean_past24hrs"]] <- NA
dat_analysis[["expectancy_cig_or_vape_mean_past24hrs"]] <- NA

dat_analysis[["motivation_cig_nreported_past24hrs"]] <- NA
dat_analysis[["motivation_vape_nreported_past24hrs"]] <- NA
dat_analysis[["self_efficacy_cig_nreported_past24hrs"]] <- NA
dat_analysis[["self_efficacy_vape_nreported_past24hrs"]] <- NA
dat_analysis[["expectancy_cig_nreported_past24hrs"]] <- NA
dat_analysis[["expectancy_vape_nreported_past24hrs"]] <- NA
dat_analysis[["expectancy_cig_or_vape_nreported_past24hrs"]] <- NA

for(i in 1:n_ids){
  current_participant <- all_ids[i]
  dat_current_participant <- dat_analysis %>% filter(mars_id == current_participant)
  n_blocks <- nrow(dat_current_participant)
  
  for(j in 1:n_blocks){
    is_rand <- if_else(!is.na(dat_current_participant[j,"ts_coinflip_mountain"]), TRUE, FALSE)
    
    if(is_rand == TRUE){
      n_rand_past24hrs <- dat_current_participant[j,"counts_rand_past24hrs"]
      
      if(n_rand_past24hrs > 0){
        dp_within_range <- dat_current_participant[j,"decision_points_past24hrs"][[1]]
        dat_within_range <- dat_current_participant %>% filter(decision_point %in% dp_within_range)
        
        dat_current_participant[j,"motivation_cig_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["motivation_cig"]]))
        dat_current_participant[j,"motivation_vape_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["motivation_vape"]]))
        dat_current_participant[j,"self_efficacy_cig_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["self_efficacy_cig"]]))
        dat_current_participant[j,"self_efficacy_vape_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["self_efficacy_vape"]]))
        dat_current_participant[j,"expectancy_cig_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["expectancy_cig"]]))
        dat_current_participant[j,"expectancy_vape_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["expectancy_vape"]]))
        dat_current_participant[j,"expectancy_cig_or_vape_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["expectancy_cig_or_vape"]]))
        
        # Note that checking whether number of micro-randomizations is equal to
        # the number of completed EMA will not count partially completed EMAs; 
        # although more verbose to implement in code, 
        # checking each item one at a time is more accurate
        if(n_rand_past24hrs == dat_current_participant[j,"motivation_cig_nreported_past24hrs"]){
          dat_current_participant[j,"motivation_cig_mean_past24hrs"] <- mean(dat_within_range[["motivation_cig"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"motivation_vape_nreported_past24hrs"]){
          dat_current_participant[j,"motivation_vape_mean_past24hrs"] <- mean(dat_within_range[["motivation_vape"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"self_efficacy_cig_nreported_past24hrs"]){
          dat_current_participant[j,"self_efficacy_cig_mean_past24hrs"] <- mean(dat_within_range[["self_efficacy_cig"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"self_efficacy_vape_nreported_past24hrs"]){
          dat_current_participant[j,"self_efficacy_vape_mean_past24hrs"] <- mean(dat_within_range[["self_efficacy_vape"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"expectancy_cig_nreported_past24hrs"]){
          dat_current_participant[j,"expectancy_cig_mean_past24hrs"] <- mean(dat_within_range[["expectancy_cig"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"expectancy_vape_nreported_past24hrs"]){
          dat_current_participant[j,"expectancy_vape_mean_past24hrs"] <- mean(dat_within_range[["expectancy_vape"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"expectancy_cig_or_vape_nreported_past24hrs"]){
          dat_current_participant[j,"expectancy_cig_or_vape_mean_past24hrs"] <- mean(dat_within_range[["expectancy_cig_or_vape"]], na.rm = TRUE)
        }
        
      }  # This if-then statement only executes if a block had any micro-randomization in the past 24 hours PRIOR TO the current micro-randomization
    }  # This if-then statement only executes if a block had a micro-randomization
  } # This loop completes checking each participant-block
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_analysis <- bind_rows(list_all_dat)
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Merge new variables to primary aim analytic dataset
################################################################################
keep_these_columns_for_analysis <- unlist(keep_these_columns_for_analysis)
dat_analysis <- dat_analysis %>% select(mars_id, decision_point, all_of(keep_these_columns_for_analysis))
dat_primary_aim <- left_join(x = dat_primary_aim, y = dat_analysis, by = join_by(mars_id == mars_id, decision_point == decision_point))

################################################################################
# Save
################################################################################
saveRDS(dat_primary_aim, file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_for_new_pipeline.rds"))

