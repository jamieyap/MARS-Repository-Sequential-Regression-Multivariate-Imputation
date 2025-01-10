rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Imputation number
###############################################################################
mi_dataset_num <- .__current_idx  # Change the right hand side of this line if not running within a loop
use_maxit_value <- .__par_maxit_value
st_num <- 3 # The stratum being imputed
which_penalty <- "BIC"  # Can be set to either "AIC" or "BIC"

current_dp_value <- .__current_dp  # Change the right hand side of this line if not running within a loop
suffix <- paste("_dp" ,  current_dp_value, sep = "")
suffix_lag1 <- paste("_dp" ,  current_dp_value - 1, sep = "")

################################################################################
# Load packages
################################################################################
source("paths.R")
library(MASS) # We will use the stepAIC function from the MASS package
library(tidyverse)
library(mice)

# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 

# A function for throwing an error
check_throw_error <- function(x) {
  stopifnot(x == TRUE)
}

###############################################################################
# Set up dataset in preparation for imputation at the current decision point
###############################################################################

# Read in completed dataset from previous time-point
dat_imputed_stratum_01 <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_imputed_stratum1.rds"))
dat_imputed_stratum_02 <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_imputed_stratum2.rds"))

dat_imputed_stratum_01_current_dp <- dat_imputed_stratum_01 %>% filter(decision_point == current_dp_value)
dat_imputed_stratum_02_current_dp <- dat_imputed_stratum_02 %>% filter(decision_point == current_dp_value)
n_participants_meet_sparse_restrictions_stratum_01_current_dp <- nrow(dat_imputed_stratum_01_current_dp)
n_participants_meet_sparse_restrictions_stratum_02_current_dp <- nrow(dat_imputed_stratum_02_current_dp)

# Read in time-varying variables and select the columns pertaining to the current decision point
if(current_dp_value == 2){
  # Grab completed data from baseline
  dat_wide_completed_baseline <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))
  
  # Grab completed data from first decision point
  dat_timevarying_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))
  dat_wide_completed_dp1 <- dat_timevarying_wide %>% select(replicate_id, participant_id, ends_with("_dp1"))
  
  lookup_table <- dat_imputed_stratum_01 %>% 
    filter(decision_point == 1) %>% 
    select(replicate_id, participant_id,
           cigarette_counts, motivation_cig, self_efficacy_cig)
  
  dat_wide_completed_dp1 <- left_join(x = dat_wide_completed_dp1, y = lookup_table, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
  
  dat_wide_completed_dp1 <- dat_wide_completed_dp1 %>% 
    mutate(cigarette_counts_dp1 = cigarette_counts,
           motivation_cig_dp1 = motivation_cig,
           self_efficacy_cig_dp1 = self_efficacy_cig) %>% 
    select(-cigarette_counts, -motivation_cig, -self_efficacy_cig)
  
  # Merge completed data from baseline and the first decision point
  dat_wide_from_prior_step <- left_join(x = dat_wide_completed_baseline, y = dat_wide_completed_dp1, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
}else{
  # Grab completed data from previous decision point
  # No need to read in dat_wide_completed_baseline since this will already have the imputed baseline variables
  dat_wide_from_prior_step <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num,  paste("dat_wide_completed_dp", current_dp_value - 1, ".rds", sep = "")))
}

dat_timevarying_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))
dat_timevarying_wide_current_dp <- dat_timevarying_wide %>% select(replicate_id, participant_id, ends_with(suffix))
dat_wide <- left_join(x = dat_wide_from_prior_step, y = dat_timevarying_wide_current_dp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))

###############################################################################
# Step 0. Update completed dataset -- number of cigarettes smoked
# reported at the prior decision point
###############################################################################
dat_wide[[paste("cigarette_counts_lag1", suffix, sep = "")]] <- dat_wide[[paste("cigarette_counts", suffix_lag1, sep = "")]]
dat_wide[[paste("cigarette_counts_lag1", suffix, sep = "")]] <- if_else(dat_wide[[paste("eligibility_lag1", suffix, sep = "")]] == 0, -1, dat_wide[[paste("cigarette_counts_lag1", suffix, sep = "")]])

###############################################################################
# Step 0. Update completed dataset -- cigarette motivation
# reported at the prior decision point
###############################################################################
dat_wide[[paste("motivation_cig_lag1", suffix, sep = "")]] <- dat_wide[[paste("motivation_cig", suffix_lag1, sep = "")]]
dat_wide[[paste("motivation_cig_lag1", suffix, sep = "")]] <- if_else(dat_wide[[paste("eligibility_lag1", suffix, sep = "")]] == 0, -1, dat_wide[[paste("motivation_cig_lag1", suffix, sep = "")]])

###############################################################################
# Step 0. Update completed dataset -- cigarette self-efficacy
# reported at the prior decision point
###############################################################################
dat_wide[[paste("self_efficacy_cig_lag1", suffix, sep = "")]] <- dat_wide[[paste("self_efficacy_cig", suffix_lag1, sep = "")]]
dat_wide[[paste("self_efficacy_cig_lag1", suffix, sep = "")]] <- if_else(dat_wide[[paste("eligibility_lag1", suffix, sep = "")]] == 0, -1, dat_wide[[paste("self_efficacy_cig_lag1", suffix, sep = "")]])

################################################################################
# How many replicates do we have?
################################################################################
all_replicate_ids <- unique(dat_wide[["replicate_id"]])
maximum_replicate_id <- max(all_replicate_ids)
minimum_replicate_id <- min(all_replicate_ids)

###############################################################################
# Step 0. Update completed dataset -- sum of cigarette counts in past 24 hours
###############################################################################
this_variable <- "cigarette_counts"
this_indicator <- "eligibility"

if(maximum_replicate_id > 0){
  list_dat_all <- list()
  for(idx in minimum_replicate_id:maximum_replicate_id){
    dat_current <- dat_wide %>% filter(replicate_id == idx)
    
    # This loop checks all decision points prior to the current decision point
    for(k in 1:(current_dp_value - 1)){
      variable_name_original <- paste(this_variable, "_dp", k, sep = "")
      variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
      
      dat_current[[variable_name_transformed]] <- dat_current[[variable_name_original]]
      
      # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
      dat_current[[variable_name_transformed]] <- replace(dat_current[[variable_name_transformed]], dat_current[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
    }
    
    variable_name_past24hrs <- paste(this_variable, "_sum_past24hrs", suffix, sep = "")
    variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
    variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
    variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
    
    dat_current[[variable_name_past24hrs]] <- -1
    
    for(this_participant in 1:nrow(dat_current)){
      if(dat_current[this_participant, variable_name_indicator_now] == 1){
        if(dat_current[this_participant, variable_name_indicator_past24hrs] == 1){
          # What are all the eligible decision points in the past 24 hours prior to the current decision point
          matched_dp <- dat_current[this_participant, variable_name_matched_decision_point]
          all_indices_past24hrs <- matched_dp:(current_dp_value-1)
          all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
          
          # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
          all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
          
          # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
          all_summands <- dat_current[this_participant, all_names_history_eligibility] * dat_current[this_participant, all_names_history_this_variable]
          resulting_value <- sum(all_summands)
          dat_current[this_participant, variable_name_past24hrs] <- resulting_value
        }
      }
    }
    
    list_dat_all <- append(list_dat_all, list(dat_current))
  }
  dat_wide <- bind_rows(list_dat_all)
}else{
  # This loop checks all decision points prior to the current decision point
  for(k in 1:(current_dp_value - 1)){
    variable_name_original <- paste(this_variable, "_dp", k, sep = "")
    variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
    
    dat_wide[[variable_name_transformed]] <- dat_wide[[variable_name_original]]
    
    # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
    dat_wide[[variable_name_transformed]] <- replace(dat_wide[[variable_name_transformed]], dat_wide[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
  }
  
  variable_name_past24hrs <- paste(this_variable, "_sum_past24hrs", suffix, sep = "")
  variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
  variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
  variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
  
  dat_wide[[variable_name_past24hrs]] <- -1
  
  for(this_participant in 1:nrow(dat_wide)){
    if(dat_wide[this_participant, variable_name_indicator_now] == 1){
      if(dat_wide[this_participant, variable_name_indicator_past24hrs] == 1){
        # What are all the eligible decision points in the past 24 hours prior to the current decision point
        matched_dp <- dat_wide[this_participant, variable_name_matched_decision_point]
        all_indices_past24hrs <- matched_dp:(current_dp_value-1)
        all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
        
        # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
        all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
        
        # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
        all_summands <- dat_wide[this_participant, all_names_history_eligibility] * dat_wide[this_participant, all_names_history_this_variable]
        resulting_value <- sum(all_summands)
        dat_wide[this_participant, variable_name_past24hrs] <- resulting_value
      }
    }
  }
}

###############################################################################
# Step 0. Update completed dataset -- mean of motivation_cig
# in past 24 hours
###############################################################################
this_variable <- "motivation_cig"
this_indicator <- "eligibility"

if(maximum_replicate_id > 0){
  list_dat_all <- list()
  for(idx in minimum_replicate_id:maximum_replicate_id){
    dat_current <- dat_wide %>% filter(replicate_id == idx)
    
    # This loop checks all decision points prior to the current decision point
    for(k in 1:(current_dp_value - 1)){
      variable_name_original <- paste(this_variable, "_dp", k, sep = "")
      variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
      
      dat_current[[variable_name_transformed]] <- dat_current[[variable_name_original]]
      
      # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
      dat_current[[variable_name_transformed]] <- replace(dat_current[[variable_name_transformed]], dat_current[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
    }
    
    variable_name_past24hrs <- paste(this_variable, "_mean_past24hrs", suffix, sep = "")
    variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
    variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
    variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
    
    dat_current[[variable_name_past24hrs]] <- -1
    
    for(this_participant in 1:nrow(dat_current)){
      if(dat_current[this_participant, variable_name_indicator_now] == 1){
        if(dat_current[this_participant, variable_name_indicator_past24hrs] == 1){
          # What are all the eligible decision points in the past 24 hours prior to the current decision point
          matched_dp <- dat_current[this_participant, variable_name_matched_decision_point]
          all_indices_past24hrs <- matched_dp:(current_dp_value-1)
          all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
          
          # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
          all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
          
          # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
          all_summands <- dat_current[this_participant, all_names_history_eligibility] * dat_current[this_participant, all_names_history_this_variable]
          resulting_value <- sum(all_summands)
          dat_current[this_participant, variable_name_past24hrs] <- resulting_value/length(all_summands)
        }
      }
    }
    
    list_dat_all <- append(list_dat_all, list(dat_current))
  }
  dat_wide <- bind_rows(list_dat_all)
}else{
  # This loop checks all decision points prior to the current decision point
  for(k in 1:(current_dp_value - 1)){
    variable_name_original <- paste(this_variable, "_dp", k, sep = "")
    variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
    
    dat_wide[[variable_name_transformed]] <- dat_wide[[variable_name_original]]
    
    # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
    dat_wide[[variable_name_transformed]] <- replace(dat_wide[[variable_name_transformed]], dat_wide[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
  }
  
  variable_name_past24hrs <- paste(this_variable, "_mean_past24hrs", suffix, sep = "")
  variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
  variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
  variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
  
  dat_wide[[variable_name_past24hrs]] <- -1
  
  for(this_participant in 1:nrow(dat_wide)){
    if(dat_wide[this_participant, variable_name_indicator_now] == 1){
      if(dat_wide[this_participant, variable_name_indicator_past24hrs] == 1){
        # What are all the eligible decision points in the past 24 hours prior to the current decision point
        matched_dp <- dat_wide[this_participant, variable_name_matched_decision_point]
        all_indices_past24hrs <- matched_dp:(current_dp_value-1)
        all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
        
        # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
        all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
        
        # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
        all_summands <- dat_wide[this_participant, all_names_history_eligibility] * dat_wide[this_participant, all_names_history_this_variable]
        resulting_value <- sum(all_summands)
        dat_wide[this_participant, variable_name_past24hrs] <- resulting_value/length(all_summands)
      }
    }
  }
}

###############################################################################
# Step 0. Update completed dataset -- mean of self_efficacy_cig
# in past 24 hours
###############################################################################
this_variable <- "self_efficacy_cig"
this_indicator <- "eligibility"

if(maximum_replicate_id > 0){
  list_dat_all <- list()
  for(idx in minimum_replicate_id:maximum_replicate_id){
    dat_current <- dat_wide %>% filter(replicate_id == idx)
    
    # This loop checks all decision points prior to the current decision point
    for(k in 1:(current_dp_value - 1)){
      variable_name_original <- paste(this_variable, "_dp", k, sep = "")
      variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
      
      dat_current[[variable_name_transformed]] <- dat_current[[variable_name_original]]
      
      # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
      dat_current[[variable_name_transformed]] <- replace(dat_current[[variable_name_transformed]], dat_current[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
    }
    
    variable_name_past24hrs <- paste(this_variable, "_mean_past24hrs", suffix, sep = "")
    variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
    variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
    variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
    
    dat_current[[variable_name_past24hrs]] <- -1
    
    for(this_participant in 1:nrow(dat_current)){
      if(dat_current[this_participant, variable_name_indicator_now] == 1){
        if(dat_current[this_participant, variable_name_indicator_past24hrs] == 1){
          # What are all the eligible decision points in the past 24 hours prior to the current decision point
          matched_dp <- dat_current[this_participant, variable_name_matched_decision_point]
          all_indices_past24hrs <- matched_dp:(current_dp_value-1)
          all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
          
          # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
          all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
          
          # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
          all_summands <- dat_current[this_participant, all_names_history_eligibility] * dat_current[this_participant, all_names_history_this_variable]
          resulting_value <- sum(all_summands)
          dat_current[this_participant, variable_name_past24hrs] <- resulting_value/length(all_summands)
        }
      }
    }
    
    list_dat_all <- append(list_dat_all, list(dat_current))
  }
  dat_wide <- bind_rows(list_dat_all)
}else{
  # This loop checks all decision points prior to the current decision point
  for(k in 1:(current_dp_value - 1)){
    variable_name_original <- paste(this_variable, "_dp", k, sep = "")
    variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
    
    dat_wide[[variable_name_transformed]] <- dat_wide[[variable_name_original]]
    
    # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
    dat_wide[[variable_name_transformed]] <- replace(dat_wide[[variable_name_transformed]], dat_wide[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
  }
  
  variable_name_past24hrs <- paste(this_variable, "_mean_past24hrs", suffix, sep = "")
  variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
  variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
  variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
  
  dat_wide[[variable_name_past24hrs]] <- -1
  
  for(this_participant in 1:nrow(dat_wide)){
    if(dat_wide[this_participant, variable_name_indicator_now] == 1){
      if(dat_wide[this_participant, variable_name_indicator_past24hrs] == 1){
        # What are all the eligible decision points in the past 24 hours prior to the current decision point
        matched_dp <- dat_wide[this_participant, variable_name_matched_decision_point]
        all_indices_past24hrs <- matched_dp:(current_dp_value-1)
        all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
        
        # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
        all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
        
        # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
        all_summands <- dat_wide[this_participant, all_names_history_eligibility] * dat_wide[this_participant, all_names_history_this_variable]
        resulting_value <- sum(all_summands)
        dat_wide[this_participant, variable_name_past24hrs] <- resulting_value/length(all_summands)
      }
    }
  }
}

###############################################################################
#                                                                             #
#    Specify variables we would consider as predictors in imputation models   #
#                                                                             #
###############################################################################
my_list <- list("cigarette_counts" = NULL,
                "motivation_cig" = NULL,
                "self_efficacy_cig" = NULL)

this_outcome <- "cigarette_counts"
my_list[[this_outcome]] <- c(paste(this_outcome, suffix, sep = ""),
                             "baseline_tobacco_history", "Nicotine_dep",
                             paste(this_outcome, "_sum_past24hrs", suffix, sep = ""),
                             paste(this_outcome, "_lag1", suffix, sep = ""),
                             paste("completed_app_usage_preblock", "_lag1", suffix, sep = ""),
                             paste("motivation_cig", "_mean_past24hrs", suffix, sep = ""),
                             paste("self_efficacy", "_mean_past24hrs", suffix, sep = ""))

this_outcome <- "motivation_cig"
my_list[[this_outcome]] <- c(paste(this_outcome, suffix, sep = ""),
                             "TSAM_Total",
                             paste(this_outcome, "_mean_past24hrs", suffix, sep = ""),
                             paste(this_outcome, "_lag1", suffix, sep = ""),
                             paste("cigarette_counts", suffix, sep = ""),
                             paste("completed_app_usage_preblock", "_lag1", suffix, sep = ""),
                             paste("cigarette_counts", "_sum_past24hrs", suffix, sep = ""),
                             paste("self_efficacy", "_mean_past24hrs", suffix, sep = ""))

this_outcome <- "self_efficacy_cig"
my_list[[this_outcome]] <- c(paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""),
                             "SE_total",
                             paste(this_outcome, "_mean_past24hrs", suffix, sep = ""),
                             paste(this_outcome, "_lag1", suffix, sep = ""),
                             paste("cigarette_counts", suffix, sep = ""),
                             paste("motivation_cig", suffix, sep = ""),
                             paste("completed_app_usage_preblock", "_lag1", suffix, sep = ""),
                             paste("motivation_cig", "_mean_past24hrs", suffix, sep = ""),
                             paste("cigarette_counts", "_sum_past24hrs", suffix, sep = ""))

###############################################################################
#                                                                             #
#              Create a list in which to save any mice logged events          #
#                                                                             #
###############################################################################
list_mice_logged_events <- list("cigarette_counts" = NULL,
                                "motivation_cig" = NULL,
                                "self_efficacy_cig" = NULL)

list_mice_model <- list("cigarette_counts" = NULL,
                        "motivation_cig" = NULL,
                        "self_efficacy_cig" = NULL)

list_logged_convergence_initial_model <- list("cigarette_counts" = NULL,
                                              "motivation_cig" = NULL,
                                              "self_efficacy_cig" = NULL)

list_logged_convergence_stepwise_model <- list("cigarette_counts" = NULL,
                                               "motivation_cig" = NULL,
                                               "self_efficacy_cig" = NULL)

###############################################################################
#                                                                             #
#                 Impute missing proximal outcome in stratum 3                #
#                                                                             #
###############################################################################

###############################################################################
# Step 1. Impute cigarette_counts
###############################################################################
this_outcome <- "cigarette_counts"
LHS <- paste(this_outcome, suffix, sep = "")

list_collect_data <- list()

# Rows where imputation at the current decision point will NOT be performed ---
restriction_violate_string <- paste("eligibility", suffix, " == 0", sep = "")
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))
list_collect_data <- append(list_collect_data, list(rows_violate_restriction))

# Rows where imputation at the current decision point will be performed -------
cond1 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 0", sep = ""), ")", sep = "")
cond2 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
cond3 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 1", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
list_restriction_meet_string <- list("less_stringent" = list(cond1, cond2),
                                     "most_stringent" = cond3)

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 1
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_01_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_01_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 2
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_02_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_02_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for most stringent restriction -----------------------------------
restriction_meet_string <- list_restriction_meet_string[["most_stringent"]]
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

n_participants_remain <- rows_meet_restriction %>% filter(replicate_id == 0) %>% nrow(.)  # Need to just grab all the rows having replicate_id == 0 to count the total number of unique individuals
if(which_penalty == "BIC"){
  use_penalty <- log(n_participants_remain)
}else if(which_penalty == "AIC"){
  use_penalty <- 2
}else{
  print("Choose valid option")
}

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(rows_meet_restriction))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(rows_meet_restriction)
meth_list <- dummy_list

vars <- colnames(rows_meet_restriction)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

if(check_convergence_result == TRUE){
  if(length(fit$coefficients) != length(summary(fit)[["coefficients"]][,"Estimate"]) ){
    check_convergence_result <- FALSE
  }else if(length(fit$coefficients) != length(summary(fit)[["coefficients"]][,"Std. Error"]) ){
    check_convergence_result <- FALSE
  }else{
    check_convergence_result <- TRUE
  }
}

if(check_convergence_result == TRUE){
  stepwise_convergence <- TRUE
  
  fit_step <- tryCatch(expr = {
    stepAIC(fit, 
            direction = "both",
            scope = list(lower = as.formula("~1"),
                         upper = fit$formula),
            trace = FALSE, 
            k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)
  }, warning = function(w){"Hey, a warning"})
  
  stepwise_convergence <- (class(fit_step)[[1]] == "glm")
  
  if(stepwise_convergence == TRUE){
    use_fit <- fit_step
  }else{
    use_fit <- fit
  }
  
  info_criterion <- extractAIC(use_fit, k = use_penalty)[[2]]  # Calculated info criterion of selected model
  
  selected_vars <- names(use_fit$coefficients)
  selected_vars <- selected_vars[-1] # remove the intercept term because the predictorMatrix argument does not allow a row/column for that
  
  if(length(selected_vars) == 0){
    meth_list[[LHS]] <- "pmm"
    
    dummy_list <- as.list(colnames(rows_meet_restriction))
    dummy_list <- lapply(dummy_list, function(x){return("")})
    names(dummy_list) <- colnames(rows_meet_restriction)
    formula_list <- dummy_list
    
    for(i in 1:length(formula_list)){
      fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
      formula_list[[i]] <- fmla
    }
    
    imp <- mice(data = rows_meet_restriction, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                formulas = formula_list)
    rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
  }else{
    for(i in 1:length(selected_vars)){
      pred_mat[LHS, selected_vars[i]] <- 1
    }
    
    meth_list[[LHS]] <- "pmm"
    imp <- mice(data = rows_meet_restriction, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                predictorMatrix = pred_mat)
    rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
  }
}else{
  stepwise_convergence <- TRUE # This is just a placeholder value for this condition
  meth_list[[LHS]] <- "pmm"
  
  dummy_list <- as.list(colnames(rows_meet_restriction))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(rows_meet_restriction)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  imp <- mice(data = rows_meet_restriction, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
}

if(class(rows_meet_restriction_completed[[LHS]]) == "factor"){
  rows_meet_restriction_completed[[LHS]] <- as.numeric(rows_meet_restriction_completed[[LHS]]) - 1
}

# Before we move on to the next variable...
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
dat_wide <- bind_rows(list_collect_data)

list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[LHS]])

list_logged_convergence_initial_model[[which(names(list_logged_convergence_initial_model) == this_outcome)]] <- check_convergence_result
list_logged_convergence_stepwise_model[[which(names(list_logged_convergence_stepwise_model) == this_outcome)]] <- stepwise_convergence

###############################################################################
# Step 2. Impute motivation_cig
###############################################################################
this_outcome <- "motivation_cig"
LHS <- paste(this_outcome, suffix, sep = "")

list_collect_data <- list()

# Rows where imputation at the current decision point will NOT be performed ---
restriction_violate_string <- paste("eligibility", suffix, " == 0", sep = "")
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))
list_collect_data <- append(list_collect_data, list(rows_violate_restriction))

# Rows where imputation at the current decision point will be performed -------
cond1 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 0", sep = ""), ")", sep = "")
cond2 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
cond3 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 1", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
list_restriction_meet_string <- list("less_stringent" = list(cond1, cond2),
                                     "most_stringent" = cond3)

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 1
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_01_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_01_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 2
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_02_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_02_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for most stringent restriction -----------------------------------
restriction_meet_string <- list_restriction_meet_string[["most_stringent"]]
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

n_participants_remain <- rows_meet_restriction %>% filter(replicate_id == 0) %>% nrow(.)  # Need to just grab all the rows having replicate_id == 0 to count the total number of unique individuals
if(which_penalty == "BIC"){
  use_penalty <- log(n_participants_remain)
}else if(which_penalty == "AIC"){
  use_penalty <- 2
}else{
  print("Choose valid option")
}

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(rows_meet_restriction))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(rows_meet_restriction)
meth_list <- dummy_list

vars <- colnames(rows_meet_restriction)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% 
  select(all_of(consider_these_vars))

fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), 
                            family = gaussian, 
                            data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

if(check_convergence_result == TRUE){
  if(length(fit$coefficients) != length(summary(fit)[["coefficients"]][,"Estimate"]) ){
    check_convergence_result <- FALSE
  }else if(length(fit$coefficients) != length(summary(fit)[["coefficients"]][,"Std. Error"]) ){
    check_convergence_result <- FALSE
  }else{
    check_convergence_result <- TRUE
  }
}

if(check_convergence_result == TRUE){
  stepwise_convergence <- TRUE
  
  fit_step <- tryCatch(expr = {
    stepAIC(fit, 
            direction = "both",
            scope = list(lower = as.formula("~1"),
                         upper = fit$formula),
            trace = FALSE, 
            k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)
  }, warning = function(w){"Hey, a warning"})
  
  stepwise_convergence <- (class(fit_step)[[1]] == "glm")
  
  if(stepwise_convergence == TRUE){
    use_fit <- fit_step
  }else{
    use_fit <- fit
  }
  
  info_criterion <- extractAIC(use_fit, k = use_penalty)[[2]]  # Calculated info criterion of selected model
  
  selected_vars <- names(use_fit$coefficients)
  selected_vars <- selected_vars[-1] # remove the intercept term because the predictorMatrix argument does not allow a row/column for that
  
  if(length(selected_vars) == 0){
    meth_list[[LHS]] <- "pmm"
    
    dummy_list <- as.list(colnames(rows_meet_restriction))
    dummy_list <- lapply(dummy_list, function(x){return("")})
    names(dummy_list) <- colnames(rows_meet_restriction)
    formula_list <- dummy_list
    
    for(i in 1:length(formula_list)){
      fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
      formula_list[[i]] <- fmla
    }
    
    imp <- mice(data = rows_meet_restriction, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                formulas = formula_list)
    rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
  }else{
    for(i in 1:length(selected_vars)){
      pred_mat[LHS, selected_vars[i]] <- 1
    }
    
    meth_list[[LHS]] <- "pmm"
    imp <- mice(data = rows_meet_restriction, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                predictorMatrix = pred_mat)
    rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
  }
}else{
  stepwise_convergence <- TRUE # This is just a placeholder value for this condition
  meth_list[[LHS]] <- "pmm"
  
  dummy_list <- as.list(colnames(rows_meet_restriction))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(rows_meet_restriction)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  imp <- mice(data = rows_meet_restriction, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
}

if(class(rows_meet_restriction_completed[[LHS]]) == "factor"){
  rows_meet_restriction_completed[[LHS]] <- as.numeric(rows_meet_restriction_completed[[LHS]]) - 1
}

# Before we move on to the next variable...
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
dat_wide <- bind_rows(list_collect_data)

list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[LHS]])

list_logged_convergence_initial_model[[which(names(list_logged_convergence_initial_model) == this_outcome)]] <- check_convergence_result
list_logged_convergence_stepwise_model[[which(names(list_logged_convergence_stepwise_model) == this_outcome)]] <- stepwise_convergence

###############################################################################
# Step 3. Impute self_efficacy_cig
###############################################################################
this_outcome <- "self_efficacy_cig"
LHS <- paste(this_outcome, suffix, sep = "")

list_collect_data <- list()

# Rows where imputation at the current decision point will NOT be performed ---
restriction_violate_string <- paste("eligibility", suffix, " == 0", sep = "")
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))
list_collect_data <- append(list_collect_data, list(rows_violate_restriction))

# Rows where imputation at the current decision point will be performed -------
cond1 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 0", sep = ""), ")", sep = "")
cond2 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
cond3 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 1", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
list_restriction_meet_string <- list("less_stringent" = list(cond1, cond2),
                                     "most_stringent" = cond3)

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 1
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_01_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_01_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 2
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_02_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_02_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for most stringent restriction -----------------------------------
restriction_meet_string <- list_restriction_meet_string[["most_stringent"]]
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

n_participants_remain <- rows_meet_restriction %>% filter(replicate_id == 0) %>% nrow(.)  # Need to just grab all the rows having replicate_id == 0 to count the total number of unique individuals
if(which_penalty == "BIC"){
  use_penalty <- log(n_participants_remain)
}else if(which_penalty == "AIC"){
  use_penalty <- 2
}else{
  print("Choose valid option")
}

# Add two-way interactions that we would like to include in the imputation model
rows_meet_restriction[[paste("high_effort_times_self_efficacy_lag1", suffix, sep = "")]] <- rows_meet_restriction[[paste("is_high_effort", suffix, sep = "")]] * rows_meet_restriction[[paste("self_efficacy_cig_lag1", suffix, sep = "")]] 
rows_meet_restriction[[paste("low_effort_times_self_efficacy_lag1", suffix, sep = "")]] <- rows_meet_restriction[[paste("is_low_effort", suffix, sep = "")]] * rows_meet_restriction[[paste("self_efficacy_cig_lag1", suffix, sep = "")]]

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(rows_meet_restriction))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(rows_meet_restriction)
meth_list <- dummy_list

vars <- colnames(rows_meet_restriction)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]

dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% 
  select(all_of(consider_these_vars), 
         paste("high_effort_times_self_efficacy_lag1", suffix, sep = ""), paste("low_effort_times_self_efficacy_lag1", suffix, sep = ""))

fit <- tryCatch(expr = {glm(as.formula(paste(LHS, 
                                             paste("~ ", "is_high_effort", suffix, "+", "is_low_effort", suffix, "+", 
                                                   "self_efficacy_cig_lag1", suffix, "+", 
                                                   paste("high_effort_times_self_efficacy_lag1", suffix, sep = ""), "+", paste("low_effort_times_self_efficacy_lag1", suffix, sep = ""), "+",
                                                   sep = ""),
                                             "+.",
                                             sep = "")), 
                            family = gaussian, 
                            data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})

check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

if(check_convergence_result == TRUE){
  if(length(fit$coefficients) != length(summary(fit)[["coefficients"]][,"Estimate"]) ){
    check_convergence_result <- FALSE
  }else if(length(fit$coefficients) != length(summary(fit)[["coefficients"]][,"Std. Error"]) ){
    check_convergence_result <- FALSE
  }else{
    check_convergence_result <- TRUE
  }
}

if(check_convergence_result == TRUE){
  stepwise_convergence <- TRUE
  
  fit_step <- tryCatch(expr = {
    stepAIC(fit, 
            direction = "both",
            scope = list(lower = as.formula(paste(LHS, 
                                                  paste("~ ", "is_high_effort", suffix, "+", "is_low_effort", suffix, "+", 
                                                        "self_efficacy_cig_lag1", suffix, "+", 
                                                        paste("high_effort_times_self_efficacy_lag1", suffix, sep = ""), "+", paste("low_effort_times_self_efficacy_lag1", suffix, sep = ""),
                                                        sep = ""), sep = "")),
                         upper = fit$formula),
            trace = FALSE, 
            k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)
  }, warning = function(w){"Hey, a warning"})
  
  stepwise_convergence <- (class(fit_step)[[1]] == "glm")
  
  if(stepwise_convergence == TRUE){
    use_fit <- fit_step
  }else{
    use_fit <- fit
  }
  
  info_criterion <- extractAIC(use_fit, k = use_penalty)[[2]]  # Calculated info criterion of selected model
  
  selected_vars <- names(use_fit$coefficients)
  selected_vars <- selected_vars[-1] # remove the intercept term because the predictorMatrix argument does not allow a row/column for that
  
  if(length(selected_vars) == 0){
    meth_list[[LHS]] <- "pmm"
    
    dummy_list <- as.list(colnames(rows_meet_restriction))
    dummy_list <- lapply(dummy_list, function(x){return("")})
    names(dummy_list) <- colnames(rows_meet_restriction)
    formula_list <- dummy_list
    
    for(i in 1:length(formula_list)){
      fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
      formula_list[[i]] <- fmla
    }
    
    imp <- mice(data = rows_meet_restriction, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                formulas = formula_list)
    rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
  }else{
    for(i in 1:length(selected_vars)){
      pred_mat[LHS, selected_vars[i]] <- 1
    }
    
    meth_list[[LHS]] <- "pmm"
    imp <- mice(data = rows_meet_restriction, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                predictorMatrix = pred_mat)
    rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
  }
}else{
  stepwise_convergence <- TRUE # This is just a placeholder value for this condition
  meth_list[[LHS]] <- "pmm"
  
  dummy_list <- as.list(colnames(rows_meet_restriction))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(rows_meet_restriction)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  imp <- mice(data = rows_meet_restriction, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
}

if(class(rows_meet_restriction_completed[[LHS]]) == "factor"){
  rows_meet_restriction_completed[[LHS]] <- as.numeric(rows_meet_restriction_completed[[LHS]]) - 1
}

rows_meet_restriction_completed[[paste("high_effort_times_self_efficacy_lag1", suffix, sep = "")]] <- NULL
rows_meet_restriction_completed[[paste("low_effort_times_self_efficacy_lag1", suffix, sep = "")]] <- NULL

# Before we move on to the next variable...
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
dat_wide <- bind_rows(list_collect_data)

list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[LHS]])

list_logged_convergence_initial_model[[which(names(list_logged_convergence_initial_model) == this_outcome)]] <- check_convergence_result
list_logged_convergence_stepwise_model[[which(names(list_logged_convergence_stepwise_model) == this_outcome)]] <- stepwise_convergence

###############################################################################
# Save
###############################################################################
saveRDS(list_mice_logged_events, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("logged_events_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))
saveRDS(list_mice_model, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imputation_model_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))

saveRDS(list_logged_convergence_initial_model, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("logged_convergence_initial_model_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))
saveRDS(list_logged_convergence_stepwise_model, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("logged_convergence_stepwise_model_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))

saveRDS(dat_wide, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_wide_completed", "_dp", current_dp_value, ".rds", sep = "")))

