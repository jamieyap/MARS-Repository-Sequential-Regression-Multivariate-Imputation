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
st_num <- 1 # The stratum being imputed
which_penalty <- "BIC"  # Can be set to either "AIC" or "BIC"

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

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets"))
}

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num))
}

################################################################################
# Load datasets
################################################################################

# Read in completed dataset from previous time-point
dat_wide_completed_baseline <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))

# Lay out all of the options
cond1 <- "(eligibility == 1 & eligibility_lag1 == 0 & any_recent_eligible_dp == 0)"  # -- stratum 1
cond2 <- "(eligibility == 1 & eligibility_lag1 == 0 & any_recent_eligible_dp == 1)"  # -- stratum 2
cond3 <- "(eligibility == 1 & eligibility_lag1 == 1)"                                # -- stratum 3

use_cond <- case_when(
  st_num == 1 ~ cond1,
  st_num == 2 ~ cond2,
  st_num == 3 ~ cond3,
  .default = NA_character_
)

###############################################################################
#                                                                             #
#                 Impute missing proximal outcome in stratum 1                #
#                                                                             #
###############################################################################

cols_to_keep_baseline <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_baseline.rds"))
cols_to_keep_timevarying <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_timevarying.rds"))

# Recall that dat_primary_aim_replicated.rds is an 
# output of the script create-replicated-dataset.R
dat_long <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_for_new_pipeline_replicated.rds"))
dat_long <- dat_long %>% select(all_of(c("replicate_id", "participant_id", "decision_point", cols_to_keep_timevarying)))
dat_long_merged <- left_join(x = dat_wide_completed_baseline, y = dat_long, by = join_by(replicate_id == replicate_id, participant_id == participant_id))

###############################################################################
#                                                                             #
#                          Specify relevant restriction                       #
#                                                                             #
###############################################################################
dat_stratum <- dat_long_merged %>% filter(!!rlang::parse_expr(use_cond))
dat_stratum[["Y"]] <- as_factor(dat_stratum[["Y"]])

###############################################################################
#                                                                             #
#                       Specify model selection criteria                      #
#                                                                             #
###############################################################################
n_participants_remain <- dat_stratum %>% filter(replicate_id == 0) %>% nrow(.)

if(which_penalty == "BIC"){
  use_penalty <- log(n_participants_remain)
}else if(which_penalty == "AIC"){
  use_penalty <- 2
}else{
  print("Choose valid option")
}

###############################################################################
#                                                                             #
#    Specify variables we would consider as predictors in imputation models   #
#                                                                             #
###############################################################################
my_list <- list("expectancy_cig" = NULL,
                "self_efficacy_cig" = NULL,
                "cigarette_counts" = NULL,
                "Y" = NULL,
                "motivation_cig" = NULL)

this_outcome <- "expectancy_cig"
my_list[[this_outcome]] <- c(this_outcome,
                             "baseline_tobacco_history",
                             "income_val",
                             "FinancialStrain",
                             "nd_mean",
                             "food_security_mean",
                             "SSSladders",
                             "pp1_1")


this_outcome <- "self_efficacy_cig"
my_list[[this_outcome]] <- c(this_outcome,
                             "baseline_tobacco_history",
                             "SE_total",
                             "srq_mean",
                             "hour_coinflip_local")

this_outcome <- "cigarette_counts"
my_list[[this_outcome]] <- c(this_outcome,
                             "baseline_tobacco_history",
                             "hour_coinflip_local")

this_outcome <- "Y"
my_list[[this_outcome]] <- c(this_outcome,
                             "baseline_tobacco_history",
                             "SE_total",
                             "srq_mean",
                             "is_high_effort", "is_low_effort",
                             "hour_coinflip_local")

this_outcome <- "motivation_cig"
my_list[[this_outcome]] <- c(this_outcome,
                             "baseline_tobacco_history",
                             "SE_total",
                             "srq_mean",
                             "is_high_effort", "is_low_effort",
                             "hour_coinflip_local")

###############################################################################
#                                                                             #
#              Create a list in which to save any mice logged events          #
#                                                                             #
###############################################################################
list_mice_logged_events <- list("expectancy_cig" = NULL,
                                "self_efficacy_cig" = NULL,
                                "cigarette_counts" = NULL,
                                "Y" = NULL,
                                "motivation_cig" = NULL)

list_mice_model <- list("expectancy_cig" = NULL,
                        "self_efficacy_cig" = NULL,
                        "cigarette_counts" = NULL,
                        "Y" = NULL,
                        "motivation_cig" = NULL)

list_logged_convergence_initial_model <- list("expectancy_cig" = NULL,
                                              "self_efficacy_cig" = NULL,
                                              "cigarette_counts" = NULL,
                                              "Y" = NULL,
                                              "motivation_cig" = NULL)

list_logged_convergence_stepwise_model <- list("expectancy_cig" = NULL,
                                               "self_efficacy_cig" = NULL,
                                               "cigarette_counts" = NULL,
                                               "Y" = NULL,
                                               "motivation_cig" = NULL)

###############################################################################
# Step 1. Impute expectancy_cig
###############################################################################
this_outcome <- "expectancy_cig"

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(dat_stratum))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(dat_stratum)
meth_list <- dummy_list

vars <- colnames(dat_stratum)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

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
    meth_list[[this_outcome]] <- "pmm"
    
    dummy_list <- as.list(colnames(dat_stratum))
    dummy_list <- lapply(dummy_list, function(x){return("")})
    names(dummy_list) <- colnames(dat_stratum)
    formula_list <- dummy_list
    
    for(i in 1:length(formula_list)){
      fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
      formula_list[[i]] <- fmla
    }
    
    imp <- mice(data = dat_stratum, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                formulas = formula_list)
    dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
  }else{
    for(i in 1:length(selected_vars)){
      pred_mat[this_outcome, selected_vars[i]] <- 1
    }
    
    meth_list[[this_outcome]] <- "pmm"
    imp <- mice(data = dat_stratum, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                predictorMatrix = pred_mat)
    dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
  }
}else{
  stepwise_convergence <- TRUE # This is just a placeholder value for this condition
  meth_list[[this_outcome]] <- "pmm"
  
  dummy_list <- as.list(colnames(dat_stratum))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(dat_stratum)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  imp <- mice(data = dat_stratum, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
}

# Before we move on to the next variable...
dat_stratum <- dat_stratum_completed
list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[this_outcome]])

list_logged_convergence_initial_model[[which(names(list_logged_convergence_initial_model) == this_outcome)]] <- check_convergence_result
list_logged_convergence_stepwise_model[[which(names(list_logged_convergence_stepwise_model) == this_outcome)]] <- stepwise_convergence

###############################################################################
# Step 2. Impute self_efficacy_cig
###############################################################################
this_outcome <- "self_efficacy_cig"

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(dat_stratum))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(dat_stratum)
meth_list <- dummy_list

vars <- colnames(dat_stratum)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

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
    meth_list[[this_outcome]] <- "pmm"
    
    dummy_list <- as.list(colnames(dat_stratum))
    dummy_list <- lapply(dummy_list, function(x){return("")})
    names(dummy_list) <- colnames(dat_stratum)
    formula_list <- dummy_list
    
    for(i in 1:length(formula_list)){
      fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
      formula_list[[i]] <- fmla
    }
    
    imp <- mice(data = dat_stratum, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                formulas = formula_list)
    dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
  }else{
    for(i in 1:length(selected_vars)){
      pred_mat[this_outcome, selected_vars[i]] <- 1
    }
    
    meth_list[[this_outcome]] <- "pmm"
    imp <- mice(data = dat_stratum, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                predictorMatrix = pred_mat)
    dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
  }
}else{
  stepwise_convergence <- TRUE # This is just a placeholder value for this condition
  meth_list[[this_outcome]] <- "pmm"
  
  dummy_list <- as.list(colnames(dat_stratum))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(dat_stratum)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  imp <- mice(data = dat_stratum, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
}

# Before we move on to the next variable...
dat_stratum <- dat_stratum_completed
list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[this_outcome]])

list_logged_convergence_initial_model[[which(names(list_logged_convergence_initial_model) == this_outcome)]] <- check_convergence_result
list_logged_convergence_stepwise_model[[which(names(list_logged_convergence_stepwise_model) == this_outcome)]] <- stepwise_convergence

###############################################################################
# Step 3. Impute cigarette_counts
###############################################################################
this_outcome <- "cigarette_counts"

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(dat_stratum))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(dat_stratum)
meth_list <- dummy_list

vars <- colnames(dat_stratum)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

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
    meth_list[[this_outcome]] <- "pmm"
    
    dummy_list <- as.list(colnames(dat_stratum))
    dummy_list <- lapply(dummy_list, function(x){return("")})
    names(dummy_list) <- colnames(dat_stratum)
    formula_list <- dummy_list
    
    for(i in 1:length(formula_list)){
      fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
      formula_list[[i]] <- fmla
    }
    
    imp <- mice(data = dat_stratum, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                formulas = formula_list)
    dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
  }else{
    for(i in 1:length(selected_vars)){
      pred_mat[this_outcome, selected_vars[i]] <- 1
    }
    
    meth_list[[this_outcome]] <- "pmm"
    imp <- mice(data = dat_stratum, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                predictorMatrix = pred_mat)
    dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
  }
}else{
  stepwise_convergence <- TRUE # This is just a placeholder value for this condition
  meth_list[[this_outcome]] <- "pmm"
  
  dummy_list <- as.list(colnames(dat_stratum))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(dat_stratum)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  imp <- mice(data = dat_stratum, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
}

# Before we move on to the next variable...
dat_stratum <- dat_stratum_completed
list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[this_outcome]])

list_logged_convergence_initial_model[[which(names(list_logged_convergence_initial_model) == this_outcome)]] <- check_convergence_result
list_logged_convergence_stepwise_model[[which(names(list_logged_convergence_stepwise_model) == this_outcome)]] <- stepwise_convergence

###############################################################################
# Step 4. Impute Y
###############################################################################
this_outcome <- "Y"

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(dat_stratum))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(dat_stratum)
meth_list <- dummy_list

vars <- colnames(dat_stratum)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = binomial, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

if(check_convergence_result == TRUE){
  stepwise_convergence <- TRUE
  
  fit_step <- tryCatch(expr = {
    stepAIC(fit, 
            direction = "both",
            scope = list(lower = as.formula("~is_high_effort + is_low_effort"),
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
    stepwise_convergence <- TRUE # This is just a placeholder value for this condition
    dat_stratum[[LHS]] <- as.numeric(dat_stratum[[LHS]]) - 1
    n_missing <- sum(ici(dat_stratum[[LHS]]))
    fit_step <- glm(as.formula(paste(LHS, "~ 1", sep = "")), family = binomial, data = dat_stratum)
    tmp_dat <- predict.glm(fit_step, type = "response")
    imputed_vals <- rbinom(n = n_missing, size = 1, prob = tmp_dat[[1]])
    dat_stratum[[LHS]][ici(dat_stratum[[LHS]])] <- imputed_vals
    dat_stratum_completed <- dat_stratum  # Update dat_stratum
  }else{
    for(i in 1:length(selected_vars)){
      pred_mat[this_outcome, selected_vars[i]] <- 1
    }
    
    meth_list[[this_outcome]] <- "logreg"
    imp <- mice(data = dat_stratum, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                predictorMatrix = pred_mat)
    dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
  }
}else{
  stepwise_convergence <- TRUE # This is just a placeholder value for this condition
  dat_stratum[[LHS]] <- as.numeric(dat_stratum[[LHS]]) - 1
  n_missing <- sum(ici(dat_stratum[[LHS]]))
  fit_step <- glm(as.formula(paste(LHS, "~ 1", sep = "")), family = binomial, data = dat_stratum)
  tmp_dat <- predict.glm(fit_step, type = "response")
  imputed_vals <- rbinom(n = n_missing, size = 1, prob = tmp_dat[[1]])
  dat_stratum[[LHS]][ici(dat_stratum[[LHS]])] <- imputed_vals
  dat_stratum_completed <- dat_stratum  # Update dat_stratum
}

# Before we move on to the next variable...
dat_stratum_completed[[this_outcome]] <- as.numeric(dat_stratum_completed[[this_outcome]]) - 1

dat_stratum <- dat_stratum_completed
list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[this_outcome]])

list_logged_convergence_initial_model[[which(names(list_logged_convergence_initial_model) == this_outcome)]] <- check_convergence_result
list_logged_convergence_stepwise_model[[which(names(list_logged_convergence_stepwise_model) == this_outcome)]] <- stepwise_convergence

###############################################################################
# Step 5. Impute motivation_cig
###############################################################################
this_outcome <- "motivation_cig"

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(dat_stratum))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(dat_stratum)
meth_list <- dummy_list

vars <- colnames(dat_stratum)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

if(check_convergence_result == TRUE){
  stepwise_convergence <- TRUE
  
  fit_step <- tryCatch(expr = {
    stepAIC(fit, 
            direction = "both",
            scope = list(lower = as.formula("~is_high_effort + is_low_effort"),
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
    meth_list[[this_outcome]] <- "pmm"
    
    dummy_list <- as.list(colnames(dat_stratum))
    dummy_list <- lapply(dummy_list, function(x){return("")})
    names(dummy_list) <- colnames(dat_stratum)
    formula_list <- dummy_list
    
    for(i in 1:length(formula_list)){
      fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
      formula_list[[i]] <- fmla
    }
    
    imp <- mice(data = dat_stratum, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                formulas = formula_list)
    dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
  }else{
    for(i in 1:length(selected_vars)){
      pred_mat[this_outcome, selected_vars[i]] <- 1
    }
    
    meth_list[[this_outcome]] <- "pmm"
    imp <- mice(data = dat_stratum, 
                m = 1, 
                maxit = use_maxit_value,
                meth =  meth_list,
                predictorMatrix = pred_mat)
    dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
  }
}else{
  stepwise_convergence <- TRUE # This is just a placeholder value for this condition
  meth_list[[this_outcome]] <- "pmm"
  
  dummy_list <- as.list(colnames(dat_stratum))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(dat_stratum)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  imp <- mice(data = dat_stratum, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
}

# Before we exit...
dat_stratum <- dat_stratum_completed
list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[this_outcome]])

list_logged_convergence_initial_model[[which(names(list_logged_convergence_initial_model) == this_outcome)]] <- check_convergence_result
list_logged_convergence_stepwise_model[[which(names(list_logged_convergence_stepwise_model) == this_outcome)]] <- stepwise_convergence

###############################################################################
# Save
###############################################################################
saveRDS(list_mice_logged_events, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("logged_events_stratum", st_num, ".rds", sep = "")))
saveRDS(list_mice_model, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imputation_model_stratum", st_num, ".rds", sep = "")))

saveRDS(list_logged_convergence_initial_model, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("logged_convergence_initial_model_stratum", st_num, ".rds", sep = "")))
saveRDS(list_logged_convergence_stepwise_model, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("logged_convergence_stepwise_model_stratum", st_num, ".rds", sep = "")))

saveRDS(dat_stratum, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_imputed_stratum", st_num, ".rds", sep = "")))
