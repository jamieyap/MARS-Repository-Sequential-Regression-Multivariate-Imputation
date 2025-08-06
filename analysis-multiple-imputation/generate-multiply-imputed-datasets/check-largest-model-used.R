rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
source("paths.R")

M <- .__total_imputed_datasets
max_dp <- .__maximum_march_forward

st_num <- 3

###############################################################################
# self_efficacy_cig
###############################################################################
this_outcome <- "self_efficacy_cig"

dat_largest_model_used <- data.frame(decision_point = 2:max_dp)

for(mi_dataset_num in 1:M){
  dat_largest_model_used[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  for(current_dp_value in 2:max_dp){
    list_largest_model_used <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                 "sequentially-completed-datasets", 
                                                 mi_dataset_num, paste("largest_model_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))
    
    if(is.null(list_largest_model_used[["is_minimal"]])){
      val <- -1
    }else{
      val <- ifelse(list_largest_model_used[["is_minimal"]]==TRUE, 1, 0)
    }
    
    this_row <- which(dat_largest_model_used[["decision_point"]] == current_dp_value)
    dat_largest_model_used[this_row, paste("m", mi_dataset_num, sep = "")] <- val
  }
}

###############################################################################
# Do some calculations
###############################################################################
dat_summary <- data.frame(decision_point = 2:max_dp, 
                          n_largest = rowSums(dat_largest_model_used[, 2:ncol(dat_largest_model_used)] == 1),
                          n_largest2 = rowSums(dat_largest_model_used[, 2:ncol(dat_largest_model_used)] == 0),
                          n_intercept_only = rowSums(dat_largest_model_used[, 2:ncol(dat_largest_model_used)] == -1))

dat_summary[["prop_largest"]] <- dat_summary[["n_largest"]]/M
dat_summary[["prop_largest2"]] <- dat_summary[["n_largest2"]]/M
dat_summary[["prop_intercept_only"]] <- dat_summary[["n_intercept_only"]]/M

###############################################################################
# Save
###############################################################################

write.csv(dat_summary, file = file.path("analysis-multiple-imputation", "formatted-output", paste("summary_largest_model_used_for_proximal_outcome_", st_num, ".csv", sep = "")), row.names = FALSE)
