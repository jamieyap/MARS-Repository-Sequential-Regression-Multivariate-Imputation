rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
source("paths.R")

M <- .__total_imputed_datasets

st_num <- 1
my_list <- list("cigarette_counts" = NULL,
                "motivation_cig" = NULL,
                "self_efficacy_cig" = NULL)

###############################################################################
# cigarette_counts
###############################################################################
this_outcome <- "cigarette_counts"
dat_logged_convergence <- data.frame(row.names = NULL)

for(mi_dataset_num in 1:M){
  dat_logged_convergence[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  list_logged_convergence_initial_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                             "sequentially-completed-datasets", 
                                                             mi_dataset_num, 
                                                             paste("logged_convergence_stepwise_model_stratum", st_num, ".rds", sep = "")))
  
  dat_logged_convergence[1, paste("m", mi_dataset_num, sep = "")] <- list_logged_convergence_initial_model[[this_outcome]]
}

my_list[[this_outcome]] <- dat_logged_convergence

###############################################################################
# motivation_cig
###############################################################################
this_outcome <- "motivation_cig"
dat_logged_convergence <- data.frame(row.names = NULL)

for(mi_dataset_num in 1:M){
  dat_logged_convergence[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  list_logged_convergence_initial_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                             "sequentially-completed-datasets", 
                                                             mi_dataset_num, 
                                                             paste("logged_convergence_stepwise_model_stratum", st_num, ".rds", sep = "")))
  
  dat_logged_convergence[1, paste("m", mi_dataset_num, sep = "")] <- list_logged_convergence_initial_model[[this_outcome]]
}

my_list[[this_outcome]] <- dat_logged_convergence

###############################################################################
# self_efficacy_cig
###############################################################################
this_outcome <- "self_efficacy_cig"
dat_logged_convergence <- data.frame(row.names = NULL)

for(mi_dataset_num in 1:M){
  dat_logged_convergence[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  list_logged_convergence_initial_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                             "sequentially-completed-datasets", 
                                                             mi_dataset_num, 
                                                             paste("logged_convergence_stepwise_model_stratum", st_num, ".rds", sep = "")))
  
  dat_logged_convergence[1, paste("m", mi_dataset_num, sep = "")] <- list_logged_convergence_initial_model[[this_outcome]]
}

my_list[[this_outcome]] <- dat_logged_convergence

###############################################################################
# Do some calculations
###############################################################################
my_list2 <- list("cigarette_counts" = NULL,
                 "motivation_cig" = NULL,
                 "self_efficacy_cig" = NULL)

this_outcome <- "cigarette_counts"
my_list2[[this_outcome]] <- data.frame(n_converged = rowSums(my_list[[this_outcome]][, 1: ncol(my_list[[this_outcome]])]),
                                       prop_converged = rowMeans(my_list[[this_outcome]][, 1: ncol(my_list[[this_outcome]])]))

this_outcome <- "motivation_cig"
my_list2[[this_outcome]] <- data.frame(n_converged = rowSums(my_list[[this_outcome]][, 1: ncol(my_list[[this_outcome]])]),
                                       prop_converged = rowMeans(my_list[[this_outcome]][, 1: ncol(my_list[[this_outcome]])]))

this_outcome <- "self_efficacy_cig"
my_list2[[this_outcome]] <- data.frame(n_converged = rowSums(my_list[[this_outcome]][, 1: ncol(my_list[[this_outcome]])]),
                                       prop_converged = rowMeans(my_list[[this_outcome]][, 1: ncol(my_list[[this_outcome]])]))

dat_summary_prop_converged <- do.call(cbind, my_list2)

###############################################################################
# Save
###############################################################################
saveRDS(my_list, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", paste("reshaped_list_logged_convergence_stepwise_model_stratum", st_num, ".rds", sep = "")))
saveRDS(dat_summary_prop_converged, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", paste("dat_summary_prop_converged_stepwise_model_stratum", st_num, ".rds", sep = "")))

write.csv(dat_summary_prop_converged, file = file.path("analysis-multiple-imputation", "formatted-output", paste("dat_summary_prop_converged_stepwise_model_stratum", st_num, ".csv", sep = "")), row.names = FALSE)



rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
source("paths.R")

M <- .__total_imputed_datasets

st_num <- 2
my_list <- list("cigarette_counts" = NULL,
                "motivation_cig" = NULL,
                "self_efficacy_cig" = NULL)

###############################################################################
# cigarette_counts
###############################################################################
this_outcome <- "cigarette_counts"
dat_logged_convergence <- data.frame(row.names = NULL)

for(mi_dataset_num in 1:M){
  dat_logged_convergence[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  list_logged_convergence_initial_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                             "sequentially-completed-datasets", 
                                                             mi_dataset_num, 
                                                             paste("logged_convergence_stepwise_model_stratum", st_num, ".rds", sep = "")))
  
  dat_logged_convergence[1, paste("m", mi_dataset_num, sep = "")] <- list_logged_convergence_initial_model[[this_outcome]]
}

my_list[[this_outcome]] <- dat_logged_convergence

###############################################################################
# motivation_cig
###############################################################################
this_outcome <- "motivation_cig"
dat_logged_convergence <- data.frame(row.names = NULL)

for(mi_dataset_num in 1:M){
  dat_logged_convergence[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  list_logged_convergence_initial_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                             "sequentially-completed-datasets", 
                                                             mi_dataset_num, 
                                                             paste("logged_convergence_stepwise_model_stratum", st_num, ".rds", sep = "")))
  
  dat_logged_convergence[1, paste("m", mi_dataset_num, sep = "")] <- list_logged_convergence_initial_model[[this_outcome]]
}

my_list[[this_outcome]] <- dat_logged_convergence

###############################################################################
# self_efficacy_cig
###############################################################################
this_outcome <- "self_efficacy_cig"
dat_logged_convergence <- data.frame(row.names = NULL)

for(mi_dataset_num in 1:M){
  dat_logged_convergence[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  list_logged_convergence_initial_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                             "sequentially-completed-datasets", 
                                                             mi_dataset_num, 
                                                             paste("logged_convergence_stepwise_model_stratum", st_num, ".rds", sep = "")))
  
  dat_logged_convergence[1, paste("m", mi_dataset_num, sep = "")] <- list_logged_convergence_initial_model[[this_outcome]]
}

my_list[[this_outcome]] <- dat_logged_convergence

###############################################################################
# Do some calculations
###############################################################################
my_list2 <- list("cigarette_counts" = NULL,
                 "motivation_cig" = NULL,
                 "self_efficacy_cig" = NULL)

this_outcome <- "cigarette_counts"
my_list2[[this_outcome]] <- data.frame(n_converged = rowSums(my_list[[this_outcome]][, 1: ncol(my_list[[this_outcome]])]),
                                       prop_converged = rowMeans(my_list[[this_outcome]][, 1: ncol(my_list[[this_outcome]])]))

this_outcome <- "motivation_cig"
my_list2[[this_outcome]] <- data.frame(n_converged = rowSums(my_list[[this_outcome]][, 1: ncol(my_list[[this_outcome]])]),
                                       prop_converged = rowMeans(my_list[[this_outcome]][, 1: ncol(my_list[[this_outcome]])]))

this_outcome <- "self_efficacy_cig"
my_list2[[this_outcome]] <- data.frame(n_converged = rowSums(my_list[[this_outcome]][, 1: ncol(my_list[[this_outcome]])]),
                                       prop_converged = rowMeans(my_list[[this_outcome]][, 1: ncol(my_list[[this_outcome]])]))

dat_summary_prop_converged <- do.call(cbind, my_list2)

###############################################################################
# Save
###############################################################################
saveRDS(my_list, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", paste("reshaped_list_logged_convergence_stepwise_model_stratum", st_num, ".rds", sep = "")))
saveRDS(dat_summary_prop_converged, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", paste("dat_summary_prop_converged_stepwise_model_stratum", st_num, ".rds", sep = "")))

write.csv(dat_summary_prop_converged, file = file.path("analysis-multiple-imputation", "formatted-output", paste("dat_summary_prop_converged_stepwise_model_stratum", st_num, ".csv", sep = "")), row.names = FALSE)



rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
source("paths.R")

M <- .__total_imputed_datasets
max_dp <- .__maximum_march_forward

st_num <- 3
my_list <- list("cigarette_counts" = NULL,
                "motivation_cig" = NULL,
                "self_efficacy_cig" = NULL)

###############################################################################
# cigarette_counts
###############################################################################
this_outcome <- "cigarette_counts"
dat_logged_convergence <- data.frame(decision_point = 2:max_dp)

for(mi_dataset_num in 1:M){
  dat_logged_convergence[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  for(current_dp_value in 2:max_dp){
    list_logged_convergence_initial_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                               "sequentially-completed-datasets", 
                                                               mi_dataset_num, 
                                                               paste("logged_convergence_stepwise_model_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))
    
    this_row <- which(dat_logged_convergence[["decision_point"]] == current_dp_value)
    dat_logged_convergence[this_row, paste("m", mi_dataset_num, sep = "")] <- list_logged_convergence_initial_model[[this_outcome]]
  }
}

my_list[[this_outcome]] <- dat_logged_convergence

###############################################################################
# motivation_cig
###############################################################################
this_outcome <- "motivation_cig"
dat_logged_convergence <- data.frame(decision_point = 2:max_dp)

for(mi_dataset_num in 1:M){
  dat_logged_convergence[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  for(current_dp_value in 2:max_dp){
    list_logged_convergence_initial_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                               "sequentially-completed-datasets", 
                                                               mi_dataset_num, 
                                                               paste("logged_convergence_stepwise_model_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))
    
    this_row <- which(dat_logged_convergence[["decision_point"]] == current_dp_value)
    dat_logged_convergence[this_row, paste("m", mi_dataset_num, sep = "")] <- list_logged_convergence_initial_model[[this_outcome]]
  }
}

my_list[[this_outcome]] <- dat_logged_convergence

###############################################################################
# self_efficacy_cig
###############################################################################
this_outcome <- "self_efficacy_cig"
dat_logged_convergence <- data.frame(decision_point = 2:max_dp)

for(mi_dataset_num in 1:M){
  dat_logged_convergence[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  for(current_dp_value in 2:max_dp){
    list_logged_convergence_initial_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                               "sequentially-completed-datasets", 
                                                               mi_dataset_num, 
                                                               paste("logged_convergence_stepwise_model_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))
    
    this_row <- which(dat_logged_convergence[["decision_point"]] == current_dp_value)
    dat_logged_convergence[this_row, paste("m", mi_dataset_num, sep = "")] <- list_logged_convergence_initial_model[[this_outcome]]
  }
}

my_list[[this_outcome]] <- dat_logged_convergence

###############################################################################
# Do some calculations
###############################################################################
my_list2 <- list("cigarette_counts" = NULL,
                 "motivation_cig" = NULL,
                 "self_efficacy_cig" = NULL)

this_outcome <- "cigarette_counts"
my_list2[[this_outcome]] <- data.frame(decision_point = 2:max_dp, 
                                       n_converged = rowSums(my_list[[this_outcome]][, 2: ncol(my_list[[this_outcome]])]),
                                       prop_converged = rowMeans(my_list[[this_outcome]][, 2: ncol(my_list[[this_outcome]])]))

this_outcome <- "motivation_cig"
my_list2[[this_outcome]] <- data.frame(decision_point = 2:max_dp, 
                                       n_converged = rowSums(my_list[[this_outcome]][, 2: ncol(my_list[[this_outcome]])]),
                                       prop_converged = rowMeans(my_list[[this_outcome]][, 2: ncol(my_list[[this_outcome]])]))

this_outcome <- "self_efficacy_cig"
my_list2[[this_outcome]] <- data.frame(decision_point = 2:max_dp, 
                                       n_converged = rowSums(my_list[[this_outcome]][, 2: ncol(my_list[[this_outcome]])]),
                                       prop_converged = rowMeans(my_list[[this_outcome]][, 2: ncol(my_list[[this_outcome]])]))

dat_summary_prop_converged <- do.call(cbind, my_list2)

###############################################################################
# Save
###############################################################################
saveRDS(my_list, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", paste("reshaped_list_logged_convergence_stepwise_model_stratum", st_num, ".rds", sep = "")))
saveRDS(dat_summary_prop_converged, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", paste("dat_summary_prop_converged_stepwise_model_stratum", st_num, ".rds", sep = "")))

write.csv(dat_summary_prop_converged, file = file.path("analysis-multiple-imputation", "formatted-output", paste("dat_summary_prop_converged_stepwise_model_stratum", st_num, ".csv", sep = "")), row.names = FALSE)

