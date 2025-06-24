rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
source("paths.R")

M <- .__total_imputed_datasets
max_dp <- .__maximum_march_forward

st_num <- 1
my_list <- list("cigarette_counts" = NULL,
                "motivation_cig" = NULL,
                "self_efficacy_cig" = NULL)

###############################################################################
# cigarette_counts
###############################################################################
this_outcome <- "cigarette_counts"
dat_logged_imputation_model <- data.frame(row.names = NULL)

for(mi_dataset_num in 1:M){
  dat_logged_imputation_model[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  list_logged_imputation_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                    "sequentially-completed-datasets", 
                                                    mi_dataset_num, 
                                                    paste("imputation_model_stratum", st_num, ".rds", sep = "")))
  
  dat_logged_imputation_model[1, paste("m", mi_dataset_num, sep = "")] <- do.call(paste, as.list(format(list_logged_imputation_model[[this_outcome]][[1]])))
}

my_list[[this_outcome]] <- dat_logged_imputation_model

###############################################################################
# motivation_cig
###############################################################################
this_outcome <- "motivation_cig"
dat_logged_imputation_model <- data.frame(row.names = NULL)

for(mi_dataset_num in 1:M){
  dat_logged_imputation_model[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  list_logged_imputation_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                    "sequentially-completed-datasets", 
                                                    mi_dataset_num, 
                                                    paste("imputation_model_stratum", st_num, ".rds", sep = "")))
  
  dat_logged_imputation_model[1, paste("m", mi_dataset_num, sep = "")] <- do.call(paste, as.list(format(list_logged_imputation_model[[this_outcome]][[1]])))
}

my_list[[this_outcome]] <- dat_logged_imputation_model

###############################################################################
# self_efficacy_cig
###############################################################################
this_outcome <- "self_efficacy_cig"
dat_logged_imputation_model <- data.frame(row.names = NULL)

for(mi_dataset_num in 1:M){
  dat_logged_imputation_model[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  list_logged_imputation_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                    "sequentially-completed-datasets", 
                                                    mi_dataset_num, 
                                                    paste("imputation_model_stratum", st_num, ".rds", sep = "")))
  
  dat_logged_imputation_model[1, paste("m", mi_dataset_num, sep = "")] <- do.call(paste, as.list(format(list_logged_imputation_model[[this_outcome]][[1]])))
}

my_list[[this_outcome]] <- dat_logged_imputation_model

###############################################################################
# Save
###############################################################################
saveRDS(my_list, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", paste("final_imputation_model_stratum", st_num, ".rds", sep = "")))

write.csv(my_list[["cigarette_counts"]], file = file.path("analysis-multiple-imputation", "formatted-output", paste("cigarette_counts_", "final_imputation_model_stratum", st_num, ".csv", sep = "")), row.names = FALSE)
write.csv(my_list[["motivation_cig"]], file = file.path("analysis-multiple-imputation", "formatted-output", paste("motivation_cig_", "final_imputation_model_stratum", st_num, ".csv", sep = "")), row.names = FALSE)
write.csv(my_list[["self_efficacy_cig"]], file = file.path("analysis-multiple-imputation", "formatted-output", paste("self_efficacy_cig_", "final_imputation_model_stratum", st_num, ".csv", sep = "")), row.names = FALSE)

rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
source("paths.R")

M <- .__total_imputed_datasets
max_dp <- .__maximum_march_forward

st_num <- 2
my_list <- list("cigarette_counts" = NULL,
                "motivation_cig" = NULL,
                "self_efficacy_cig" = NULL)

###############################################################################
# cigarette_counts
###############################################################################
this_outcome <- "cigarette_counts"
dat_logged_imputation_model <- data.frame(row.names = NULL)

for(mi_dataset_num in 1:M){
  dat_logged_imputation_model[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  list_logged_imputation_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                    "sequentially-completed-datasets", 
                                                    mi_dataset_num, 
                                                    paste("imputation_model_stratum", st_num, ".rds", sep = "")))
  
  dat_logged_imputation_model[1, paste("m", mi_dataset_num, sep = "")] <- do.call(paste, as.list(format(list_logged_imputation_model[[this_outcome]][[1]])))
}

my_list[[this_outcome]] <- dat_logged_imputation_model

###############################################################################
# motivation_cig
###############################################################################
this_outcome <- "motivation_cig"
dat_logged_imputation_model <- data.frame(row.names = NULL)

for(mi_dataset_num in 1:M){
  dat_logged_imputation_model[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  list_logged_imputation_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                    "sequentially-completed-datasets", 
                                                    mi_dataset_num, 
                                                    paste("imputation_model_stratum", st_num, ".rds", sep = "")))
  
  dat_logged_imputation_model[1, paste("m", mi_dataset_num, sep = "")] <- do.call(paste, as.list(format(list_logged_imputation_model[[this_outcome]][[1]])))
}

my_list[[this_outcome]] <- dat_logged_imputation_model

###############################################################################
# self_efficacy_cig
###############################################################################
this_outcome <- "self_efficacy_cig"
dat_logged_imputation_model <- data.frame(row.names = NULL)

for(mi_dataset_num in 1:M){
  dat_logged_imputation_model[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  list_logged_imputation_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                    "sequentially-completed-datasets", 
                                                    mi_dataset_num, 
                                                    paste("imputation_model_stratum", st_num, ".rds", sep = "")))
  
  dat_logged_imputation_model[1, paste("m", mi_dataset_num, sep = "")] <- do.call(paste, as.list(format(list_logged_imputation_model[[this_outcome]][[1]])))
}

my_list[[this_outcome]] <- dat_logged_imputation_model

###############################################################################
# Save
###############################################################################
saveRDS(my_list, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", paste("final_imputation_model_stratum", st_num, ".rds", sep = "")))

write.csv(my_list[["cigarette_counts"]], file = file.path("analysis-multiple-imputation", "formatted-output", paste("cigarette_counts_", "final_imputation_model_stratum", st_num, ".csv", sep = "")), row.names = FALSE)
write.csv(my_list[["motivation_cig"]], file = file.path("analysis-multiple-imputation", "formatted-output", paste("motivation_cig_", "final_imputation_model_stratum", st_num, ".csv", sep = "")), row.names = FALSE)
write.csv(my_list[["self_efficacy_cig"]], file = file.path("analysis-multiple-imputation", "formatted-output", paste("self_efficacy_cig_", "final_imputation_model_stratum", st_num, ".csv", sep = "")), row.names = FALSE)

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
dat_logged_imputation_model <- data.frame(dp = 2:max_dp)

for(mi_dataset_num in 1:M){
  dat_logged_imputation_model[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  for(current_dp_value in 2:max_dp){
    list_logged_imputation_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                      "sequentially-completed-datasets", 
                                                      mi_dataset_num, 
                                                      paste("imputation_model_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))
    
    this_row <- which(dat_logged_imputation_model[["decision_point"]] == current_dp_value)
    dat_logged_imputation_model[this_row, paste("m", mi_dataset_num, sep = "")] <- do.call(paste, as.list(format(list_logged_imputation_model[[this_outcome]][[1]])))
  }
}

my_list[[this_outcome]] <- dat_logged_imputation_model

###############################################################################
# motivation_cig
###############################################################################
this_outcome <- "motivation_cig"
dat_logged_imputation_model <- data.frame(dp = 2:max_dp)

for(mi_dataset_num in 1:M){
  dat_logged_imputation_model[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  for(current_dp_value in 2:max_dp){
    list_logged_imputation_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                      "sequentially-completed-datasets", 
                                                      mi_dataset_num, 
                                                      paste("imputation_model_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))
    
    this_row <- which(dat_logged_imputation_model[["decision_point"]] == current_dp_value)
    dat_logged_imputation_model[this_row, paste("m", mi_dataset_num, sep = "")] <- do.call(paste, as.list(format(list_logged_imputation_model[[this_outcome]][[1]])))
  }
}

my_list[[this_outcome]] <- dat_logged_imputation_model

###############################################################################
# self_efficacy_cig
###############################################################################
this_outcome <- "self_efficacy_cig"
dat_logged_imputation_model <- data.frame(dp = 2:max_dp)

for(mi_dataset_num in 1:M){
  dat_logged_imputation_model[[paste("m", mi_dataset_num, sep = "")]] <- NULL
  
  for(current_dp_value in 2:max_dp){
    list_logged_imputation_model <- readRDS(file.path(path_multiple_imputation_pipeline_data, 
                                                      "sequentially-completed-datasets", 
                                                      mi_dataset_num, 
                                                      paste("imputation_model_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))
    
    this_row <- which(dat_logged_imputation_model[["decision_point"]] == current_dp_value)
    dat_logged_imputation_model[this_row, paste("m", mi_dataset_num, sep = "")] <- do.call(paste, as.list(format(list_logged_imputation_model[[this_outcome]][[1]])))
  }
}

my_list[[this_outcome]] <- dat_logged_imputation_model

###############################################################################
# Save
###############################################################################
saveRDS(my_list, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", paste("final_imputation_model_stratum", st_num, ".rds", sep = "")))

write.csv(my_list[["cigarette_counts"]], file = file.path("analysis-multiple-imputation", "formatted-output", paste("cigarette_counts_", "final_imputation_model_stratum", st_num, ".csv", sep = "")), row.names = FALSE)
write.csv(my_list[["motivation_cig"]], file = file.path("analysis-multiple-imputation", "formatted-output", paste("motivation_cig_", "final_imputation_model_stratum", st_num, ".csv", sep = "")), row.names = FALSE)
write.csv(my_list[["self_efficacy_cig"]], file = file.path("analysis-multiple-imputation", "formatted-output", paste("self_efficacy_cig_", "final_imputation_model_stratum", st_num, ".csv", sep = "")), row.names = FALSE)

