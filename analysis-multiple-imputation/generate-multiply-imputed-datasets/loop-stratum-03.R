rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Generate imputed datasets for decision points within stratum 1
###############################################################################
for(.__current_idx in 1:.__total_imputed_datasets){
  for(.__current_dp in 2:.__maximum_march_forward){
    source("paths.R")
    source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-restrictions-stratum-03.R"))
    print(list_mice_model)
  }
}

