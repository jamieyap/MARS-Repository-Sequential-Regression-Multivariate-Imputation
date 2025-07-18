rm(list = ls())

###############################################################################
# Data preparation
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

source(file.path("analysis-multiple-imputation", "create-replicated-dataset.R"))
source(file.path("analysis-multiple-imputation", "create-wide-format-dataset-for-mi.R"))

###############################################################################
# Generate imputed datasets for baseline variables
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

source(file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-baseline.R"))

###############################################################################
# Generate imputed datasets for decision points within each stratum
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

source(file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "loop-stratum-01.R"))
source(file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "loop-stratum-02.R"))
source(file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "loop-stratum-03.R"))

source(file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "check-convergence-of-initial-model.R"))
source(file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "check-convergence-of-stepwise-model.R"))
source(file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "check-final-model.R"))

###############################################################################
# Data preparation
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "reshape-completed-datasets-from-wide-to-long.R"))
}

###############################################################################
# Data analysis
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-marginal.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-moderator.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-study-day-linear.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-study-day-quadratic.R"))
}

###############################################################################
# Pooling estimates across imputed datasets and posterior predictive checking
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-primary.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-primary-moderator.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-primary-study-day-linear.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-primary-study-day-quadratic.R"))

###############################################################################
# Data analysis
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-marginal.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-moderator.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-study-day-linear.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-study-day-quadratic.R"))
}

###############################################################################
# Pooling estimates across imputed datasets and posterior predictive checking
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary-moderator.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary-study-day-linear.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary-study-day-quadratic.R"))

###############################################################################
# Pooling estimates across imputed datasets and posterior predictive checking
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "estimate-time-specific-means-for-mi-data.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "estimate-time-specific-means-for-replicated-data.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "ppc-time-specific-means.R"))

###############################################################################
# Pooling estimates across imputed datasets and posterior predictive checking
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "estimate-means-by-moderator-level-for-mi-data.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "estimate-means-by-moderator-level-for-replicated-data.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "ppc-means-by-prior-self-efficacy.R"))

###############################################################################
# Plot CC and MI results together
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

source(file.path("plot-cc-and-mi-together", "plot-primary.R"))
source(file.path("plot-cc-and-mi-together", "plot-secondary.R"))
source(file.path("plot-cc-and-mi-together", "plot-time-specific-means-by-dp.R"))
source(file.path("plot-cc-and-mi-together", "plot-means-by-prior-self-efficacy.R"))

###############################################################################
# Plot descriptive statistics
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

source(file.path("plot-cc-and-mi-together", "descriptive-stats-eligibility-pattern.R"))


