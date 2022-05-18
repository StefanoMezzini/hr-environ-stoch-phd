# runtime: < 5 minutes
setwd('/scratch/st-mnoonan-1/stefano/hr-environ-stoch-masters') # if in sockeye
library('ctmm') # for continuous-time movement modeling

# import data
days_summarized <- readRDS('simulations/days-summarized.rds')

N <- nrow(days_summarized) # to dencrease computation time substantially (>~ 3 times)

tictoc::tic()
# use a for loop to save the models each time
for(i in 1:N) {
  # create an R script that fits a movement model to the data
  cat("setwd('/scratch/st-mnoonan-1/stefano/hr-environ-stoch-masters')",
      "library('ctmm')    # for continuous-time movement modeling",
      "library('dplyr')   # for data wrangling (e.g., %>%)",
      "library('purrr')   # for functional programming (e.g., map(), map_dbl())",
      "library('tidyr')   # for data wrangling (e.g., nested tibbles)",
      "source('functions/rgamma2.R') # rgamma() parameterized by mean and variance",
      "source('analysis/figures/mean-variance-trends-panel-data.R') # means, variances",
      "N_CORES <- parallel::detectCores() # cores to use in parallel computations",
      "ROW <- readRDS('simulations/days-summarized.rds') %>% # import data",
      paste0("  slice(", i, ") # take the ith row"),
      "",
      "# fit movement model (OUF)",
      "ROW[[1, 'model']] <-",
      paste0("  ctmm.fit(data = ROW[[1, 'tel']][[1]],"),
      paste0("           CTMM = ROW[[1, 'theta']][[1]],"),
      "           # cores > 1 doesn't work in Windows",
      "           control = list(cores = N_CORES)) %>% list()",
      paste0("saveRDS(ROW, 'analysis/sockeye-scripts/temp-files/days-row", i, ".rds')"),
      "",
      file = paste0('analysis/sockeye-scripts/temp-files/model-', i,
                    '-of-', N, '.R'),
      sep = '\n')
  
  # create a PBS script to run the R script
  cat('#!/bin/bash',
      '# ------------Sockeye Parameters----------------- #',
      '#PBS -l walltime=00:20:00,select=1:ncpus=32:mem=187gb',
      paste0('#PBS -N m', i, '/', N), # job name
      '#PBS -A st-mnoonan-1', # allocation
      # output file if the script runs fully
      paste0('#PBS -o /scratch/st-mnoonan-1/stefano/hr-environ-stoch-masters/',
             'analysis/sockeye-scripts/temp-files/output-model-', i, '.txt'),
      # output file if the script fails due to an error
      paste0('#PBS -e /scratch/st-mnoonan-1/stefano/hr-environ-stoch-masters/',
             'analysis/sockeye-scripts/temp-files/error-model-', i, '.txt\n'),
      '# ----------------Modules------------------------- #',
      'source $HOME/miniconda3/etc/profile.d/conda.sh',
      'conda activate stefano-ctmm\n',
      '# -----------------My Commands-------------------- #',
      paste0('Rscript /scratch/st-mnoonan-1/stefano/hr-environ-stoch-masters/',
             'analysis/sockeye-scripts/temp-files/model-', i, '.R'),
      file = paste0('analysis/sockeye-scripts/temp-files/pbs-model-', i, '.txt'),
      sep = '\n')
  
  # run the PBS script
  system(paste0('qsub /scratch/st-mnoonan-1/stefano/hr-environ-stoch-masters/',
                'analysis/sockeye-scripts/temp-files/pbs-model-', i, '.txt'))
}
tictoc::toc()
