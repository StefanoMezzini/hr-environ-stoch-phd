library('dplyr')   # for data wrangling
library('purrr')   # for functional programming
library('tidyr')   # for data wrangling
library('ggplot2') # for fancy plots
library('cowplot') # for fancy multi-panel plots
library('ctmm')    # for movement modeling
source('functions/window_hr.R') # function to calculate HRs and create figures
theme_set(theme_bw()) # change default theme

data('buffalo') # attach movement dataset

# run moving window analysis for all buffalos ----
lapply(1:length(buffalo), function(i) {
  cat('Running models for', names(buffalo)[i], '.\n')
  window_hr(buffalo[[i]],
            window = 7 %#% 'day', # 1 week of data for sufficient sample size
            dt = 1 %#% 'day', # move window over by a single day each time
            fig_path = 'figures/variance-raster/buffalo-moving-window',
            rds_path = 'models/buffalo-moving-window')
})
