# runtime on 32 cores with 187 GB of memory: 
setwd('/scratch/st-mnoonan-1/stefano/hr-environ-stoch-masters') # if in sockeye
library('ctmm')    # for continuous-time movement modeling
library('dplyr')   # for data wrangling (e.g., %>%)
library('purrr')   # for functional programming (e.g., map(), map_dbl())
library('tidyr')   # for data wrangling (e.g., nested tibbles)

N <- readRDS('simulations/days-summarized.rds') %>% nrow()

days_models <-
  map_dfr(1:N, \(i) paste0('analysis/sockeye-scripts/temp-files/days-row', i, '.rds') %>%
            readRDS())

days_models <-
  mutate(days_models,
         # mean exploration length
         t_expl = map_dbl(tel, \(x) as.numeric(max(x$timestamp))),
         # position variance
         pos_var = map_dbl(model, \(x) {
           if(! is.null(x)) {
             ctmm:::area.covm(x$sigma)
           } else NA_real_
         }),
         # calculate the HR as a function of the positional variance
         hr_50 = -2 * log(1 - 0.50) * pi * pos_var,
         hr_95 = -2 * log(1 - 0.95) * pi * pos_var)

saveRDS(days_models, 'simulations/days-final.rds')
