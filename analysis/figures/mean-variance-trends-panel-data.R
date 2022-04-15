#' created a separate script for `d55` since it's used in multiple scripts

library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling (expand_grid())
source('functions/mean-variance-trends.R')  # functions to generate means and variances

N <- 200 # number of generations
types <- c('constant', 'linear', 'cyclical', 'drifting', 'erratic') # functions types

# create a tibble of parameters
d55 <-
  expand_grid(mean = types,
              variance = types,
              t = list(tibble(t = 1:N))) %>%
  mutate( # assign trends in means and variances depending on the type
    mu = case_when(mean == 'constant' ~ mean_constant(generations = 1:N),
                   mean == 'linear' ~ mean_linear(generations = 1:N),
                   mean == 'cyclical' ~ mean_cyclical(generations = 1:N),
                   mean == 'drifting' ~ mean_drifting(generations = 1:N),
                   mean == 'erratic' ~ mean_erratic(generations = 1:N)),
    sigma2 = case_when(variance == 'constant' ~ variance_constant(generations = 1:N),
                       variance == 'linear' ~ variance_linear(generations = 1:N),
                       variance == 'cyclical' ~ variance_cyclical(generations = 1:N),
                       variance == 'drifting' ~ variance_drifting(generations = 1:N),
                       variance == 'erratic' ~ variance_erratic(generations = 1:N))) %>%
  unnest(c(t, mu, sigma2)) %>%
  mutate(mean = factor(mean, levels = types),
         variance = factor(variance, levels = types))
