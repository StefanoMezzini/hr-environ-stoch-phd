library('ctmm')    # for continuous-time movement modeling
library('dplyr')   # for data wrangling (e.g., %>%)
library('purrr')   # for functional programming (e.g., map(), map_dbl())
library('tidyr')   # for data wrangling (e.g., nested tibbles)
library('ggplot2') # for fancy plots
source('functions/mean-variance-trends.R') # functions for trends in E(U) and V(U)
source('functions/rgamma2.R') # rgamma() parameterized by mean and variance
source('analysis/figures/mean-variance-trends-panel-data.R') # create tibble of parameters

N_CORES <- 7 # 32 # number of cores to use in parallel computations
REQUIRED <- 1e3 # calories required for satiety
m <- ctmm(tau = c(Inf, 1), sigma = 0.1, mu = c(0, 0)) # infinitely diffusive model

tracks <- readRDS('simulations/1e4-labelled-tracks.rds') %>%
  filter(t <= 1e5) %>% # ~[4.5e4, 7e4] is enough to reach satiety in worst conditions
  filter(day <= 500) # too long of a track will cause unnest() to fail

WORST <- filter(d55, mu == min(mu)) %>% # lowest mean resources
  filter(sigma2 == max(sigma2)) # with highest variance

days <-
  # modify WORST to follow the syntax used in 'offspring-simulations.R'
  transmute(WORST, animal = t, mu, sigma2, d = list(tracks)) %>%
  unnest(d) %>% # unnest the datasets so we have a single, large tibble
  # generate the food for each row from a gamma distribution
  mutate(food = rgamma2(mu = mu, sigma2 = sigma2, N = n()),
         # if the animal visits a new cell, it finds food, otherwise it doesn't
         food = if_else(new_cell, food, 0)) %>%
  # end the movement once the animal has reached satiety
  group_by(day, animal) %>%
  # calculate the total number of visits, total calories, and if animal is full
  mutate(satiety = cumsum(food), # use for diagnostics if animals don't get full
         full = satiety >= REQUIRED) # did the animal reach the required needs?
days_end <- days %>%
  filter(full, ! duplicated(full)) %>% # take the 1st row where the animal is full
  rename(t_expl = t) %>% # to avoid duplicated colnames with tracks
  # remove unneded columns (also avoids duplicated colnames with tracks)
  dplyr::select(-c(x, y, vx, vy, timestamp, longitude, latitude, food))

max(days_end$t_expl) / max(tracks$t) # check max proportion of time used
sum(! days_end$full) # check how many did not reach satiety

ggplot(days_end, aes('', t_expl)) +
  geom_boxplot()

ggplot(days, aes(t, satiety, group = day)) +
  geom_line(alpha = 0.1) +
  geom_hline(yintercept = REQUIRED, color = 'red')
