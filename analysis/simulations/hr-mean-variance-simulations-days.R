# runtime on 32 cores with 187 GB of memory: ~ 10 minutes
setwd('/scratch/st-mnoonan-1/stefano/hr-environ-stoch-masters') # if in sockeye
library('ctmm')  # for continuous-time movement modeling
library('dplyr') # for data wrangling (e.g., %>%)
library('purrr') # for functional programming (e.g., map(), map_dbl())
library('furrr') # for parallelized functional programming (e.g., future_map*())
library('tidyr') # for data wrangling (e.g., nested tibbles)
source('analysis/figures/mean-variance-trends-panel-data.R') # trends in E(U) and V(U)
source('functions/rgamma2.R') # rgamma() parameterized by mean and variance

# create a blank file to keep track of progress on sockeye
cat('', file = 'analysis/sockeye-scripts/progress.txt', append = FALSE)

N_CORES <- parallel::detectCores() # number of cores to use in parallel computations
REQUIRED <- 1e3 # calories required for satiety

tracks <- readRDS('simulations/labelled-tracks.rds') # movement tracks
types <- c('constant', 'linear', 'cyclical', 'drifting', 'erratic') # trend types

days <-
  # tibble of animals and movement dataset
  d55 %>%
  mutate(d = list(tracks)) %>%
  unnest(d) %>% # unnest the datasets so we have a single, large tibble
  # generate the food for each row from a gamma distribution
  mutate(food = rgamma2(mu = mu, sigma2 = sigma2, N = n()),
         # if the animal visits a new cell, it finds food, otherwise it doesn't
         food = if_else(new_cell, food, 0)) %>%
  # end the movement once the animal has reached satiety
  group_by(day, animal) %>%
  # calculate the total number of visits, total calories, and if animal is full
  mutate(satiety = cumsum(food), # use for diagnostics if animals don't get full
         full = satiety >= REQUIRED) %>% # did the animal reach the required needs?
  filter(full, ! duplicated(full)) %>% # take the 1st row where the animal is full
  rename(t_expl = t) %>% # to avoid duplicated colnames with tracks
  # remove unneeded columns (also avoids duplicated colnames with tracks)
  dplyr::select(-c(x, y, vx, vy, timestamp, longitude, latitude, food))

plan(multisession, workers = N_CORES)

if(any(! days$full)) {
  warning(paste('CAUTION:', sum(days$full), 'animals did *NOT* reach satiety!'))
  cat('\n\n* * *\n\n', sum(days$full), 'animals did *NOT* reach satiety!\n\n* * *\n\n',
      file = 'analysis/sockeye-scripts/progress.txt', append = TRUE)
}

plan(sequential)

saveRDS(days, 'simulations/days.rds') # save all simulations together
