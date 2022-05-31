library('ctmm')    # for generating movement models
library('dplyr')   # for data wrangling (%>%, mutate(), slice(), filter(), etc.)
library('purrr')   # for functional programming (map_*(), etc.)
library('tidyr')   # for data wrangling (pivot_*(), etc.)
library('ggplot2') # for fancy plots
source('analysis/simulations/movement-model.R') # for consistency between scripts
source('functions/add_returns.R') # to add return trips to (0, 0)
source('functions/get_hr.R') # to extract gaussian HR estimate from spatial variance
theme_set(theme_bw() + # change default ggplot theme
            theme(legend.position = 'none', strip.background = element_blank(),
                  strip.text = element_blank()))

DELTA_T <- 30 # sample every 30 s
SAMPLES <- seq(0, 60 * 60 * 24, by = DELTA_T) # 24 hours in seconds
PROJ <- '+proj=aeqd +lon_0=0 +lat_0=0 +datum=WGS84' # equidistant projection

newd <- tibble(hour = 1:24, # 24 hours
               t = as.POSIXct(60 * 60 * hour, origin = '1970-01-01'),
               longitude = 0, # always at (0, 0) at the beginning of each hour
               latitude = 0,
               animal = 'test animal') %>%
  as.telemetry(projection = PROJ)
head(newd)
sims_loop <- simulate(model, data = newd, dt = DELTA_T, seed = 1, complete = TRUE)
sims_out <- sims_loop[sims_loop$t %% (60 * 60) < (60 * 30), ] # only first half hours
sims_out <- sims_out[sims_out$t < (60 * 60 * 24), ] # remove last data point

plot(rep(1, nrow(sims_loop)) ~ sims_loop$t, pch = '.', ylim = c(-1, 2))
points(rep(0, nrow(sims_out)) ~ sims_out$t, pch = '.')

fit_ctmm <- function(.data) {
  tictoc::tic() # start the "stopwatch"
  theta <- ctmm.guess(.data, interactive = FALSE)
  model <- ctmm.fit(data = .data, CTMM = theta)
  tictoc::toc() # print the elapsed time
  return(model)
}
                              # DELTA_t     :  5 minutes,  1 minute , 30 seconds
m_loop <- fit_ctmm(sims_loop) # Fitting time: 16 seconds, 65 seconds, 16 seconds
m_out <- fit_ctmm(sims_out)   # Fitting time:  8 seconds, 32 seconds,  8 seconds

summary(m_loop)
summary(m_out)
