setwd('/scratch/st-mnoonan-1/stefano/hr-environ-stoch-masters') # if in sockeye
getwd() # check directory is correct
library('ctmm')    # for continuous-time movement modeling
library('dplyr')   # for data wrangling (e.g., %>%)
library('purrr')   # for functional programming (e.g., map(), map_dbl())
library('furrr')   # for parallelized functional programming (e.g., future_map*())
library('tidyr')   # for data wrangling (e.g., nested tibbles)
library('ggplot2') # for fancy plots
library('cowplot') # for multi-panel fancy plots
source('functions/mean-variance-trends.R') # functions for trends in E(U) and V(U)
source('functions/rgamma2.R') # rgamma() parameterized by mean and variance
source('functions/pull_parameter.R') # to extract parameters from ctmm and akde objects
source('analysis/default-figure-styling.R') # defaults for figures (theme, size, ...)
source('analysis/figures/mean-variance-trends-panel-data.R') # create tibble of parameters

N_CORES <- 7 # 32 # number of cores to use in parallel computations
N_ANIMALS <- 2 # 200 # total number of animals in different locations (or timepoints)
REQUIRED <- 1e3 # calories required for satiety
m <- ctmm(tau = c(Inf, 1), sigma = 0.1, mu = c(0, 0)) # infinitely diffusive model

tracks <- readRDS('simulations/1e4-labelled-tracks.rds') %>%
  filter(t < 700000) %>% # enough to reach satiety in worst conditions
  filter(day < 3750) # too long of a track will cause unnest() to fail
types <- c('constant', 'linear', 'cyclical', 'drifting', 'erratic') # trend types

plan(multisession, workers = N_CORES)

if(TRUE) {
  # using for loops to reduce computation costs
  for(i in 1:(length(types))) {
    mean_fcn <- get(paste0('mean_', types[i]))
    for(j in 1:(length(types))) {
      var_fcn <- get(paste0('variance_', types[j]))
      # calculate the number of visits necessary for each animal in each day
      days <-
        # tibble of days (1:N_ANIMALS) and movement datasets
        tibble(animal = 1:200, d = list(tracks)) %>%
        mutate(mu = mean_fcn(animal)[[1]]$mu, # mean resources as a fcn of day
               sigma2 = var_fcn(animal)[[1]]$sigma2) %>% # var as a fcn of day
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
        # remove unneded columns (also avoids duplicated colnames with tracks)
        dplyr::select(-c(x, y, vx, vy, timestamp, longitude, latitude, food))
      # save the simulation
      saveRDS(days,
              paste0('simulations/days-5-by-5/', types[i], '-mean-',
                     types[j], '-variance-days-exploration',
                     Sys.time(), '.rds'))
      cat('Completed:', types[i], 'mean,', types[j], 'variance.\n')
    } # close `for` for variances
  } # close `for` for means
  
  # create a single tibble from all the simulations
  days <-
    expand_grid(mean = types, variance = types) %>%
    mutate(mean = factor(mean, levels = types),
           variance = factor(variance, levels = types),
           d = future_map2(mean, variance, # add the simulated tracks
                           \(x, y) paste0('simulations/days-5-by-5/',
                                          x, '-mean-',
                                          y, '-variance-days-exploration.rds') %>%
                             readRDS())) %>%
    unnest(d)
  if(any(! days$full)) warning(paste(sum(days$full), 'animals did *NOT* reach satiety!'))
  saveRDS(days, 'simulations/days.rds') # save all simulations together
} else {
  days <- readRDS('simulations/days.rds')
}

days_summarized <-
  days %>% # times at which animals reached satiety on each day in each environment
  # add labelled telemetry tracks for each day
  left_join(readRDS('simulations/1e4-tracks.rds'), by = 'day') %>%
  # add a return track to (0, 0) after satiety is reached during exploration
  mutate(tel = future_map2(tel, t_expl,
                           \(.tel, t_end_seconds) {
                             # exploration path to reach satiety
                             expl <- .tel[.tel$t <= t_end_seconds, ]
                             # start and end of exploration path (needed for return)
                             ends <- expl[c(1, nrow(expl)), ]
                             # convert `expl` to data.frame to be bound to other data
                             expl <- data.frame(expl)
                             # add a return to the exploration so the animal loops back
                             loop <-
                               # return takes as long as exploration; same sampling freq
                               simulate(object = m, data = ends, t = unique(expl$t)) %>%
                               # convert for data wrangling
                               data.frame() %>%
                               # convert for ease of use
                               tibble() %>%
                               # reverse time so animal returns home instead of exploring
                               mutate(t = rev(t) + max(ends$t), path = 'return') %>%
                               # add the exploration track
                               bind_rows(mutate(expl, path = 'exploration')) %>%
                               # arrange by time so exploration comes before return
                               arrange(t)
                             return(loop)
                           },
                           .options = furrr_options(seed = TRUE)),
         # find the end of each "day"
         t_end = map_dbl(tel, \(y) max(y$t)), # do NOT use future_: already parallelized
         # days end after the previous one(s)
         t_end = cumsum(t_end),
         # days start at end of previous one(s)
         t_start = lag(t_end),
         # the first day starts at time t = 0
         t_start = if_else(is.na(t_start), 0, t_start)) %>%
  # drop unnecessary columns
  dplyr::select(-c(t_expl, t_end)) %>%
  # 1 dataset / (day * animal * panel) --> a single big dataset
  unnest(tel) %>%
  mutate(
    # change t to be the date and time rather than the "time of day"
    t = t + t_start,
    # make timestamp reflect the new values of t
    timestamp =
      as.POSIXct(t, tz = 'UTC',
                 origin = as.POSIXct('1970-01-01 00:00 UTC')),
    # id for each animal (needed to create a telemetry object)
    individual.local.identifier = paste(mean, ' mean,',
                                        variance, 'variance,',
                                        'animal', animal)) %>%
  # drop unnecessary columns
  select(-c(cell_id, new_cell, full, path)) %>%
  # move the time columns to the beginning
  relocate(c(t_start, t), .after = day) %>%
  # a single big dataset --> 1 dataset (animal * panel), i.e., multiple days/animal now
  nest(tel = -c(mean, variance, animal, mu, sigma2)) %>%
  mutate(
    # create telemetry objects with each dataset (suppress message on min sampling time)
    tel = future_map(tel, \(x) suppressMessages(as.telemetry(x))),
    # estimate spatial needs
    theta = future_map(tel, \(x) ctmm.guess(data = x, interactive = FALSE)), # variogram
    model = future_map2(tel, theta, ctmm.fit), # movement model (OUF)
    pos_var = future_map_dbl(model, \(x) ctmm:::area.covm(x$sigma)), # position variance
    # calculate the HR as a function of the positional variance
    hr_50 = -2 * log(1 - 0.50) * pi * pos_var,
    hr_95 = -2 * log(1 - 0.95) * pi * pos_var)
# AKDE = future_map2(tel, model, akde), # HR autocorrelated kernel density estimate
# # extract 50% and 95% home range quantiles from AKDE (with CIs)
# hr_50 = future_map2_dbl(AKDE, 'area', \(x, y) pull_parameter(x, y, 0.50, 'est')),
# hr_50_lwr = future_map2_dbl(AKDE, 'area', \(x, y) pull_parameter(x, y, 0.50, 'low')),
# hr_50_upr = future_map2_dbl(AKDE, 'area', \(x, y) pull_parameter(x, y, 0.50, 'high')),
# hr_95 = future_map2_dbl(AKDE, 'area', \(x, y) pull_parameter(x, y, 0.95, 'est')),
# hr_95_lwr = future_map2_dbl(AKDE, 'area', \(x, y) pull_parameter(x, y, 0.95, 'low')),
# hr_95_upr = future_map2_dbl(AKDE, 'area', \(x, y) pull_parameter(x, y, 0.95, 'high')))

plan(sequential)

if(! grepl('scratch', getwd())) {
  # simulation figure
  hr_lab <- expression(Home~range~size~(italic(H))) # label for y axis
  p_sim <-
    ggplot(days_summarized) +
    facet_grid(mean ~ variance, scales = 'free_y') + # facet by trends in mean and variance
    # area between 50% and 95% HRs
    # geom_ribbon(aes(animal, ymin = hr_50, ymax = hr_95), fill = pal[3], alpha = 0.2) +
    # add lines for 50% and 95% HRs
    geom_line(aes(animal, hr_50), color = pal[3], lwd = 1) +
    geom_line(aes(animal, hr_95), color = pal[3]) +
    # add 95% CIs for 50% and 95% CIs (excessive?)
    # geom_ribbon(aes(animal, ymin = hr_50_lwr, ymax = hr_50_upr), fill = pal[3], alpha = 0.2) +
    # geom_ribbon(aes(animal, ymin = hr_95_lwr, ymax = hr_95_upr), fill = pal[3], alpha = 0.2) +
    # add better axis labels and remove ticks and grids
    scale_x_continuous('Time', breaks = NULL) +
    scale_y_continuous(hr_lab, breaks = NULL) +
    # remove facet names since they appear in the marginal panels
    theme(strip.background = element_blank(), strip.text = element_blank())
  
  # trends in the mean
  p_mean <-
    ggplot(d55, aes(t, mu)) +
    facet_grid(mean ~ ., switch = 'y') +
    geom_line(color = pal[1], lwd = 1) +
    scale_x_continuous(expression(italic(U)), breaks = NULL) +
    scale_y_continuous(expression(Mean~italic(U)), breaks = NULL) +
    theme(strip.background = element_blank(), axis.title.y = element_text(face = 'bold'),
          axis.title.x = element_text(colour = 'transparent'))
  
  # trends in the variance
  p_variance <-
    ggplot(d55, aes(t, sigma2)) +
    facet_grid(. ~ variance) +
    geom_line(color = pal[2], lwd = 1) +
    scale_x_continuous(expression(Variance~'in'~italic(U)), breaks = NULL, position = 'top') +
    scale_y_continuous(expression(italic(U)), breaks = NULL) +
    theme(strip.background = element_blank(), axis.title.x = element_text(face = 'bold'),
          axis.title.y = element_text(color = 'transparent'))
  
  # create a single plot
  plot_grid(plot_grid(NULL, p_variance, rel_widths = c(1, 4.4), nrow = 1),
            plot_grid(p_mean, p_sim, rel_widths = c(1, 4.5), nrow = 1),
            rel_heights = c(1, 4), nrow = 2)
  
  # save the plot as a png
  ggsave('figures/mean-variance-5-by-5-hr-sims.png', width = 8, height = 4.5, scale = 2,
         units = 'in', dpi = 'print', bg = 'white')
}
