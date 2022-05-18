# runtime on 32 cores with 187 GB of memory: > 4 hours
setwd('/scratch/st-mnoonan-1/stefano/hr-environ-stoch-masters') # if in sockeye
library('ctmm')    # for continuous-time movement modeling
library('dplyr')   # for data wrangling (e.g., %>%)
library('purrr')   # for functional programming (e.g., map(), map_dbl())
library('tidyr')   # for data wrangling (e.g., nested tibbles)

m <- ctmm(tau = c(Inf, 1), sigma = 0.1, mu = c(0, 0)) # model for return tracks
tracks <- readRDS('simulations/labelled-tracks.rds') # simulated tracks
types <- c('constant', 'linear', 'cyclical', 'drifting', 'erratic') # trend types

days <- readRDS('simulations/days.rds')

cat('Adding return trips...\n',
    file = 'analysis/sockeye-scripts/progress.txt', append = FALSE)

days_summarized <-
  days %>% # times at which animals reached satiety on each day in each environment
  # add labelled telemetry tracks for each day
  left_join(readRDS('simulations/tracks.rds'), by = 'day') %>%
  # group by panel and animal
  group_by(mean, variance, animal) %>%
  # add a return track to (0, 0) after satiety is reached during exploration
  mutate(tel = map2(tel, t_expl,
                    \(.tel, t_end_seconds) {
                      # exploration path to reach satiety
                      expl <- .tel[.tel$t <= t_end_seconds, ]
                      # start and end of exploration path (needed for return)
                      ends <- expl[c(1, nrow(expl)), ]
                      # convert `expl` to data.frame to be bound to other data
                      expl <- data.frame(expl)
                      # add a return to the exploration so the animal loops back
                      loop <-
                        # return takes as long as exploration; same sampling frequency
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
                    }),
         # find the end of each "day"
         t_end = map_dbl(tel, \(y) max(y$t)),
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
  nest(tel = -c(mean, variance, animal, mu, sigma2))


cat('Converting data to telemetries and fiting variograms...\n',
    file = 'analysis/sockeye-scripts/progress.txt', append = TRUE)
days_summarized <-
  mutate(days_summarized,
         tel = map(tel, \(x) suppressMessages(as.telemetry(x))),
         theta = map(tel, \(x) ctmm.guess(data = x, interactive = FALSE)))

saveRDS(days_summarized, 'simulations/days-summarized.rds')
