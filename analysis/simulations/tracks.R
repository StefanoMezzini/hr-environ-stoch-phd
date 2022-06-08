# does not require large amounts of RAM, takes < 1 minute
setwd('/scratch/st-mnoonan-1/stefano/hr-environ-stoch-phd') # if in sockeye
library('ctmm')   # for generating movement models
library('raster') # for working with raster data
library('dplyr')  # for data wrangling
library('purrr')  # for functional programming
source('functions/label_visits.R') # function for when animal moves to a new cell & eats
source('analysis/simulations/movement-model.R') # for consistency between scripts

# track progress on a file
cat('', file = 'analysis/sockeye-scripts/progress.txt', append = FALSE)

N_DAYS <- 100 # number of "days" (i.e., simulations with different seeds)
SIMULATE_TRACKS <- TRUE # set to "TRUE" if you want to generate new tracks

# extracts simulated tracks from a ctmm movement model for given sample times
get_tracks <- function(day, times = SAMPLES) {
  simulate(model, # ctmm movement model
           t = times, # sampling times in seconds
           seed = day, # set a seed for consistent results (different track each day)
           complete = TRUE, # add lat, long, and timestamp to the telemetry object
           crs = PROJECTION) # CRS projection string
}

# generate simulated tracks (will be truncated at satiety later)
cat('Generating tracks...\n', file = 'analysis/sockeye-scripts/progress.txt',
    append = TRUE)
if(SIMULATE_TRACKS) {
  tracks <- tibble(day = 1:N_DAYS, # a simulation for each day
                   tel = map(.x = day, # set a seed for consistent results
                             .f = get_tracks)) # function to generate tracks
  saveRDS(tracks, file = 'simulations/tracks.rds')
} else tracks <- readRDS('simulations/tracks.rds')

# find patch visits and calories consumed from the tracks
cat('Labelling tracks...\n', file = 'analysis/sockeye-scripts/progress.txt',
    append = TRUE)
tracks <- transmute(tracks, # drop tel column
                    day, # keep day column
                    track = map(.x = tel, # add a column of full tracks
                                .f = \(x) label_visits(.tel = x, .habitat = HABITAT)))

# make a single, large tibble (will need lots of RAM)
tracks <- tidyr::unnest(tracks, track)
saveRDS(tracks, file = 'simulations/labelled-tracks.rds')

# check if grid any animals moved out of the habitat raster
if(any(is.na(tracks$new_cell))) {
  warning(paste('CAUTION:', sum(tracks$new_cell), 'animals left the habitat raster!'))
  cat('\n\n* * *\n\n', sum(tracks$new_cell),
      'animals left the habitat raster!\n\n* * *\n\n',
      file = 'analysis/sockeye-scripts/progress.txt', append = TRUE)
}

# check scaling is consistent and sampling frequency is sufficiently high
if(FALSE) {
  library('ggplot2')
  theme_set(theme_bw())
  
  test <- readRDS('simulations/tracks.rds')
  
  track <- test$tel[[2]] %>%
    data.frame() %>%
    mutate(cell_id = cellFromXY(object = HABITAT,
                                xy = SpatialPoints.telemetry(test$tel[[2]])),
           new_cell = c(1, diff(cell_id)) != 0)
  mean(track$new_cell) # fraction of observations that are feeding events
  
  track_short <- filter(track, t < 1e4)
  
  test_habitat <- rasterToPoints(HABITAT, spatial = TRUE) %>% data.frame()
  test_habitat_small <- filter(test_habitat,
                               x >= min(track$x), x <= max(track$x),
                               y >= min(track$y), y <= max(track$y))
  
  ggplot(track_short, aes(x, y)) +
    coord_equal(xlim = range(track_short$x), ylim = range(track_short$y)) +
    geom_tile(data = test_habitat_small, fill = 'transparent', color = 'black') +
    geom_path() +
    geom_point(aes(color = new_cell, size = new_cell)) +
    geom_point(aes(0, 0), color = 'darkorange', size = 4) +
    scale_color_brewer('New cell', type = 'qual', palette = 6) +
    scale_size_manual('New cell', values = c(1, 2))
  
  ggplot(track, aes(x, y)) +
    coord_equal(xlim = range(track$x), ylim = range(track$y)) +
    geom_tile(data = test_habitat, fill = 'transparent', color = 'black') +
    geom_path() +
    geom_point(aes(color = new_cell, size = new_cell)) +
    geom_point(aes(0, 0), color = 'darkorange', size = 4) +
    scale_color_brewer('New cell', type = 'qual', palette = 6) +
    scale_size_manual('New cell', values = c(1, 2))
  
  test <- test %>%
    mutate(tel = map(tel, data.frame)) %>%
    tidyr::unnest(tel) %>%
    group_by(day) %>%
    mutate(new_cell = cellFromXY(object = HABITAT,
                                 xy = SpatialPoints.telemetry(tibble(x, y))) %>%
             suppressWarnings(),
           new_cell = c(0, diff(new_cell)) != 0)
  
  test %>%
    ggplot(aes(x, y)) +
    coord_equal(xlim = range(test$x), ylim = range(test$y)) +
    geom_tile(data = test_habitat, fill = 'transparent', color = 'black') +
    geom_path(aes(group = day), alpha = 0.4)
  
  test %>%
    group_by(day) %>%
    summarize(encounters = sum(new_cell)) %>%
    ggplot() +
    geom_histogram(aes(encounters), bins = 7, color = 'black')
  
  test %>%
    group_by(day) %>%
    summarize(encounters = sum(new_cell)) %>%
    pull(encounters) %>%
    range()
}