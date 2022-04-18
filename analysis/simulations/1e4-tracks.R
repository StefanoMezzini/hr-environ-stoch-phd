library('ctmm')  # for generating movement models
library('dplyr') # for data wrangling
library('purrr') # for functional programming
library('furrr') # for parallel and clustered computing
source('functions/energetics-functions.R') # movement & costs based on animal mass

MASS <- 75e3 # an arbitrary mass (in grams)
N_DAYS <- 1e4 # number of "days" (i.e., simulations with different seeds)
SIMULATE_TRACKS <- TRUE # set to "TRUE" if you want to generate new tracks
N_CORES <- 7 # number of cores to use for parallel computation
HABITAT <- create_raster(mass = MASS, width = 4) # raster of patches
model <- ctmm(tau = c(Inf, 1), sigma = 0.1) # infinitely diffusive movement model
SAMPLES <- sampling(mass = MASS, crossings = CROSSINGS) # sampling times

# extracts simulated tracks from a ctmm movement model for given sample times
get_tracks <- function(day, times = SAMPLES) {
  simulate(model, # ctmm movement model
           t = times, # sampling times in seconds
           seed = day, # set a seed for consistent results (different track each day)
           complete = TRUE) # add lat, long, and timestamp to the telemetry object
}

# generate simulated tracks (will be truncated at satiety later)
if(SIMULATE_TRACKS) {
  tictoc::tic()
  plan(multiprocess, workers = 7)
  tracks <- tibble(day = 1:N_DAYS, # a simulation for each day
                   tel = future_map(.x = day, # set a seed for consistent results
                                    .f = get_tracks, # function to generate tracks
                                    # don't check; seeds are set manually in simulate()
                                    .options = furrr_options(seed = NULL)))
  plan(sequential)
  tictoc::toc()
  saveRDS(tracks, file = 'simulations/1e4-tracks.rds')
  beepr::beep(sound = 2) # notify when done
} else tracks <- readRDS('simulations/1e4-tracks.rds')

if(FALSE) {
  # plot a subset of the first track and its akde (takes ~ 1 minute)
  plot(tracks$tel[[1]][1:500, ], type = 'l')
  plot(akde(data = tracks$tel[[1]][1:500, ],
            CTMM = ctmm.fit(data = tracks$tel[[1]][1:500, ],
                            CTMM = ctmm.guess(tracks$tel[[1]][1:500, ],
                                              interactive =FALSE))),
       add = TRUE)
}

# find patch visits and calories consumed from the 10 000 tracks
tracks <- transmute(tracks, # drop tel column
                    day, # keep day column
                    track = map(.x = tel,
                                .f = \(x) label_visits(tel = x, habitat = HABITAT)))

# make a single, large tibble (will need lots of RAM)
tracks <- tidyr::unnest(tracks, track)
saveRDS(tracks, file = 'simulations/1e4-labelled-tracks.rds')
beepr::beep(2)
