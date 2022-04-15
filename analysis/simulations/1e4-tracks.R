library('ctmm')     # for generating movement models
library('dplyr')    # for data wrangling
library('purrr')    # for functional programming
library('parallel') # for parallel and clustered computing
source('functions/energetics-functions.R') # movement & costs based on animal mass
source('functions/get_tracks.R') # function to get simulated tracks from ctmm model

MASS <- 75e3 # an arbitrary mass (in grams)
N_DAYS <- 5 # number of "days" (i.e., simulations with different seeds)
SIMULATE_TRACKS <- TRUE # set to "TRUE" if you want to generate new tracks
WINDOWS <- TRUE # set to "TRUE" if running on a Windows system
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
  if(WINDOWS) {
    cl <- makeCluster(4) # make a cluster
    clusterExport(cl, c('HABITAT') )
  }
  tracks <- tibble(day = 1:N_DAYS, # a simulation for each day
                   tel = mclapply(X = day, # set a seed for consistent results
                                  FUN = get_tracks)) # function to generate tracks
  stopCluster(cl)
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
tracks <-
  mutate(tracks,
         track = map(.x = tel, # map seeds since they are 
                     .f = \(x) label_visits(tel = x, habitat = HABITAT))) %>%
  dplyr::select(-tel) %>% # remove telemetry column
  tidyr::unnest(track) # make a single, large tibble
readr::write_csv(tracks, 'simulations/1e4-tracks-and-visits.csv')
readr::read_csv('simulations/1e4-tracks-and-visits.csv') # check the csv is ok
beepr::beep(2)
