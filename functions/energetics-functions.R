# script adapted from https://github.com/NoonanM/BallisticMotion
library('dplyr')  # for data wrangling (e.g., mutate(), %>%)
library('purrr')  # for functional programming (e.g., map(), map_dbl())
library('ctmm')   # for movement modeling and simulations
library('raster') # for raster data
source('functions/rgamma2.R') # rgamma() parameterized by mean and variance

CROSSINGS <- 10

# calculates the Straight Line (Euclidean) Distance between two points
sld <- function(x_1, y_1, x_2, y_2){
  sqrt((x_1 - x_2)^2 + (y_1 - y_2)^2)
}

# generates animal's var[position] based on mass (in g) ----
# model comes from Noonan et al. 2020 https://doi.org/10.1111/cobi.13495
est_var_pos <- function(mass, variance = FALSE) {
  
  HR <- 0.5078955 + 1.372162 * log10(mass) # calculate HR based on mass
  HR <- 10^(HR) # back-transform
  SIG <- HR / (-2 * log(0.05) * pi) # convert from 95% HR to var[position]
  
  # generate the value from a chi-squared distribution
  if(variance == TRUE){SIG <- rchisq(n = length(mass), df = SIG)}
  
  return(SIG)
}

# generates E[tau_p] based on mass (in g) ----
# model comes from Noonan et al. 2020  https://doi.org/10.1111/cobi.13495
est_tau_p <- function(mass, variance = FALSE) {
  
  tau_p <- 1.2994292 + 0.8129125 * log10(mass)
  tau_p <- 10^(tau_p) # back-transform
  
  # generate the value from a chi-squared distribution
  if(variance == TRUE){tau_p <- rchisq(n = length(mass), df = tau_p)}
  
  return(tau_p)
}

# generates E[tau_v] based on mass (in g) ----
# model comes from Noonan et al. 2020  https://doi.org/10.1111/cobi.13495
est_tau_v <- function(mass, variance = FALSE) {
  
  tau_v <- -1.365200 + 0.787177 * log10(mass)
  tau_v <- 10^(tau_v)# back-transform
  
  # generate the value from a chi-squared distribution
  if(variance == TRUE){tau_v <- rchisq(n = length(mass), df = tau_v)}
  
  return(tau_v)
}

# generates raster of food patches based on mass_prey (g) ----
# `width` is proportional to to (some function of) the distance between resources
create_raster <- function(mass, width = 20) {
  
  # var[position]
  SIG <- est_var_pos(mass)
  
  # Range of raster based on 99.9% HR area
  EXT <- round(sqrt((-2 * log(0.0001) * pi) * SIG))
  
  # number of patches based on fixed patch width
  N <- EXT / width
  
  # build the raster (N * N matrix)
  FOOD <- raster(matrix(data = 1, nrow = N, ncol = N),
                 xmx = EXT, xmn =- EXT,
                 ymx = EXT, ymn =- EXT)
  
  return(FOOD)
}

# counts the number of patches visited (assumes immediate renewal) ----
count_visits <- function(track, habitat, metric = 'patches') {
  
  # patch identities
  IDs <- suppressWarnings(cellFromXY(habitat, SpatialPoints.telemetry(track)))
  
  PATCHES <- sum(diff(IDs) != 0) # count number of times it moved to a new food patch
  
  TIME <- mean(rle(c(FALSE, diff(IDs) != 0))$lengths) # mean time between patches 
  
  if(metric == 'patches'){return(PATCHES)}
  if(metric == 'time'){return(TIME)}
}

# determines lifespan and sampling times based on mass (g) and number of crossings ----
sampling <- function(mass, crossings = CROSSINGS) {
  
  # total lifespan (based on number of range crossings)
  lifespan <- round(est_tau_p(mass)*crossings)
  
  # sampling interval (tau_v)
  interval <- round(est_tau_v(mass))
  
  # "Lifespan" and sampling interval for the simulations
  t <- seq(from = 0, to = lifespan, by = interval)
  
  #return the vector of sampling times
  return(t)
}

# estimates fitness passed to the offspring ----
generate_offspring <- function(patches, mass, models, crossings = CROSSINGS,
                               calories = 150) {
  
  # Extract movement speeds from the models
  SPEED <- map_dbl(models,
                   \(m) {
                     if(nrow(summary(m, units = FALSE)$CI) >= 4){
                       summary(m, units = FALSE)$CI[4,2]
                     } else Inf })
  
  # basal metabolic rate (in kj/day)
  # from Nagy 1987 https://doi.org/10.2307/1942620 
  FMR <- 0.774 + 0.727 * log10(mass)
  
  # back transform 
  FMR <- 10^FMR
  
  # total lifespan in days (based on number of range crossings)
  lifespan <- round(est_tau_p(mass) * crossings) / 60 / 60 / 24
  
  # Metabolic cost of movement in watts/kg
  # from Taylor et al. 1982 https://doi.org/10.1242/jeb.97.1.1 
  E <- 10.7 * (mass / 1000)^(-0.316) * SPEED + 6.03 * (mass / 1000)^(-0.303)
  
  # Convert to kJ/s
  E <- (E * (mass / 1000)) / 1000
  
  # Maximum running speed in km/hr
  # from Hirt et al. 2017 https://doi.org/10.1038/s41559-017-0241-4
  v_max <- 25.5 * (mass / 1000)^(0.26) * (1 - exp(-22 * (mass / 1000)^(-0.66)))
  
  # convert to m/s
  v_max <- v_max / 3.6
  
  # total energetic cost in kj as a function of FMR and movement speed
  COST <- FMR * lifespan + E * est_tau_p(mass) * crossings
  
  # excess energy
  excess <- patches * calories - COST
  excess[is.infinite(excess)] <- NA
  
  # define number of prey offspring based on their excess energy and metabolic rate
  offspring <- floor(excess / FMR)
  # change NAs and negatives to 0, cap at 5, and keep the rest the same
  offspring <- case_when(is.na(offspring) ~ 0,
                         offspring < 0 ~ 0,
                         offspring > 5 ~ 5,
                         TRUE ~ offspring)
  
  return(offspring)
}

run_replicate <- function(mass_g, generation, n_animals = N_ANIMALS,
                          crossings = CROSSINGS) {
  VAR_TAU_P <- (est_tau_p(mass_g) / 10)^2 # V(position autocorrelation) = (tau_p / 10)^2
  VAR_TAU_V <- (est_tau_v(mass_g) / 10)^2 # V(velocity autocorrelation) = (tau_v / 10)^2
  VAR_VAR_POS <- (est_var_pos(mass_g) / 10)^2 # V(V(position)) = (var_pos / 10)^2
  TIMES <- sampling(mass = mass_g, crossings = crossings) # sampling times
  HABITAT <- create_raster(mass = mass_g, width = 5)

  # generate animal movement parameters and models for all animals
  if(generation == 1){# if the first gen, generate movement parameters as functions of the mass
    # generate new parameters for the first generation
    params <-
      tibble(animal = 1:n_animals,
             mass = mass_g,
             tau_p = est_tau_p(mass = mass, variance = TRUE), # location autocorrelation
             tau_v = est_tau_v(mass = mass, variance = TRUE), # velocity autocorrelation
             var_pos = est_var_pos(mass = mass, variance = TRUE)) # positional variance
  } else {
    # sample parameters from offspring from previous gen and add some noise ("mutations")
    params <-
      tibble(animal = 1:n_animals,
             mass = mass_g,
             tau_p = sample(OFFSPRING$tau_p, n_animals) %>%
               rgamma2(sigma2 = VAR_TAU_P, N = n_distinct(animal)),
             tau_v = sample(OFFSPRING$tau_v, n_animals) %>%
               rgamma2(VAR_TAU_V, n_distinct(animal)),
             var_pos = sample(OFFSPRING$var_pos, n_animals) %>%
               rgamma2(VAR_VAR_POS, n_distinct(animal)))
  } # closes `else` statement
  
  params <- mutate(params,
                   lv = sqrt((tau_v / tau_p) * var_pos), # ballistic length scale
                   model = map(1:n_animals, # movement model
                               \(i) ctmm(tau = c(tau_p[i], tau_v[i]), sigma = var_pos[i],
                                         mu = c(0, 0))), # start from center of habitat
                   speed = map_dbl(model, \(x) summary(x, units = FALSE)$CI[4,2]),
                   track = map(model, \(x) simulate(x, t = TIMES)), # animal track
                   patches = map_dbl(track, \(x) count_visits(x, HABITAT)),
                   offspring = generate_offspring(patches = patches, mass = mass_g,
                                                  models = model, crossings = crossings))
  
  return(params)
}

# example ----
if(FALSE) {
  # an arbitrary mass (in grams)
  mass <- 75e3
  
  # estimate average parameters as a function of animal mass ----
  tau_p <- est_tau_p(mass = mass, variance = FALSE) # home range crossing time (seconds)
  tau_v <- est_tau_v(mass = mass, variance = FALSE) # directional persistence (seconds)
  spatial_var <- est_var_pos(mass = mass, variance = FALSE) # positional variance
  m <- ctmm(tau = c(tau_v, tau_p), sigma = spatial_var) # simulated movement model
  t <- sampling(mass = mass) # arbitrary time points for sampling
  habitat <- create_raster(mass = mass, width = 1) # raster of resources
  sim <- simulate(m, t = t) # simulated track
  patches <- count_visits(track = sim, habitat = habitat, metric = 'patches') # n visits
  generate_offspring(patches = patches, mass = mass, models = list(m))
  
  # using sim_movement to generate it all easily ----
  x <- run_replicate(mass_g = 75e3, generation = 1, n_animals = 10); x$offspring
}
