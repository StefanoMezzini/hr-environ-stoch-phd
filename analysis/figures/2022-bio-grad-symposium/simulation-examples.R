library('ctmm')      # for movement models
library('ggplot2')   # for fancy plots
library('gganimate') # for amimated fancy plots
library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('purrr')     # for functional programming
library('furrr')     # for parallel computing
source('functions/rgamma2.R') # rgamma() as a function of mean and variance
source('functions/qgamma2.R') # qgamma() as a function of mean and variance
source('functions/eat.R') # to eat food in each tile
source('functions/create_animated_raster.R') # to create standard animation
source('functions/energetics-functions.R') # movement & costs based on mass
source('analysis/figures/default-figure-styling.R') # for color palettes
animate <- gganimate::animate # don't use terra::animate
select <- dplyr::select # don't use raster::select

# set up default gganimate parameters
options(gganimate.dev_args = list(width = 6, height = 6, units = 'in',
                                  res = 300))

# change function to return coordinates as long-lat instead of x-y
SpatialPoints.telemetry <- function (object, ...) {
  object <- ctmm:::listify(object)
  SP <- lapply(object, function(d) {
    sp::SpatialPoints(`[.data.frame`(d, c('longitude', 'latitude')),
                      proj4string = sp::CRS(attr(d, 'info')$projection))
  })
  SP <- do.call(sp::rbind.SpatialPoints, SP)
  return(SP)
}

# choose default theme
theme_set(theme_bw() +
            theme(panel.border = element_blank(),
                  legend.key.height = unit(0.3, units = 'in'),
                  axis.title.x = element_text(color = '#F58700', size = 20, vjust = -1),
                  axis.title.y = element_text(color = '#3A6692', size = 20, vjust = 2),
                  legend.text = element_text(size = 20),
                  legend.title = element_text(size = 20)))

# set up parameters ----
# E(food) and V(food) cannot be independent bc E(food) == 0 => V(food) == 0
# food ~ Gamma(k, theta) => mean = k * theta, var = k * theta^2
# using alternative parameterization: food ~ Gamma(E(food), V(food))
set.seed(1)                                 # for constant results
required <- 3e3                             # min food for satiety
min.mu <- 5                                 # minimum mean
max.mu <- 25                                # maximum mean
min.s2 <- 1                                 # minimum variance
max.s2 <- 25^2                              # maximum variance
mu_fcn <- function(.x) min.mu + .x * (max.mu - min.mu) / 25 # .x in [1, 25]
s2_fcn <- function(.y) min.s2 + .y * (max.s2 - min.s2) / 25 # .y in [1, 25]
K <- 200

# set up matrix of means, variances, food (animals can re-visit a location) ----
# tibble of raster cell values
m_tbl <-
  # create all combinations of mu and s2
  expand_grid(x = seq(1, 25, length.out = K), y = seq(1, 25, length.out = K))%>%
  mutate(mu = mu_fcn(x),
         s2 = s2_fcn(y),
         food = rgamma2(mu = mu, sigma2 = s2)^3, # cube for stronger contrast 
         max_food = qgamma2(0.9, mu, s2, lower.tail = TRUE), # add upper limit
         food = if_else(food < max_food, food, max_food))

# raster of resources
m_rast <-
  m_tbl %>%
  select(-c(mu, s2, max_food)) %>%
  arrange(x, desc(y)) %>% # sort by increasing mu and decreasing s2
  pivot_wider(values_from = food, names_from = x) %>% # pivot to matrix format
  dplyr::select(-y) %>% # remove the s2 column
  as.matrix() %>% # convert to a matrix
  raster(xmn = 1, xmx = 25, ymn = 1, ymx = 25) # convert to raster

# add resources to the raster in tibble format
m_tbl <-
  m_tbl %>%
  arrange(desc(s2), mu) %>% # sort by columns (up -> down), then rows (L -> R)
  mutate(cell_id = 1:n())

# plot the raster of resources
p_rast <-
  ggplot() +
  coord_equal() +
  geom_raster(aes(x, y, fill = food), m_tbl) +
  scale_fill_gradient(expression(atop('Resources', '')), low = 'white',
                      high = pal[7],
                      breaks = range(m_tbl$food), labels = c('Low', 'High')) +
  scale_x_continuous('Resource abundance', breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous('Resource unpredictability', breaks = NULL, expand=c(0, 0))

ggsave('figures/2022-bio-grad-symposium/resource-raster.png', plot = p_rast,
       height = 6, width = 8)

# generate simulated tracks ----
get_tracks <- function(seed, long, lat) {
  mass <- 40e3 # mass in g
  delta_t <- est_tau_v(mass) / 5 # need delta_t < tau_v for good speed estimates
  times <- seq(0, 60^2 * 14, by = delta_t) # sampling < 24 hours
  .tel <- tibble(longitude = 0, latitude = 0,
                 t = '1970-01-01 00:00:00',
                 individual.local.identifier = 1) %>%
    as.telemetry() %>%
    suppressMessages()
  
  # IOU movement model for an animal of ~ 40 kg
  model <- ctmm(tau = c(Inf, est_tau_v(mass)),
                sigma = est_var_pos(mass),
                mu = c(0, 0)) # generate centered at (0, 0)
  
  s <-
    simulate(model, # ctmm movement model
             t = times, # sampling times in seconds
             seed = seed, # set a seed for consistent results (different track each day)
             data = .tel, # start & end points so the animal starts and ends at (0, 0)
             complete = TRUE)
  
  # shift to the starting points
  s$longitude <- s$longitude + long
  s$latitude <- s$latitude + lat
  return(s)
}

# generate simulated tracks ----
tracks <-
  tibble(location = 1:5, # a simulation for each day
         start_x = c(0.2, 0.8, 0.2, 0.8, 0.5) * 25, # four corners + the center
         start_y = c(0.2, 0.2, 0.8, 0.8, 0.5) * 25,
         start_mu = mu_fcn(start_x),
         start_s2 = s2_fcn(start_y),
         tel = imap(.x = location, # function to generate tracks
                    .f = \(s, i) get_tracks(seed = s,
                                            long = start_x[i],
                                            lat = start_y[i]))) %>%
  mutate(tel = map(tel, \(track) {
    track %>%
      data.frame() %>%
      mutate(cell_id = cellFromXY(m_rast, SpatialPoints.telemetry(track)) %>%
               suppressWarnings(),
             new_cell = c(1, diff(cell_id)), # check if moved to new cell
             new_cell = new_cell != 0) # convert to TRUE/FALSE
  })) %>%
  unnest(tel) %>%
  # truncate the tracks at satiety
  left_join(select(m_tbl, c(cell_id, food)), by = c('cell_id')) %>%
  group_by(location) %>%
  mutate(food = if_else(new_cell, food, 0), # animal can only eat in new cells
         satiety = cumsum(food),
         full = satiety > required) %>%
  filter(satiety <= required | (full & ! duplicated(full)))

# repeat 50 times to estimate HR ----
#' see `analysis/figures/proposal-defense/animated-simulation-figure.R` for more info
tracks <- readRDS('analysis/figures/proposal-defense/50-tracks.rds')

# static plot
p_track <-
  ggplot() +
  coord_equal() +
  geom_raster(aes(x, y, fill = food), m_tbl) +
  geom_path(aes(longitude, latitude, group = paste(location, rep)), tracks) +
  geom_point(aes(start_x, start_y, group = location),
             filter(tracks, ! duplicated(location)), shape = 4, color = 'white') +
  scale_fill_gradient(expression(atop('Resources', '')), low = 'white', high = pal[7],
                      breaks = range(m_tbl$food), labels = c('Low', 'High')) +
  scale_x_continuous('Resource abundance', breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous('Resource unpredictability', breaks = NULL, expand = c(0, 0))

ggsave('figures/2022-bio-grad-symposium/static-movement.png', plot = p_track,
       height = 6, width = 8)

# calculate the average HR by location
#' see `analysis/figures/proposal-defense/animated-simulation-figure.R` for more info
hrs <-
  readRDS('analysis/figures/proposal-defense/50-tracks-hrs.rds') %>%
  mutate(hr_95 = map(akde, \(x) SpatialPolygonsDataFrame.UD(x,
                                                            level.UD = 0.999,
                                                            level = 0) %>%
                       spTransform(CRS("+proj=longlat")) %>%
                       fortify())) %>%
  select(location, hr_95) %>%
  unnest(hr_95) %>%
  filter(grepl('est', group))

# plot the tracks with only the 95% HRs
p_track_hr_95 <-
  ggplot() +
  coord_equal() +
  geom_raster(aes(x, y, fill = food), m_tbl) +
  geom_path(aes(longitude, latitude, group = paste(location, rep)), tracks) +
  geom_polygon(aes(long, lat, group = paste(location, group)), hrs,
               fill = '#009900', color = NA, alpha = 0.8) +
  geom_point(aes(start_x, start_y, group = location), shape = 4,
             filter(tracks, ! duplicated(location)), color = 'white') +
  scale_fill_gradient(expression(atop('Resources', '')), low = 'white', high = pal[7],
                      breaks = range(m_tbl$food), labels = c('Low', 'High')) +
  scale_x_continuous('Resource abundance', breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous('Resource unpredictability', breaks = NULL, expand = c(0, 0))

ggsave('figures/2022-bio-grad-symposium/static-movement-hrs.png',
       plot = p_track_hr_95, height = 6, width = 8)
