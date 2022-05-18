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
source('functions/energetics-functions.R') # movement & costs based on animal mass
animate <- gganimate::animate # don't use terra::animate
select <- dplyr::select # don't use raster::select

# set up default gganimate parameters
options(gganimate.dev_args = list(width = 6, height = 6, units = 'in', res = 300))

#' change `SpatialPoints.telemetry()` to return coordinates as long-lat instead of x-y
SpatialPoints.telemetry <- function (object, ...) {
  CLASS <- class(object)[1]
  object <- ctmm:::listify(object)
  SP <- lapply(object, function(d) {
    sp::SpatialPoints(`[.data.frame`(d, c('longitude', 'latitude')),
                      proj4string = sp::CRS(attr(d, 'info')$projection))
  })
  SP <- do.call(sp::rbind.SpatialPoints, SP)
  return(SP)
}

# two-point equidistant projection of the habitat raster
PROJ <- '+proj=tpeqd +lat_1=-25 +lon_1=25 +lat_2=25 +lon_2=-25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'

# set colors for raster palette
LOW <- '#744700'
MID <- '#d9bb94'
HIGH <- 'darkgreen'

# choose default theme
theme_set(theme_bw() + theme(legend.position = 'none', panel.border = element_blank()))

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
mu_fcn <- function(.x) min.mu + .x * (max.mu - min.mu) / 25 # assuming .x in [1, 25]
s2_fcn <- function(.y) min.s2 + .y * (max.s2 - min.s2) / 25 # assuming .y in [1, 25]

# set up matrix of means, variances, food (animals can re-visit a location) ----
# tibble of raster cell values
m_tbl <-
  # create all combinations of mu and s2
  expand_grid(x = seq(1, 25, length.out = 1e3), y = seq(1, 25, length.out = 1e3)) %>%
  mutate(mu = mu_fcn(x),
         s2 = s2_fcn(y),
         food = rgamma2(mu = mu, sigma2 = s2), # sample food from a Gamma distribution
         max_food = qgamma2(0.9, mu, s2, lower.tail = TRUE), # but add an upper boundary
         food = if_else(food < max_food, food, max_food))
hist(m_tbl$food, breaks = 20)
ggplot() +
  coord_equal() +
  geom_raster(aes(x, y, fill = food), m_tbl) +
  scale_fill_gradientn(colours = terrain.colors(n = 50, rev = TRUE)) +
  theme(legend.position = 'right')

# raster of resources
m_rast <-
  m_tbl %>%
  select(-c(mu, s2, max_food)) %>%
  arrange(x, desc(y)) %>% # sort by increasing mu and decreasing s2
  pivot_wider(values_from = food, names_from = x) %>% # pivot into a matrix format
  dplyr::select(-y) %>% # remove the s2 column
  as.matrix() %>% # convert to a matrix
  raster(xmn = 1, xmx = 25, ymn = 1, ymx = 25) # convert to raster
X11(); plot(m_rast) # compare the plots to ensure same raster orientation and axes
dev.off() # close X11() device

# add resources to the raster in tibble format
m_tbl <-
  m_tbl %>%
  arrange(desc(s2), mu) %>% # sort by columns (up -> down), then rows (left -> right)
  mutate(cell_id = 1:n())

# static base plot of resources
p0 <-
  ggplot(m_tbl, aes(x, y)) +
  coord_equal() +
  geom_raster(aes(fill = food)) +
  scale_x_continuous('Resource abundance', breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous('Environmental variance', breaks = NULL, expand = c(0, 0)) +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH)

# generate simulated tracks ----
get_tracks <- function(seed, long, lat) {
  mass <- 40e3 # mass in g
  delta_t <- est_tau_v(mass) / 5 # need delta_t < tau_v for good speed estimates
  times <- seq(0, 60^2 * 14, by = delta_t) # sampling < 24 hours
  .tel <- tibble(longitude = 0, latitude = 0,
                 t = '1970-01-01 00:00:00',
                 individual.local.identifier = 1) %>%
    as.telemetry(projection = PROJ) %>%
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
         start_x = c(0.2, 0.8, 0.2, 0.8, 0.5) * 25, # four corners and the center
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
             new_cell = c(1, diff(cell_id)), # check if the animal moved to a new cell
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

# plot the tracks
ggplot() +
  coord_equal() +
  geom_path(aes(longitude, latitude, color = factor(location)), tracks) +
  geom_point(aes(longitude, latitude), filter(tracks, t == 0)) +
  geom_rect(aes(xmin = 1, ymin = 1, xmax = 25, ymax = 25), fill = NA, color = 'red')

# check that all animals reached satiety
ggplot() +
  geom_path(aes(t, satiety, color = factor(location)), tracks) +
  geom_point(aes(t, satiety, color = factor(location)), filter(tracks, new_cell)) +
  geom_hline(yintercept = required, color = 'red') +
  geom_vline(xintercept = 60^2 * 6) +
  xlim(c(0, 60^2 * 12)) + # look at the full day
  theme(legend.position = 'right')

# note that satiety needs to be sufficiently high to obtain accurate estimates:
ggplot() +
  geom_path(aes(t, satiety, color = factor(location)), filter(tracks, satiety < 1000)) +
  geom_point(aes(t, satiety, color = factor(location)),
             filter(tracks, satiety < 1000, new_cell)) +
  theme(legend.position = 'right')

# check that cell IDs were added correctly
ggplot() +
  coord_equal() +
  geom_path(aes(longitude, latitude, group = location), tracks) +
  geom_point(aes(longitude, latitude, color = food), filter(tracks, new_cell), alpha=.3) +
  geom_point(aes(longitude, latitude, shape = factor(location)), filter(tracks, t == 0)) +
  scale_color_gradientn(colours = terrain.colors(50) %>% rev()) +
  theme(legend.position = 'right')

tracks %>%
  group_by(location) %>%
  summarize(mean = mean(food),
            var = var(food),
            n = n())

# static plot
p_track <-
  ggplot() +
  coord_equal() +
  geom_raster(aes(x, y, fill = food), m_tbl) +
  geom_path(aes(longitude, latitude, group = location), tracks) +
  geom_point(aes(x = start_x, y = start_y, group = location), tracks, shape = 4,
             color = 'white') +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH) +
  scale_color_brewer(type = 'qual', palette = 6, direction = -1) +
  scale_x_continuous('Resource abundance', breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous('Environmental variance', breaks = NULL, expand = c(0, 0)); p_track

ggsave('figures/proposal-defense/static-movement.png', plot = p_track, height = 6,
       width = 6)

a_track <-
  p_track +
  geom_point(aes(longitude, latitude, group = location), tracks) +
  transition_reveal(t)
anim_1 <- animate(a_track, duration = 10, start_pause = 7, end_pause = 15, nframes = 100,
                  renderer = magick_renderer()) # to avoid annoying green lines/areas
# gifs are too large to push to GitHub
anim_save('figures/proposal-defense/animated-movement.gif', animation = anim_1)

# repeat 50 times to estimate HR ----
if(TRUE) { # import the tracks, or set to FALSE to simulate them
  tracks <- readRDS('analysis/figures/proposal-defense/50-tracks.rds')
} else {
  plan(multisession, workers = 5)
  # generate simulated tracks
  tracks <-
    tibble(location = 1:5, # a simulation for each day
           start_x = c(5, 20, 5, 20, 12.5),
           start_y = c(5, 5, 20, 20, 12.5),
           start_mu = mu_fcn(start_x),
           start_s2 = s2_fcn(start_y),
           tel = future_imap(.x = location, # function to generate tracks
                             .f = \(s, i) {
                               map_dfr(1:50,
                                       \(.seed) {
                                         get_tracks(seed = .seed,
                                                    long = start_x[i],
                                                    lat = start_y[i]) %>%
                                           data.frame() %>% # for binding by rows
                                           mutate(rep = .seed) # split by replicate
                                       })
                             },
                             .options = furrr_options(seed = NULL))) %>%
    mutate(tel = future_map(tel, \(track) {
      track %>%
        data.frame() %>%
        mutate(cell_id = cellFromXY(m_rast, SpatialPoints.telemetry(track)) %>%
                 suppressWarnings(),
               new_cell = c(1, diff(cell_id)), # check if the animal moved to a new cell
               new_cell = new_cell != 0) # convert to TRUE/FALSE
    }, .options = furrr_options(seed = NULL))) %>%
    unnest(tel) %>%
    # truncate the tracks at satiety
    left_join(select(m_tbl, c(cell_id, food)), by = 'cell_id') %>%
    group_by(location, rep) %>%
    mutate(food = if_else(new_cell, food, 0),
           satiety = cumsum(food),
           full = satiety > required); beepr::beep(2)
  
  plan(sequential) # back to sequential computing to avoid crashes
  
  # check that all animals reached satiety
  tracks %>%
    group_by(location, rep) %>% # check all individual tracks
    summarize(full = any(full, na.rm = TRUE), # whether animal reached satiety or not
              out_of_grid = any(is.na(cell_id))) %>% # animals that moved out of the raster
    group_by(location) %>% # calculate totals by location
    summarize(unsatisfied = sum(!full),
              out_of_grid = sum(out_of_grid),
              n = n_distinct(rep))
  
  # check which animals were did not reach satiety
  tracks %>%
    group_by(location, rep) %>%
    mutate(compl = any(full)) %>%
    filter(! compl | is.na(full), t == max(t)) %>%
    dplyr::select(satiety, t)
  
  # remove portions of tracks after satiety
  tracks <- filter(tracks, satiety <= required | (full & ! duplicated(full)))
  saveRDS(tracks, 'analysis/figures/proposal-defense/50-tracks.rds')
}

# static plot
p_track <-
  ggplot() +
  coord_equal() +
  geom_raster(aes(x, y, fill = food), m_tbl) +
  geom_path(aes(longitude, latitude, group = paste(location, rep)), tracks, alpha = 0.4) +
  geom_point(aes(start_x, start_y, group = location),
             filter(tracks, ! duplicated(location)), shape = 4, color = 'white') +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH) +
  scale_color_brewer(type = 'qual', palette = 6, direction = -1) +
  scale_x_continuous('Resource abundance', breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous('Environmental variance', breaks = NULL, expand = c(0, 0))

ggsave('figures/proposal-defense/static-movement-50-tracks.png', plot = p_track,
       height = 6, width = 6)

a_track <-
  p_track +
  geom_point(aes(longitude, latitude, group = location), tracks) +
  transition_reveal(t)
anim_2 <- animate(a_track, duration = 10, start_pause = 7, end_pause = 15,  nframes = 100,
                  renderer = magick_renderer()) # to avoid annoying green lines/areas
anim_save('figures/proposal-defense/animated-movement-50-tracks.gif', animation = anim_2)

# calculate HRs ----
if(TRUE) { # import tracks with home ranges, or set to FALSE to estimate them
  tracks_hr <- readRDS('analysis/figures/proposal-defense/50-tracks-hrs.rds')
} else {
  tracks_hr <-
    tracks %>%
    ungroup() %>% # remove any remaining grouping
    nest(d = -c(location, rep)) %>%
    mutate(t_end = map_dbl(d, \(.d) max(.d$timestamp)),
           t_start = if_else(rep == 1, 0, lag(t_end) * 2),
           t_start = cumsum(t_start),
           t_start = t_start) %>%
    unnest(d) %>%
    mutate(t = t + t_start,
           timestamp = timestamp + t_start) %>%
    nest(tel = -c(location)) %>% # calculate HR for each location
    mutate(tel = imap(tel, \(.tel, i) { # convert the data to telemetry format
      .tel %>%
        mutate(individual.local.identifier = paste('location', location[i])) %>%
        as.telemetry(projection = PROJ)
    }),
    theta = imap(tel, \(d, i) { # fit a variogram to the data
      cat('Fitting variogram from location', location[i], '\n')
      ctmm.guess(data = d, interactive = FALSE)
    }))
  
  # fitting separate models for each location and replicate because return is a different
  # type of movement, and the models assume a constant state (same goes for time spent not
  # moving)
  tracks_hr <-
    mutate(tracks_hr,
           model = imap(tel, \(d, i) {
             cat('Fitting model from location', location[i],#, 'and rep', rep[i],
                 '\n')
             ctmm.fit(data = d, CTMM = theta[[i]])
           }))
  saveRDS(tracks_hr, 'analysis/figures/proposal-defense/50-tracks-ctmms.rds')
  
  # estimate separate HRs for each location and replicate
  tracks_hr <-
    tracks_hr %>%
    mutate(akde = akde(tel, model),
           hr_ratio = map_dbl(akde,
                              \(x) pull_parameter(.object = x, .parameter = 'area',
                                                  .quantile = 0.95, .value = 'est')),
           hr_ratio = hr_ratio / min(hr_ratio))
  
  saveRDS(tracks_hr, 'analysis/figures/proposal-defense/50-tracks-hrs.rds')
}

# calculate the average HR by location
hrs <-
  tracks_hr %>%
  mutate(`95%` = map(akde,
                     \(x) SpatialPolygonsDataFrame.UD(x, level.UD = 0.95, level=0.95) %>%
                       spTransform(CRS("+proj=longlat")) %>%
                       fortify()),
         `50%` = map(akde,
                     \(x) SpatialPolygonsDataFrame.UD(x, level.UD = 0.50, level=0.95) %>%
                       spTransform(CRS("+proj=longlat")) %>%
                       fortify())) %>%
  select(location, `95%`, `50%`) %>%
  pivot_longer(cols = c(`95%`, `50%`), values_to = 'akdes', names_to = 'q') %>%
  unnest(akdes) %>%
  filter(grepl('est', group))

# plot home ranges
ggplot() +
  coord_equal() +
  geom_polygon(aes(long, lat, color = q, fill = q, group = paste(location, group)), hrs) +
  theme(legend.position = 'right') +
  scale_color_manual('Quantile', values = c('darkorange', 'black')) +
  scale_fill_manual('Quantile', values = c('darkorange', 'transparent')) +
  theme_void()

# plot the tracks with the HRs
p_track_hr <-
  ggplot() +
  coord_equal() +
  geom_raster(aes(x, y, fill = food), m_tbl) +
  geom_polygon(aes(long, lat, group = paste(location, group), alpha = q), hrs,
               fill = 'dodgerblue', color = 'dodgerblue') +
  geom_path(aes(longitude, latitude, group = paste(location, rep)), tracks, alpha = 0.4) +
  geom_polygon(aes(long, lat, group = paste(location, group)), hrs, fill = NA,
               color = 'dodgerblue') +
  geom_point(aes(start_x, start_y, group = location),
             filter(tracks, ! duplicated(location)), shape = 4, color = 'white') +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH) +
  scale_alpha_manual(values = c(0.7, 0.3)) +
  scale_x_continuous('Resource abundance', breaks = NULL, expand = c(0, 0))+
  scale_y_continuous('Environmental variance', breaks = NULL, expand = c(0, 0))
ggsave('figures/proposal-defense/static-movement-hrs.png', plot = p_track_hr, height = 6,
       width = 6)
