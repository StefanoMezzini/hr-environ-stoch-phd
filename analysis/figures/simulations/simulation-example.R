library('ctmm')    # for movement modeling
library('dplyr')   # for data wrangling (%>%, mutate())
library('purrr')   # for functional programming (e.g., map*())
library('tidyr')   # for data wrangling (e.g., unnest())
library('ggplot2') # for plotting
library('cowplot') # for plots in grids
source('functions/label_visits.R') # label food encounters based on a raster
source('functions/pull_parameter.R') # to extract parameters from ctmm and akde objects
source('analysis/default-figure-styling.R') # defaults for figures (theme, size, ...)
theme_set(theme_map() + theme(legend.position = 'none'))
select <- dplyr::select # don't use raster::select as a default

NCORES <- 7 # number of cores to use to fit model in parallel
REQUIRED <- 12 # amount of food required for satiety
DELTA_T <- 0.1 # time between measurements
SAMPLES <- seq(0, 15, by = DELTA_T) # sampling times
DIM <- 15 # dimensions of raster (number of cells per side)
HABITAT <- matrix(data = 1, nrow = DIM, ncol = DIM) %>% # raster of patches
  raster(xmx = 10, xmn = -10, ymx = 10, ymn = -10)
HABITAT_tbl <- rasterToPoints(HABITAT) %>% as_tibble() # raster in tibble format

# resource abundance palette
LOW <- '#744700'
MID <- '#d9bb94'
HIGH <- 'darkgreen'

m <- ctmm(tau = c(Inf, 1), sigma = 1, mu = c(0, 0)) # infinitely diffusive model
tracks <-
  tibble(day = 0:4, # 7, 8, 9 give very linear movement
         tel = map(day, \(i) {
           simulate(m, # ctmm movement model
                    t = SAMPLES, # sampling times in seconds
                    seed = i, # consistent but different track each day
                    complete = TRUE) # add lat, long, and timestamp to track
         })) %>%
  mutate(labelled = map(tel,\(x) label_visits(.tel = x, .habitat = HABITAT)),
         truncated = map(labelled, \(x) x %>%
                           # animal eats whenever it crosses to a new cell
                           mutate(food = rgamma(n = n(), shape = 2, rate = 1),
                                  food = if_else(new_cell, food, 0),
                                  satiety = cumsum(food),
                                  full = satiety > REQUIRED) %>%
                           # only keep "not full" and first "full"
                           filter((! full) | (! duplicated(full))) %>%
                           mutate(t_full = t[which(full & ! duplicated(full))])),
         t_max = map_dbl(truncated, \(x) max(x$t)),
         t_start = lag(t_max * 2), # allow time for return
         t_start = if_else(is.na(t_start), 0, t_start))

# ensure the animal got full each day
transmute(tracks, day, full = map_lgl(truncated, \(x) any(x$full, na.rm = TRUE)))

# dataset of full tracks not truncated
labelled <- tracks %>%
  select(day, labelled) %>%
  unnest(labelled)

tracks <-
  tracks %>%
  select(- tel, - labelled) %>%
  mutate(t_start = cumsum(t_start)) %>%
  # mutate(truncated = map2(truncated, t_start, \(.tel, t0) { # make tracks sequential
  #   .tel$t <- .tel$t + t0
  #   return(.tel)
  # })) %>%
  unnest(truncated) %>% # ungroup the data for each day
  mutate(t = t + t_start) %>% # change `t` so days occur consecutively
  # change timestamp to reflect the new t, add a dummy identifier for the animal
  mutate(timestamp = as.POSIXct(t, tz='UTC', origin = as.POSIXct('1970-01-01 00:00 UTC')),
         individual.local.identifier = 'example_animal') %>%
  relocate(c(t_start, t), .after = day) # time columns together

# ensure time occurs sequentially
ggplot(tracks, aes(timestamp, t, color = factor(day))) +
  geom_line() +
  scale_color_brewer(type = 'qual', palette = 6) +
  theme_bw()

# animal leaves to explore each day and eat at points (starts at square)
ggplot(mapping = aes(x, y)) +
  coord_equal() +
  geom_path(data = filter(tracks, day == 1)) +
  geom_point(data = filter(tracks, day == 1, new_cell), size = 2) +
  geom_point(aes(0, 0), pch = 18, size = 4) +
  geom_point(aes(0, 0), pch = 18, size = 2, color = 'yellow')

# plot all tracks
ggplot(tracks) +
  coord_equal() +
  geom_path(aes(x, y, color = factor(day))) +
  geom_point(aes(x, y, color = factor(day))) +
  scale_color_brewer(type = 'qual', palette = 6) +
  theme(legend.position = 'right')

# estimate home range
tel <- as.telemetry(tracks) # change data to telemetry structure
theta <- ctmm.guess(data = tel, interactive = FALSE) # variogram
model <- ctmm.fit(tel, theta) # fit movement model (OUF)
AKDE <- akde(tel, model) # HR autocorrelated kernel density estimate

# 95% quantile autocorrelated kernel density estimates
akde_95 <-
  SpatialPolygonsDataFrame.UD(AKDE, level.UD = 0.95) %>%
  fortify() %>%
  as_tibble()

# create the figure ----
HABITAT_tbl <-
  rasterToPoints(HABITAT) %>% # convert raster to a data.frame
  as_tibble() %>% # convert to a tibble for ease of use
  filter(x >= min(akde_95$long) - 1, x <= max(akde_95$long) + 2,
         y >= min(akde_95$lat) - 1, y <= max(akde_95$lat) + 1)

# a) movement from the first "day"
p_a <-
  ggplot(filter(labelled, day == 1), aes(x, y)) +
  coord_equal() +
  geom_tile(data = HABITAT_tbl, fill = 'transparent', color = '#00000030') +
  geom_path() +
  geom_point(aes(0, 0), pch = 18, size = 4) +
  geom_point(aes(0, 0), pch = 18, size = 2, color = 'yellow'); p_a

# b) encounters (green dots)
p_b <-
  p_a +
  geom_point(data = filter(labelled, day == 1) %>% filter(new_cell), size = 2,
             alpha = 0.3) +
  geom_point(aes(0, 0), pch = 18, size = 4) +
  geom_point(aes(0, 0), pch = 18, size = 2, color = 'yellow'); p_b

# c) satiety (colored line) and unused portion (grey)
p_c <-
  ggplot(filter(tracks, day == 1), aes(x, y)) +
  coord_equal() +
  geom_tile(data = HABITAT_tbl, fill = 'transparent', color = '#00000030') +
  geom_point(data = filter(labelled, day == 1) %>% filter(new_cell), size = 2,
             alpha = 0.3) +
  geom_path(data = filter(labelled, day == 1), alpha = 0.3) +
  geom_path(aes(color = satiety, group = day)) +
  geom_point(aes(0, 0), pch = 18, size = 4) +
  geom_point(aes(0, 0), pch = 18, size = 2, color = 'yellow') +
  scale_color_gradient2('Satiety', low = LOW, mid = MID, high = HIGH,
                        limits = c(0, REQUIRED), breaks = c(0, REQUIRED),
                        labels = c('Low', 'High'),
                        midpoint = mean(0, REQUIRED)); p_c

# d) all tracks with satiety
p_d <-
  ggplot(tracks, aes(x, y)) +
  coord_equal() +
  geom_tile(data = HABITAT_tbl, fill = 'transparent', color = '#00000030') +
  geom_path(aes(color = satiety, group = day)) +
  geom_point(aes(0, 0), pch = 18, size = 4) +
  geom_point(aes(0, 0), pch = 18, size = 2, color = 'yellow') +
  scale_color_gradient2('Satiety', low = LOW, mid = MID, high = HIGH,
                        limits = c(0, REQUIRED), breaks = c(0, REQUIRED),
                        labels = c('Low', 'High'),
                        midpoint = mean(0, REQUIRED)); p_d

# e) HR estimate
p_e <-
  ggplot(tracks, aes(x, y)) +
  coord_equal() +
  geom_tile(data = HABITAT_tbl, fill = 'transparent', color = '#00000030') +
  geom_polygon(aes(long, lat, group = group, lty = group), akde_95, alpha = 0.2,
               color = 'black', show.legend = FALSE, fill = 'grey') +
  geom_path(aes(group = day)) +
  geom_path(aes(color = satiety, group = day)) +
  geom_point(aes(0, 0), pch = 18, size = 4) +
  geom_point(aes(0, 0), pch = 18, size = 2, color = 'yellow') +
  scale_color_gradient2('Satiety', low = LOW, mid = MID, high = HIGH,
                        limits = c(0, REQUIRED), breaks = c(0, REQUIRED),
                        labels = c('Low', 'High'),
                        midpoint = mean(0, REQUIRED)) +
  scale_linetype_manual(values = c(2, 1, 2)); p_e

plot_grid(get_legend(p_e +
                       scale_color_gradient2('Satiety', low = LOW, mid = MID, high = HIGH,
                                             limits = 0:1, breaks = 0:1,
                                             labels = c('Empty', 'Full'),
                                             midpoint = mean(0, REQUIRED)) +
                       theme_bw() +
                       theme(legend.position = 'top')),
          plot_grid(p_a, p_b, p_c, p_d, p_e, nrow = 1, labels = paste0(letters, '.')),
          ncol = 1, hjust = 0.5, rel_heights = c(1, 5))

ggsave('figures/simulations/simulation-example.png', width = 10, height = 2, bg = 'white',
       scale = 1.5)
