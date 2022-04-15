library('ctmm')    # for movement modeling
library('purrr')   # for functional programming (e.g., map*())
library('tidyr')   # for data wrangling (e.g., unnest())
library('ggplot2') # for plotting
library('cowplot') # for plots in grids
source('functions/energetics-functions.R') # movement & costs based on animal mass
source('functions/truncate_telemetry.R') # to cut telemetry once animal is "full"
source('functions/pull_parameter.R') # to extract parameters from ctmm and akde objects
source('analysis/default-figure-styling.R') # defaults for figures (theme, size, ...)
theme_set(theme_map() + theme(legend.position = 'none'))

NCORES <- 7 # number of cores to use to fit model in parallel
REQUIRED <- 15 # amount of food required for satiety
DELTA_T <- 0.1 # time between measurements
SAMPLES <- seq(0, 15, by = DELTA_T) # sampling times
DIM <- 15 # dimensions of raster (number of cells per side)
HABITAT <- matrix(data = 1, nrow = DIM, ncol = DIM) %>% # raster of patches
  raster(xmx = 10, xmn = -10, ymx = 10, ymn = -10)
HABITAT_tbl <- rasterToPoints(HABITAT) %>% as_tibble() # raster in tibble format

m <- ctmm(tau = c(Inf, 1), sigma = 1, mu = c(0, 0)) # infinitely diffusive model
tracks <-
  tibble(day = 0:4, # 7, 8, 9 give very linear movement
         tel = map(day, \(i) {
           simulate(m, # ctmm movement model
                    t = SAMPLES, # sampling times in seconds
                    seed = i, # consistent but different track each day
                    complete = TRUE) # add lat, long, and timestamp to track
         })) %>%
  mutate(labelled = map(tel,\(x) label_visits(tel = x, habitat = HABITAT)),
         truncated = map(labelled, \(x) x %>%
                           # animal eats whenever it crosses to a new cell
                           mutate(food = rgamma(n = n(), shape = 2, rate = 1),
                                  food = if_else(new_cell, food, 0),
                                  satiety = cumsum(food),
                                  full = satiety > REQUIRED) %>%
                           # only keep "not full" and first "full"
                           filter((! full) | (! duplicated(full))) %>%
                           mutate(t_full = t[which(full & ! duplicated(full))])),
         endpoints = map(truncated, \(x) filter(x, t == min(t) | t == max(t))))

# ensure the animal got full each day
transmute(tracks, day, full = map_lgl(truncated, \(x) any(x$full, na.rm = TRUE)))

loops <-
  tracks %>%
  mutate(track = map2(truncated, endpoints, \(expl, ends) { # add the return to (0, 0)
    simulate(object = m,
             data = ends, # endpoints: (0, 0) and position at time of satiety
             t = seq(0, max(ends$t), by = DELTA_T)) %>% # return takes as long as expl.
      data.frame() %>% # convert for data wrangling
      tibble() %>% # convert for ease of use
      mutate(t = rev(t) + max(ends$t), path = 'return') %>% # return home, not exploration
      bind_rows(mutate(expl, path = 'exploration')) %>% # add exploration track
      arrange(t) %>% # arrange by time (exploration comes before return)
      return()
  })) %>%
  mutate(t_end = map_dbl(track, \(x) max(x$t)), # end time is the latest time point
         t_end = cumsum(t_end), # end times occur sequentially after previous ones
         t_start = lag(t_end), # each day starts at the end of the previous day
         t_start = if_else(is.na(t_start), 0, t_start)) %>% # should be 0, not NA
  unnest(track) %>% # ungroup the data for each day
  mutate(t = t + t_start) %>% # change `t` so days occur consecutively
  # change timestamp to reflect the new t, add a dummy identifier for the animal
  mutate(timestamp = as.POSIXct(t, tz='UTC', origin = as.POSIXct('1970-01-01 00:00 UTC')),
         individual.local.identifier = 'example_animal') %>%
  dplyr::select(-c(tel, labelled, truncated, endpoints)) %>%
  relocate(c(t_start, t, t_end, path), .after = day) # time columns together

# animal leaves to explore (pink), collects food (dots), and returns (cyan)
ggplot(mapping = aes(x, y, color = path)) +
  coord_equal() +
  geom_path(data = filter(loops, day == 1)) +
  geom_point(data = filter(loops, day == 1, new_cell), size = 2)

ggplot(loops) +
  coord_equal() +
  geom_path(aes(x, y, color = factor(day))) +
  theme(legend.position = 'right')

# estimate home range
tel <- as.telemetry(loops) # change data to telemetry structure
theta <- ctmm.guess(data = tel, interactive = FALSE) # variogram
model <- ctmm.fit(tel, theta, control = list(cores = NCORES)) # fit movement model (OUF)
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
  ggplot(filter(tracks, day == 1)$tel[[1]], aes(x, y)) +
  coord_equal() +
  geom_tile(data = HABITAT_tbl, fill = 'transparent', color = '#00000030') +
  geom_path() +
  geom_point(aes(0, 0), pch = 18, size = 4) +
  geom_point(aes(0, 0), pch = 18, size = 2, color = 'yellow'); p_a

# b) encounters (green dots)
p_b <-
  p_a +
  geom_point(data = filter(tracks, day == 1)$labelled[[1]] %>% filter(new_cell),
             size = 2, alpha = 0.3) +
  geom_point(aes(0, 0), pch = 18, size = 4) +
  geom_point(aes(0, 0), pch = 18, size = 2, color = 'yellow'); p_b

# c) satiety (colored line) and truncate_telemetry() (truncated grey)
p_c <-
  ggplot(filter(loops, day == 1, path == 'exploration'), aes(x, y)) +
  coord_equal() +
  geom_tile(data = HABITAT_tbl, fill = 'transparent', color = '#00000030') +
  geom_path(data = filter(tracks, day ==1 )$labelled[[1]], color = 'grey') +
  geom_path(aes(color = satiety)) +
  geom_point(aes(color = satiety), filter(loops, day == 1, new_cell), size = 2) +
  geom_point(aes(0, 0), pch = 18, size = 4) +
  geom_point(aes(0, 0), pch = 18, size = 2, color = 'yellow') +
  scale_color_gradient('Satiety', labels = NULL, breaks = c(0, REQUIRED), low = 'brown4',
                       high = 'forestgreen', na.value = 'transparent'); p_c

# d) return home to (0, 0)
p_d <-
  ggplot(mapping = aes(x, y)) +
  coord_equal() +
  geom_tile(data = HABITAT_tbl, fill = 'transparent', color = '#00000030') +
  geom_path(aes(color = satiety),
            filter(loops, day == 1, path == 'exploration')) +
  geom_path(data = filter(loops, day == 1, path == 'return')) +
  geom_point(aes(color = satiety), filter(loops, day == 1, new_cell), size = 2) +
  geom_point(aes(0, 0), pch = 18, size = 4) +
  geom_point(aes(0, 0), pch = 18, size = 2, color = 'yellow') +
  scale_color_gradient('Satiety', labels = NULL, breaks = c(0, REQUIRED), low = 'brown4',
                       high = 'forestgreen', na.value = 'transparent'); p_d

# e) repeat with other paths
p_e <-
  ggplot(loops, aes(x, y)) +
  coord_equal() +
  geom_tile(data = HABITAT_tbl, fill = 'transparent', color = '#00000030') +
  geom_path(aes(group = day)) +
  geom_path(aes(color = satiety, group = day)) +
  geom_point(aes(0, 0), pch = 18, size = 4) +
  geom_point(aes(0, 0), pch = 18, size = 2, color = 'yellow') +
  scale_color_gradient('Satiety', labels = NULL, breaks = c(0, REQUIRED), low = 'brown4',
                       high = 'forestgreen', na.value = 'transparent'); p_e

# f) HR estimate
p_f <-
  ggplot(loops, aes(x, y)) +
  coord_equal() +
  geom_tile(data = HABITAT_tbl, fill = 'transparent', color = '#00000030') +
  geom_polygon(aes(long, lat, group = group, lty = group), akde_95, alpha = 0.2,
               color = 'black', show.legend = FALSE, fill = 'grey') +
  geom_path(aes(group = day)) +
  geom_path(aes(color = satiety, group = day)) +
  geom_point(aes(0, 0), pch = 18, size = 4) +
  geom_point(aes(0, 0), pch = 18, size = 2, color = 'yellow') +
  scale_color_gradient('Satiety', labels = NULL, low = '#744700', high = 'forestgreen',
                       na.value = 'transparent') +
  scale_linetype_manual(values = c(2, 1, 2)); p_f

plot_grid(p_a, p_b, p_c, p_d, p_e, p_f, nrow = 2, labels = paste0(letters, '.')) %>%
  plot_grid(get_legend(p_f +
                         scale_color_gradient('Satiety', limits = 0:1, breaks = 0:1,
                                              labels = c('Empty', 'Full'),
                                              low = '#744700', high = 'forestgreen') +
                         theme(legend.position = 'right')), rel_widths = c(10, 1))

ggsave('figures/simulation-example.png', width = 8, height = 3, bg = 'white', scale = 1.5)
