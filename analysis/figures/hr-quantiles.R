library('dplyr')   # for data wrangling (%>%, mutate())
library('tidyr')   # for data wrangling (unnest())
library('ctmm')    # for movement modeling
library('purrr')   # for functional programming (map_***())
library('ggplot2') # for fancy plots
library('cowplot') # for fancy plots in grids
theme_set(theme_bw())

# create simulated movement ----
set.seed(1) # for consistent results
m_ouf <- ctmm(tau = c(10, 9), sigma = 0.09, mu = c(0.5, 0.5), isotropic = TRUE)
sims.tel <- simulate(m_ouf, t = seq(1, 1e4, by = 0.05))
sims.df <- sims.tel %>%
  SpatialPoints.telemetry() %>%
  as.data.frame() %>%
  rename(long = x, lat = y) %>%
  mutate(t = Sys.Date() + 1:n(),
         id = 1)

# if the model hasn't been fit and saved yet, then do so, otherwise import it instead
if(! file.exists('models/hr-quantiles-akde.rds')) {
  m <- ctmm.fit(sims.tel) # only used to fit the AKDE
  hr_akde <- akde(sims.tel, m)
  saveRDS(hr_akde, 'models/hr-quantiles-akde.rds')
} else {
  hr_akde <- readRDS('models/hr-quantiles-akde.rds')
}

# movement
p_mov <-
  ggplot(sims.df, aes(long, lat)) +
  coord_equal() +
  geom_path() +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL)

hr <-
  tibble(q = seq(0, 0.999, length.out = 1e4),
         sig = 0.09, # movement variance in m_ouf()
         # H = map_dbl(q, .f = \(x) summary(hr_akde, level.UD = x, units = FALSE)$CI[2]),
         H_true = -2 * log(1 - q) * pi * sig)

# create tibbles of core and 95% HRs
hrs <- bind_rows(SpatialPolygonsDataFrame.UD(hr_akde, level.UD = 0.95) %>%
                   fortify() %>%
                   mutate(q = 0.95),
                 SpatialPolygonsDataFrame.UD(hr_akde, level.UD = 0.75) %>%
                   fortify() %>%
                   mutate(q = 0.75),
                 SpatialPolygonsDataFrame.UD(hr_akde, level.UD = 0.5) %>%
                   fortify() %>%
                   mutate(q = 0.5)) %>%
  as_tibble() %>%
  # remove 95% CIs
  filter(! grepl('low', id) & ! grepl('high', id))

# movement with core HR and 95% HR
p_hrs <-
  p_mov +
  geom_polygon(aes(group = group, color = q, fill = q),
               data = hrs, lwd = 1, alpha = 0.4) +
  scale_color_viridis_c('Quantile', option = 'A', aesthetics = c('color', 'fill'),
                        limits = c(0, 1)) +
  theme(legend.position = 'right')

# estimated area
p_area <-
  ggplot(hr, aes(q, H_true)) +
  # geom_area(fill = 'black', color = 'black', alpha = 0.3) +
  geom_segment(aes(xend = q, yend = 0, group = q, color = q)) +
  xlab('Quantile') +
  scale_y_continuous(expression(Home~range~size), breaks = NULL) +
  scale_color_viridis_c('Quantile', option = 'A', limits = c(0, 1)) +
  theme(panel.grid = element_blank(), legend.position = 'none')

p <-
  ggdraw(p_area) +
  draw_plot(p_hrs, x = 0.05, y = 0.3, width = 0.75, height = 0.75)
ggsave('figures/hr-quantiles.png', p, width = 3, height = 3, scale = 2, bg = 'white')
