library('dplyr')        # for data wrangling
library('tidyr')        # for data wrangling
library('sp')           # for spatial data
library('adehabitatHR') # for minimum convex polygon HR estimation 
library('ctmm')         # for continuous-time movement modeling
library('ggplot2')      # for fancy plots
library('gganimate')    # for animated fancy plots

theme_set(theme_void()) # set default ggplot theme

# change default gif parameters
options(gganimate.dev_args = list(width = 8, height = 8, units = 'in', res = 300))

# create simulated movement ----
set.seed(1) # for consistent results
m_ouf <- ctmm(tau = c(100, 10), sigma = 0.09, mu = c(0.5, 0.5), isotropic = TRUE)
sims.tel <- simulate(m_ouf, t = seq(1, 500, by = 0.05))
sims.df <- sims.tel %>%
  SpatialPoints.telemetry() %>%
  as.data.frame() %>%
  rename(long = x, lat = y) %>%
  mutate(t = Sys.Date() + 1:n(),
         id = 1)
m <- ctmm.fit(sims.tel)
hr <- akde(sims.tel, m) %>%
  SpatialPolygonsDataFrame.UD(proj4string = '+proj=longlat') %>%
  fortify()

# figure of movement alone
p <-
  ggplot(mapping = aes(long, lat)) +
  coord_equal() +
  geom_path(data = sims.df); p

# animate the movement data
p_anim <- p + transition_reveal(sims.df$t)
anim <- animate(p_anim, duration = 10, start_pause = 1, end_pause = 15)
anim_save(filename = 'figures/biol-417-lecture/animated-movement.gif', anim)

# static movement with 95% AKDE
p_hr <-
  p +
  geom_polygon(aes(group = group), filter(hr, id == ' 95% est'),
               size = 1, color = 'red', fill = 'red', alpha = 0.3); p_hr
ggsave('figures/biol-417-lecture/movement-hr.png',
       p_hr, width = 8, height = 8, bg = 'white')
