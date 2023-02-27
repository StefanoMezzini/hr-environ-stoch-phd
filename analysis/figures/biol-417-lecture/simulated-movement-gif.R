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
sims.tel <- simulate(m_ouf, t = seq(1, 1e4, by = 1))
sims_df <- sims.tel %>%
  SpatialPoints.telemetry() %>%
  as.data.frame() %>%
  rename(long = x, lat = y) %>%
  mutate(t = Sys.Date() + 1:n(),
         id = 1)
m <- ctmm.fit(sims.tel)
hr <- akde(sims.tel, m) %>%
  SpatialPolygonsDataFrame.UD() %>%
  fortify()

# figure of movement alone
p <-
  ggplot(sims_df, aes(long, lat)) +
  coord_equal() +
  geom_path(); p
ggsave(filename = 'figures/biol-417-lecture/static-movement.png', height = 8,
       width = 8, bg = 'white')

# animate the movement data
sims_df_short <- slice(sims_df, 1:1e3)
p_anim <-
  ggplot(sims_df_short, aes(long, lat, group = 1)) +
  coord_equal() +
  geom_path() +
  transition_reveal(sims_df_short$t)
anim <- animate(p_anim, duration = 25, start_pause = 5, end_pause = 25,
                nframes = nrow(sims_df_short) + 30)
anim_save(filename = 'figures/biol-417-lecture/animated-movement.gif', anim)

# static movement with 95% AKDE
p_hr <-
  p +
  geom_polygon(aes(group = group), filter(hr, grepl('est', id)),
               linewidth = 1, color = '#70AD47', fill = '#70AD47', alpha = 0.3); p_hr
ggsave('figures/biol-417-lecture/movement-hr.png',
       p_hr, width = 8, height = 8, bg = 'white')
