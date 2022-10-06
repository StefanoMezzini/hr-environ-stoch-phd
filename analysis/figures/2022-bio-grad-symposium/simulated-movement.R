library('dplyr')        # for data wrangling
library('ctmm')         # for continuous-time movement modeling
library('ggplot2')      # for fancy plots
library('gganimate')    # for animated fancy plots

theme_set(theme_void()) # set default ggplot theme

# change default gif parameters
options(gganimate.dev_args = list(width = 4, height = 4, units = 'in',
                                  res = 300))

# create simulated movement ----
m_ouf <- ctmm(tau = c(10, 1), sigma = 0.09, mu = c(0.5, 0.5), isotropic = TRUE)
sims_tel <- simulate(m_ouf, t = seq(1, 1000, by = 0.1), seed = 1)
sims_df <- sims_tel %>%
  SpatialPoints.telemetry() %>%
  as.data.frame() %>%
  rename(long = x, lat = y) %>%
  mutate(t = Sys.Date() + 1:n(),
         id = 1)

# static plot
ggplot(sims_df[1:MAX, ], aes(long, lat)) +
  coord_equal() +
  geom_path() +
  scale_y_reverse() # reverse y axis for aesthetics

# animate only part of the track
NFRAMES <- round(nrow(sims_df)/4)
FPS <- NFRAMES / 30
p_anim <-
  ggplot(sims_df[1:MAX, ], aes(long, lat)) +
  coord_equal() +
  geom_path() +
  scale_y_reverse() +
  transition_reveal(t)

anim <- animate(p_anim, start_pause = 0, end_pause = 0, nframes = NFRAMES,
                fps = FPS)

anim_save('figures/2022-bio-grad-symposium/animated-movement.gif', anim)
