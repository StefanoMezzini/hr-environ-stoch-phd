library('ctmm')    # for movement modeling
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('ggplot2') # for fancy plots
library('cowplot') # for fancy multi-panel plots
source('analysis/figures/default-figure-styling.R') # for common theme and color palette

theme_set(theme_get() + theme(legend.position = 'none'))

hr_lab <- '\U1D54D(position)' # label for y axis

# simulate animal movement ----
p_track <-
  simulate(ctmm(tau = c(Inf, 1), sigma = 1), t = seq(1, 400, by = 0.05), seed = 5) %>%
  SpatialPoints.telemetry() %>%
  data.frame() %>%
  ggplot(aes(x, y)) +
  coord_equal() +
  geom_path() +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(plot.background = element_blank())

# hypothesis for change in H over E(R) ----
d <- tibble(mu = seq(0, 1, length.out = 400),
            sigma2 = rev(mu),
            split = mu < 0.35,
            h_1 = if_else(mu < 0.2, 2, sinpi(0.5 - (mu - 0.2) / 2)^10 * 2) + 0.2,
            h_2 = if_else(split, 2 * mu^2 - 7 * mu + 3.916873, h_1),
            h_3 = if_else(split, h_2 + 30 * (0.35 - mu)^2, h_2))

p_mu <-
  ggplot(d, aes(mu)) +
  coord_cartesian(ylim = c(0, 4)) +
  geom_ribbon(aes(ymin = h_1, ymax = h_3), alpha = 0.3) +
  geom_line(aes(y = h_1)) +
  geom_line(aes(y = h_3)) +
  labs(x = '\U1D53C(\U1D445)', y = hr_lab) +
  scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous(breaks = 0)

# hypothesis for change in H over V(R) ----
# vary color of lines by certainty ([0, 1]), with 1 being black and 0 being black w 0% alpha
p_s2 <-
  ggplot(d, aes(sigma2)) +
  coord_cartesian(ylim = c(0, 4)) +
  geom_ribbon(aes(ymin = h_1, ymax = h_3), alpha = 0.3) +
  geom_line(aes(y = h_1)) +
  geom_line(aes(y = h_3)) +
  labs(x = '\U1D54D(\U1D445)', y = hr_lab) +
  scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous(breaks = 0)

# add the animal tracks ---
p_e <-
  ggdraw(p_mu) +
  draw_plot(p_track, x = 1, y = 1.0335, width = 0.45, height = 0.45, hjust = 1, vjust = 1)

p_v <-
  ggdraw(p_s2) +
  draw_plot(p_track, x = 0.096, y = 1.0335, width = 0.45, height = 0.45, hjust = 0,
            vjust = 1)

# save the plots
ggsave('figures/mean-abundance-hr-hypotheses.png', plot = p_mu, width = 4, height = 4,
       dpi = 600, bg = 'white')
ggsave('figures/variance-abundance-hr-hypotheses.png', plot = p_s2, width = 4, height = 4,
       dpi = 600, bg = 'white')
