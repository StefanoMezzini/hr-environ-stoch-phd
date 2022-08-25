library('ctmm')    # for movement modeling
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('ggplot2') # for fancy plots
library('ggbrace') # for curly braces on fancy plots
library('cowplot') # for fancy multi-panel plots
source('analysis/figures/default-figure-styling.R') # for common theme and color palette

theme_set(theme_get() + theme(legend.position = 'none'))

hr_lab <- '\U1D54D(position)' # label for y axis

# hypothesis for change in H over E(R) ----
d <- tibble(mu = seq(0, 1, length.out = 400),
            sigma2 = rev(mu),
            split = mu < 0.35,
            h_1 = if_else(mu < 0.2, 2, sinpi(0.5 - (mu - 0.2) / 2)^10 * 2) + 0.2,
            h_2 = if_else(split, 2 * mu^2 - 7 * mu + 3.916873, h_1),
            h_3 = if_else(split, h_2 + 30 * (0.35 - mu)^2, h_2))

notes <- tibble(x = c(0.13, 0.36, 0.175, 0.68),
                y = c(2.05, 3.2, 1.35, 2.05),
                text = c('range-resident', 'nomadic or dispersing',
                         'evolutionary timescale', 'ecological timescale'))

p_mu <-
  ggplot() +
  coord_cartesian(ylim = c(0, 4)) +
  geom_ribbon(aes(mu, ymin = h_1, ymax = h_3), d, color = 'black', alpha = 0.2) +
  geom_brace(aes(x = c(0, 0.345), y = c(1.45, 1.7)), rotate = 180) +
  geom_brace(aes(x = c(0.36, 1), y = c(1.95, 1.7)), rotate = 0) +
  geom_text(aes(x = x, y = y, label = text), notes) +
  scale_x_continuous('\U1D53C(\U1D445)', breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous(hr_lab, breaks = 0); p_mu

# hypothesis for change in H over V(R) ----
p_s2 <-
  p_mu +
  scale_x_reverse('\U1D54D(\U1D445)', breaks = NULL, expand = c(0, 0)); p_s2

# save the plots ----
ggsave('figures/mean-abundance-hr-hypotheses.png', plot = p_mu, width = 4, height = 4,
       scale = 1.5, dpi = 400, bg = 'white')
ggsave('figures/variance-abundance-hr-hypotheses.png', plot = p_s2, width = 4, height = 4,
       scale = 1.5, dpi = 400, bg = 'white')

# add simulated animal movement ----
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

p_e <-
  ggdraw(p_mu) +
  draw_plot(p_track, x = 1, y = 1.0335, width = 0.45, height = 0.45, hjust = 1, vjust = 1)

p_v <-
  ggdraw(p_s2) +
  draw_plot(p_track, x = 0.096, y = 1.0335, width = 0.45, height = 0.45, hjust = 0,
            vjust = 1)
