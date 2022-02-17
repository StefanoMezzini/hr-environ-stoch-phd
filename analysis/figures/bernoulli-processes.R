library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('ctmm')      # for movement modeling
library('lubridate') # for working with data more smoothly
library('ggplot2')   # for fancy plots
library('cowplot')   # for fancy plots in grids
theme_set(theme_bw() + theme(legend.position = 'top', panel.grid = element_blank()))

# resource abundance palette
LOW <- '#744700'
MID <- '#d9bb94'
HIGH <- 'darkgreen'

# create simulated movement ----
set.seed(1) # for consistent results
m_ouf <- ctmm(tau = c(50, 10), sigma = 0.09, mu = c(0.5, 0.5), isotropic = TRUE)

# simulate data
sims <- simulate(m_ouf, t = seq(1, 500, by = 0.05)) %>%
  SpatialPoints.telemetry() %>%
  as.data.frame() %>%
  mutate(x = scales::rescale(x, to = c(1, 11), from = range(x)),
         y = scales::rescale(y, to = c(1, 11), from = range(y)))

ggplot(sims) +
  coord_equal() +
  geom_path(aes(x, y))

# changing mean, p = 1 ----
d_1 <- expand_grid(mu = 1:11,
                   patch = letters[1:11]) %>% # Var(Y) = p * (1 - p) = 1 * 0 = 0
  mutate(patch = factor(patch, levels = letters), # for "a" at the top of the y axis
         p = 1,
         y = rbinom(n = n(), size = 1, prob = p),
         a = mu * y,
         sigma2 = p * (1 - p))

# raster of abundance over time
p_1a <-
  ggplot(d_1, aes(mu, as.numeric(patch), fill = sigma2)) +
  geom_raster() +
  geom_point(color = 'white', size = 2) +
  geom_point(color = 'black', size = 1) +
  scale_x_continuous('Time', expand = c(0, 0), breaks = NULL) +
  scale_y_continuous('Patch', breaks = 1:11, labels = unique(d_1$patch),
                     expand = c(0, 0),
                     sec.axis = sec_axis(trans = identity,
                                         breaks = 1:11,
                                         labels = rep('1   ', 11),
                                         name = 'Probability of success')) +
  # can use latex2exp::TeX() to use LaTeX, but it doesn't support \mathbb or \mathcal
  scale_fill_viridis_c('Stochasticity', direction = -1, option = 'D', end = 1,
                       begin = 0.3, limits = c(0, 0.25), breaks = c(0, 0.125, 0.25),
                       labels = c('0', '0.125', '0.25')) +
  scale_alpha_continuous(range = c(0, 1))

# mean-variance relationship
p_1b <-
  ggplot() +
  geom_line(aes(0:10, 0), color = 'darkorange') +
  scale_x_continuous('Patch', breaks = 0:10, labels = letters[1:11],
                     sec.axis = sec_axis(identity, 'Probability of success',
                                         breaks = 0:10, labels = rep(1, 11))) +
  scale_y_continuous('Stochasticity', breaks = 0)

# raster with animal movement
p_1c <-
  ggplot() +
  coord_equal() +
  geom_raster(aes(mu, as.numeric(patch), fill = a), d_1) +
  geom_path(aes(x, y), sims, color = 'black') +
  scale_x_continuous('Mean resource abundance', expand = c(0, 0), breaks = NULL) +
  scale_y_continuous('Patch', expand = c(0, 0), breaks = unique(1:11),
                     labels = unique(d_1$patch),
                     sec.axis = sec_axis(trans = identity,
                                         breaks = 1:11,
                                         labels = rep('1   ', 11),
                                         name = 'Probability of success')) +
  scale_fill_gradient2('Resource availability', low = LOW, mid = MID,
                       high = HIGH, limits = c(0, 11), breaks = c(0, 11),
                       labels = c('Low', 'High'))

p_1 <- plot_grid(p_1a, plot_grid(p_1b, p_1c, labels = c('b.', 'c.')),
                 labels = c('a.', NA), ncol = 1); p_1

ggsave('figures/bernoulli-constant-p.png', plot = p_1, width = 8, height = 8, bg ='white')

# varying p ----
d_2 <-
  expand_grid(mu = 1:11, p = 0:10) %>%
  mutate(patch = letters[p + 1] %>% factor(levels = letters),
         p = p / 10,
         sigma2 = 1 * p * (1 - p),
         y = rbinom(n = n(), size = 1, prob = p),
         a = mu * y)

# raster of abundance over time
p_2a <-
  ggplot() +
  geom_raster(aes(mu, p, fill = sigma2), d_2) +
  geom_point(aes(mu, p), d_2, show.legend = FALSE, color = 'white', size = 2)+
  geom_point(aes(mu, p, alpha = y), d_2, show.legend = FALSE, color = 'black', size = 1)+
  scale_x_continuous('Time', expand = c(0, 0), breaks = NULL) +
  scale_y_continuous('Patch', breaks = unique(d_2$p), labels = unique(d_2$patch),
                     expand = c(0, 0),
                     sec.axis = sec_axis(trans = identity,
                                         breaks = unique(d_2$p),
                                         name = 'Probability of success')) +
  scale_fill_viridis_c('Stochasticity', direction = -1, option = 'D', end = 1,
                       begin = 0.3, limits = c(0, 0.25), breaks = c(0, 0.125, 0.25),
                       labels = c('0', '0.125', '0.25')) +
  scale_alpha_continuous(range = 0:1)

# mean-variance relationship
p_2b <-
  tibble(p = seq(0, 1, length.out = 400), sigma2 = 1 * p * (1 - p)) %>%
  ggplot() +
  geom_line(aes(p, sigma2), color = 'darkorange') +
  scale_x_continuous('Patch', sec.axis = sec_axis(identity, 'Probability of success'),
                     labels = letters[1:11], breaks = 0:10 / 10) +
  scale_y_continuous('Stochasticity')

# raster with animal movement
p_2c <-
  ggplot() +
  coord_equal() +
  geom_raster(aes(mu, as.numeric(patch), fill = a), d_2) +
  geom_path(aes(x, y), sims, color = 'black') +
  scale_x_continuous('Mean resource abundance', expand = c(0, 0), breaks = NULL) +
  scale_y_continuous('Patch', breaks = unique(d_2$p * 10 + 1), labels = unique(d_2$patch),
                     expand = c(0, 0),
                     sec.axis = sec_axis(trans = identity,
                                         breaks = unique(d_2$p * 11),
                                         labels = unique(d_2$p),
                                         name = 'Probability of success')) +
  scale_fill_gradient2('Resource availability', low = LOW, mid = MID,
                       high = HIGH, limits = range(d_2$a), breaks = range(d_2$a),
                       labels = c('Low', 'High')); p_2c

p_2 <- plot_grid(p_2a, plot_grid(p_2b, p_2c, labels = c('b.', 'c.')),
                 labels = c('a.', NA), ncol = 1); p_2

ggsave('figures/bernoulli-changing-p.png', plot = p_2, width = 8, height = 8, bg ='white')
