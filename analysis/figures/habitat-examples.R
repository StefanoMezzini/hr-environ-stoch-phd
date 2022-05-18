library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('purrr')     # for functional programming
library('ctmm')      # for movement modeling
library('lubridate') # for working with data more smoothly
library('ggplot2')   # for fancy plots
library('latex2exp') # for LaTeX in plots
library('cowplot')   # for fancy plots in grids
library('ragg')    # needed for custom alpha with coord_cartesian
# see https://github.com/tidyverse/ggplot2/issues/4029
source('functions/rgamma2.R') # function to generate rgamma() from mean and variance
source('functions/qgamma2.R') # function to generate qgamma() from mean and variance
theme_set(theme_bw() + theme(legend.position = 'top', panel.grid = element_blank()))

# color-blind-friendly color palette
pal <- c('#ff8c00', '#4477AA', '#009900', '#66CCEE',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

# resource abundance palette
LOW <- '#744700'
MID <- '#d9bb94'
HIGH <- 'darkgreen'

# create simulated movement ----
m_ouf <- ctmm(tau = c(50, 10), sigma = 0.1, mu = c(0.50, 0.50), isotropic = TRUE)

# simulate data
set.seed(1) # for consistent results
sims <- simulate(m_ouf, t = seq(1, 500, by = 0.05)) %>%
  SpatialPoints.telemetry() %>%
  as.data.frame() %>%
  as_tibble() %>%
  # rescale to fit (0, 1)
  mutate(x = scales::rescale(x, to = c(0.025, 0.975), from = range(x)),
         y = scales::rescale(y, to = c(0.025, 0.975), from = range(y)))

ggplot(sims) +
  coord_equal() +
  geom_path(aes(x, y)) +
  scale_x_continuous(limits = 0:1) +
  scale_y_continuous(limits = 0:1)

# variable definition ----
# functions for trends in mean and variance
mean_fcn <- function(.t) {
  sinpi(.t / 4.5 - 0.75) * 20 + 35
}

var_fcn <- function(.t) {
  (sinpi(.t / 4.5 - 0.75) * 15 + 17.5)
}

params <- tibble(t = seq(1, 10, length.out = 250),
                 mu = mean_fcn(t),
                 sigma2 = var_fcn(t),
                 sigma2_constant = mean(sigma2))

lim_food <- c(0,
              qgamma2(p = 0.01, mu = max(params$mu), max(params$sigma2)) %>%
                ceiling())

lim_sigma2 <- c(floor(min(params$sigma2)), ceiling(max(params$sigma2)))

# changing mean, constant variance ----
# samples
d_1 <- expand_grid(t = 1:10,
                   patch = letters[t]) %>%
  mutate(mu = mean_fcn(t),
         sigma2 = mean(var_fcn(t)), # constant values
         food = rgamma2(mu = mu, sigma2 = sigma2, N = n()),
         e = food - mu)

# probability density of R
r_density <- tibble(r = seq(0, 10, length.out = 400),
                    dens = dgamma(r, shape = 3, scale = 1))

# a. raster of abundance with events
p_1a <-
  ggplot(d_1, aes(t, patch)) +
  geom_raster(aes(fill = food)) +
  geom_point(color = 'black', size = 3) +
  geom_point(aes(color = sigma2), size = 2) +
  scale_x_continuous('Time', expand = c(0, 0), breaks = NULL) +
  scale_y_discrete('Patch', expand = c(0, 0)) +
  # can use latex2exp::TeX() to use LaTeX, but it doesn't support \mathbb or \mathcal
  scale_color_viridis_c(expression(sigma^2), direction = -1, option = 'D', end = 1,
                        begin = 0.3, limits = lim_sigma2, breaks = lim_sigma2,
                        labels = c('Low', 'High')) +
  scale_fill_gradient2(expression(italic(R)), low = LOW, mid = MID, high = HIGH,
                       limits = lim_food, breaks = lim_food, labels = c('Low', 'High'),
                       midpoint = quantile(lim_food, 0.5))

# b. variable definition
# b-1: Gamma distribution
plot_expr <- function(string) {
  ggplot() +
    annotate('text', 0, 0, label = string, parse = TRUE, size = 10) +
    theme_void()
}

# probability density of R
r_pdf <-
  ggplot(r_density, aes(r, dens)) +
  geom_area(fill = pal[3], alpha = 0.5, color = pal[3]) +
  scale_x_continuous(expression(italic(r)), breaks = NULL) +
  scale_y_continuous(expression(P(italic(R==r))), breaks = NULL)

## b-2, b-3: mean and variance trends
p_mean <-
  ggplot() +
  geom_point(aes(t, food), d_1, alpha = 0.25, show.legend = FALSE) +
  geom_line(aes(t, mu, color = mu), params, lwd = 1, show.legend = FALSE) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(expression(italic(R)), limits = lim_food, breaks = lim_food,
                     labels = c('Low', 'High')) +
  scale_color_gradient2(low = LOW, mid = MID, high = HIGH, limits = lim_food,
                        midpoint = quantile(lim_food, 0.5))

p_var <-
  ggplot() +
  geom_line(aes(t, sigma2_constant, color = sigma2_constant), params, lwd = 1,
            show.legend = FALSE) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(expression(sigma^2), limits = lim_sigma2,
                     breaks = lim_sigma2, labels = c('Low', 'High')) +
  scale_color_viridis_c(direction = -1, option = 'D', end = 1,
                        begin = 0.3, limits = lim_sigma2)

p_1b <-
  plot_grid(
    plot_expr('italic(R%~%Gamma(mu, sigma^2))'), r_pdf,
    plot_expr('E(italic(R))==mu(t)'), p_mean,
    plot_expr('V(italic(R))==sigma^2'), p_var,
    ncol = 2)

# c. raster of U with animal movement in low productivity season
rast_1 <- expand_grid(x = seq(0, 1, length.out = 10),
                      y = seq(0, 1, length.out = 10)) %>%
  mutate(food_low = rgamma2(min(params$mu), params$sigma2_constant, n()),
         food_high = rgamma2(max(params$mu), params$sigma2_constant, n()))
title_low <- paste0('italic(R)%~%Gamma(mu==', round(min(params$mu)), ',~sigma^2==',
                     round(unique(params$sigma2_constant)), ')')
title_high <- paste0('italic(R)%~%Gamma(mu==', round(max(params$mu)), ',~sigma^2==',
                     round(unique(params$sigma2_constant)), ')')
p_1c <-
  ggplot() +
  coord_equal() +
  geom_raster(aes(x, y, fill = food_low), rast_1, show.legend = FALSE) +
  geom_path(aes(x, y), sims, color = 'white', size = 1) +
  geom_path(aes(x, y), sims, color = 'black') +
  scale_x_continuous(NULL, expand = c(0, 0), breaks = NULL) +
  scale_y_discrete(NULL, expand = c(0, 0)) +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, limits = lim_food,
                       midpoint = quantile(lim_food, 0.5)) +
  labs(title = parse(text = eval(parse(text = 'title_low'))))

# c. raster of U with animal movement in high productivity season
p_1d <-
  ggplot() +
  coord_equal() +
  geom_raster(aes(x, y, fill = food_high), rast_1, show.legend = FALSE) +
  geom_path(aes(x, y), sims, color = 'white', size = 1) +
  geom_path(aes(x, y), sims, color = 'black') +
  scale_x_continuous(NULL, expand = c(0, 0), breaks = NULL) +
  scale_y_discrete(NULL, expand = c(0, 0)) +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, limits = lim_food,
                       midpoint = quantile(lim_food, 0.5)) +
  labs(title = parse(text = eval(parse(text = 'title_high'))))

p_1 <- plot_grid(p_1a,
                 plot_grid(p_1b,
                           plot_grid(p_1c, p_1d, labels = c('c.', 'd.'), ncol = 1),
                           nrow = 1, labels = c('b.', ''), rel_widths = 2:1),
                 labels = c('a.', ''), ncol = 1)

ggsave('figures/habitat-examples-constant-variance.png',
       plot = p_1, width = 7, height = 8, bg ='white')

# varying mean, varying variance ----
# samples
set.seed(4) # to ensure consistent samples within raster fill scale of R
d_2 <- expand_grid(t = 1:10,
                   patch = letters[t]) %>%
  mutate(mu = mean_fcn(t),
         sigma2 = var_fcn(t), # changing values
         food = rgamma2(mu = mu, sigma2 = sigma2, N = n()),
         e = food - mu)

# a. raster of abundance with events
p_2a <-
  ggplot(d_2, aes(t, patch)) +
  geom_raster(aes(fill = food)) +
  geom_point(color = 'black', size = 3) +
  geom_point(aes(color = sigma2), size = 2) +
  scale_x_continuous('Time', expand = c(0, 0), breaks = NULL) +
  scale_y_discrete('Patch', expand = c(0, 0)) +
  # can use latex2exp::TeX() to use LaTeX, but it doesn't support \mathbb or \mathcal
  scale_color_viridis_c(expression(sigma^2), direction = -1, option = 'D', end = 1,
                        begin = 0.3, limits = lim_sigma2, breaks = lim_sigma2,
                        labels = c('Low', 'High')) +
  scale_fill_gradient2(expression(italic(R)), low = LOW, mid = MID, high = HIGH,
                       limits = lim_food, breaks = lim_food, labels = c('Low', 'High'),
                       midpoint = quantile(lim_food, 0.5))

# b. variable definition
## b-2, b-3: mean and variance trends
p_mean <-
  ggplot() +
  geom_point(aes(t, food), d_2, alpha = 0.25, show.legend = FALSE) +
  geom_line(aes(t, mu, color = mu), params, lwd = 1, show.legend = FALSE) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(expression(italic(R)), limits = lim_food, breaks = lim_food,
                     labels = c('Low', 'High')) +
  scale_color_gradient2(low = LOW, mid = MID, high = HIGH, limits = lim_food,
                       midpoint = quantile(lim_food, 0.5))

p_var <-
  ggplot() +
  geom_line(aes(t, sigma2, color = sigma2), params, lwd = 1,
            show.legend = FALSE) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(expression(sigma^2), limits = lim_sigma2,
                     breaks = lim_sigma2, labels = c('Low', 'High')) +
  scale_color_viridis_c(direction = -1, option = 'D', end = 1,
                        begin = 0.3, limits = lim_sigma2)

p_2b <-
  plot_grid(
    plot_expr('italic(R%~%Gamma(mu, sigma^2))'), r_pdf,
    plot_expr('E(italic(R))==mu(t)'), p_mean,
    plot_expr('V(italic(R))==sigma^{2}*(t)'), p_var,
    ncol = 2)

# c. raster of U with animal movement in low productivity season
rast_2 <- expand_grid(x = seq(0, 1, length.out = 10),
                      y = seq(0, 1, length.out = 10)) %>%
  mutate(food_low = rgamma2(min(params$mu), min(params$sigma2), n()),
         food_high = rgamma2(max(params$mu), max(params$sigma2), n()))
title_low <- paste0('italic(R)%~%Gamma(mu==', round(min(params$mu)), ',~sigma^2==',
                    round(min(params$sigma2)), ')')
title_high <- paste0('italic(R)%~%Gamma(mu==', round(max(params$mu)), ',~sigma^2==',
                     round(max(params$sigma2)), ')')
p_2c <-
  ggplot() +
  coord_equal() +
  geom_raster(aes(x, y, fill = food_low), rast_2, show.legend = FALSE) +
  geom_path(aes(x, y), sims, color = 'white', size = 1) +
  geom_path(aes(x, y), sims, color = 'black') +
  scale_x_continuous(NULL, expand = c(0, 0), breaks = NULL) +
  scale_y_discrete(NULL, expand = c(0, 0)) +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, limits = lim_food,
                       midpoint = quantile(lim_food, 0.5)) +
  labs(title = parse(text = eval(parse(text = 'title_low'))))

# c. raster of U with animal movement in high productivity season
p_2d <-
  ggplot() +
  coord_equal() +
  geom_raster(aes(x, y, fill = food_high), rast_2, show.legend = FALSE) +
  geom_path(aes(x, y), sims, color = 'white', size = 1) +
  geom_path(aes(x, y), sims, color = 'black') +
  scale_x_continuous(NULL, expand = c(0, 0), breaks = NULL) +
  scale_y_discrete(NULL, expand = c(0, 0)) +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, limits = lim_food,
                       midpoint = quantile(lim_food, 0.5)) +
  labs(title = parse(text = eval(parse(text = 'title_high'))))

p_2 <- plot_grid(p_2a,
                 plot_grid(p_2b,
                           plot_grid(p_2c, p_2d, labels = c('c.', 'd.'), ncol = 1),
                           nrow = 1, labels = c('b.', NA), rel_widths = 2:1),
                 labels = c('a.', NA), ncol = 1)

ggsave('figures/habitat-examples-changing-variance.png',
       plot = p_2, width = 7, height = 8, bg ='white')
