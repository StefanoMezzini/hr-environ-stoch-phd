library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('ctmm')      # for movement modeling
library('lubridate') # for working with data more smoothly
library('ggplot2')   # for fancy plots
library('latex2exp') # for LaTeX in plots
library('cowplot')   # for fancy plots in grids
library('ragg')    # needed for custom alpha with coord_cartesian
# see https://github.com/tidyverse/ggplot2/issues/4029
theme_set(theme_bw() + theme(legend.position = 'top', panel.grid = element_blank()))

# color-blind-friendly color palette
pal <- c('#ff8c00', '#4477AA', '#009900', '#66CCEE',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

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
  as_tibble() %>%
  mutate(#x = scales::rescale(x, to = c(1, 11), from = range(x)),
    y = scales::rescale(y, to = c(1, 11), from = range(y)))

ggplot(sims) +
  coord_equal(ratio = 0.1) +
  geom_path(aes(x, y))

# variable definition ----
## R = resource abundance
## S = success/failure
## U = R * S usable resource

# changing mean, p = 1 ----
d_1 <- expand_grid(t = seq(0, 1, by = 0.1), r = seq(0, 1, by = 0.1)) %>%
  mutate(patch = factor(letters[t * 10 + 1]),
         p = 1,
         s = rbinom(n = n(), size = 1, prob = p),
         u = r * s,
         e_u = r * p,
         sigma2 = p * (1 - p)) # Var(Y) = p * (1 - p) = 1 * 0 = 0

# a. raster of abundance with events
p_1a <-
  ggplot(d_1, aes(r, t)) +
  geom_raster(aes(fill = r)) +
  geom_point(color = 'black', size = 3) +
  geom_point(aes(color = sigma2), size = 2) +
  scale_x_continuous('Resource abundance', expand = c(0, 0), breaks = 0:1,
                     labels = c('Low', 'High')) +
  scale_y_continuous(expression(Probability~of~success~(italic(p))), expand = c(0, 0),
                     breaks = unique(d_1$t), labels = rep(1, n_distinct(d_1$t))) +
  # can use latex2exp::TeX() to use LaTeX, but it doesn't support \mathbb or \mathcal
  scale_color_viridis_c('Stochasticity', direction = -1, option = 'D', end = 1,
                        begin = 0.3, limits = c(0, 0.25), breaks = c(0, 0.125, 0.25),
                        labels = c('0', '0.125', '0.25')) +
  scale_fill_gradient2('Resource availability', low = LOW, mid = MID,
                       high = HIGH, limits = range(d_1$r), breaks = range(d_1$r),
                       labels = c('Low', 'High'))

# b. variable definition
plot_expr <- function(string) {
  ggplot() +
    annotate('text', 0, 0, label = string, parse = TRUE, size = 10) +
    theme_void()
}

r_density <-
  tibble(r = seq(0, 10, length.out = 400), dens = dgamma(r, shape = 3, scale = 1)) %>%
  ggplot(aes(r, dens)) +
  geom_area(fill = pal[1], alpha = 0.5, color = pal[1]) +
  scale_x_continuous(expression(italic(r)), breaks = NULL) +
  scale_y_continuous(expression(P(italic(R==r))), breaks = NULL)

s_density <-
  tibble(s = 0:1, dens = dbinom(s, 1, 1)) %>%
  ggplot(aes(s, dens)) +
  geom_bar(fill = pal[2], alpha = 0.5, color = pal[2], stat = 'identity', width = 0.5) +
  scale_x_continuous(expression(italic(s)), breaks = 0:1) +
  scale_y_continuous(expression(P(italic(S==s))))

u_density <-
  expand_grid(r = seq(0, 10, length.out = 400), s = 0:1) %>%
  mutate(r_dens = dgamma(r, shape = 3, scale = 1),
         s_dens = dbinom(s, size = 1, prob = 1),
         u = r * s,
         dens = r_dens * s_dens,
         dens_2 = if_else(u > 2.5, dens, NA_real_)) %>%
  ggplot(aes(u, dens)) +
  geom_area(fill = 'transparent', alpha = 0.5, color = pal[3]) +
  geom_area(aes(y = dens_2), fill = pal[3], alpha = 0.5, color = pal[3]) +
  scale_x_continuous(expression(italic(u==r%.%s)), breaks = NULL) +
  scale_y_continuous(expression(P(italic(U==u))), breaks = NULL, limits = c(0, 0.3))

p_1b <- plot_grid(plot_expr('italic(R%~%Gamma(k, theta))'),
                  r_density,
                  plot_expr('italic(S%~%Ber(p))'),
                  s_density,
                  plot_expr('italic(U==R%.%S)'),
                  u_density,
                  ncol = 2)

## with S ~ Beta(0, 1) -- not used; avoid a, b < 1 as they will result in Inf densities
if(FALSE) {
  r_density <-
    tibble(r = seq(0, 10, length.out = 400), dens = dgamma(r, shape = 3, scale = 1)) %>%
    ggplot(aes(r, dens)) +
    geom_area(fill = 'grey', alpha = 0.5, color = 'black') +
    scale_x_continuous(expression(italic(r)), breaks = NULL) +
    scale_y_continuous(expression(P(italic(R==r))), breaks = NULL)
  
  s_density <-
    tibble(s = seq(0, 1, length.out = 100), dens = dbeta(s, 5, 5)) %>%
    ggplot(aes(s, dens)) +
    geom_area(fill = 'grey', alpha = 0.5, color = 'black') +
    scale_x_continuous(expression(italic(s))) +
    scale_y_continuous(expression(P(S==s)), limits = 0:1)
  
  u_density <-
    expand_grid(r = seq(0, 10, length.out = 100), s = seq(0, 1, length.out = 100)) %>%
    mutate(r_dens = dgamma(r, shape = 3, scale = 1),
           s_dens = dbeta(s, shape1 = 5, shape2 = 5),
           u = r * s,
           dens = r_dens * s_dens) %>%
    ggplot(aes(r, s, fill = dens)) +
    geom_tile() +
    scale_x_continuous(expression(italic(r)), breaks = NULL, expand = c(0, 0)) +
    scale_y_continuous(expression(italic(s)), expand = c(0, 0), limits = c(0, 0.3)) +
    scale_fill_viridis_c(expression(P(italic(U==u)))) +
    theme(legend.position = 'right')
  
  plot_grid(plot_expr('italic(R%~%Gamma(k, theta))'),
            r_density,
            plot_expr('italic(S%~%Beta(a==5,b==5))'),
            s_density,
            plot_expr('italic(U==R%.%S)'),
            u_density,
            ncol = 2) 
}

# c. raster of U with animal movement
p_1c <-
  ggplot() +
  coord_equal(ratio = 0.1) +
  geom_raster(aes(r, patch, fill = u), d_1, show.legend = FALSE) +
  geom_path(aes(x, y), sims, color = 'white', size = 1) +
  geom_path(aes(x, y), sims, color = 'black') +
  scale_x_continuous('Resource abundance', expand = c(0, 0), breaks = 0:1,
                     labels = c('Low', 'High')) +
  scale_y_discrete('Patch', expand = c(0, 0)) +
  scale_fill_gradient2('Resource availability', low = LOW, mid = MID,
                       high = HIGH, limits = 0:1, breaks = range(d_1$u),
                       labels = c('Low', 'High'))

# d. raster of E(U) with animal movement
p_1d <-
  ggplot() +
  coord_equal(ratio = 0.1) +
  geom_raster(aes(r, patch, fill = e_u), d_1, show.legend = FALSE) +
  geom_path(aes(x, y), sims, color = 'white', size = 1) +
  geom_path(aes(x, y), sims, color = 'black') +
  scale_x_continuous('Resource abundance', expand = c(0, 0), breaks = 0:1,
                     labels = c('Low', 'High')) +
  scale_y_discrete('Patch', expand = c(0, 0)) +
  scale_fill_gradient2('Resource availability', low = LOW, mid = MID,
                       high = HIGH, limits = 0:1, breaks = 0:1,
                       labels = c('Low', 'High'))

p_1 <- plot_grid(p_1a,
                 plot_grid(p_1b,
                           plot_grid(p_1c, p_1d, labels = c('c.', 'd.'), ncol = 1),
                           labels = c('b.', NA), rel_widths = c(2, 1)),
                 labels = c('a.', NA), ncol = 1); p_1

ggsave('figures/bernoulli-constant-p.png', plot = p_1, width = 7, height = 8, bg ='white')

# varying p ----
d_2 <-
  expand_grid(r = seq(0, 1, by = 0.1), p = seq(0, 1, by = 0.1)) %>%
  mutate(patch = factor(letters[p * 10 + 1]),
         sigma2 = p * (1 - p),
         s = rbinom(n = n(), size = 1, prob = p),
         u = r * s,
         e_u = r * p)

# a. raster of abundance with events
p_2a <-
  ggplot(d_2, aes(r, p)) +
  geom_raster(aes(fill = r)) +
  geom_point(aes(alpha = s), color = 'black', size = 3) +
  geom_point(aes(alpha = s, color = sigma2), size = 2) +
  scale_x_continuous('Resource abundance', expand = c(0, 0), breaks = 0:1,
                     labels = c('Low', 'High')) +
  scale_y_continuous(expression(Probability~of~success~(italic(p))), expand = c(0, 0)) +
  scale_color_viridis_c('Stochasticity', direction = -1, option = 'D', end = 1,
                        begin = 0.3, limits = c(0, 0.25), breaks = c(0, 0.125, 0.25),
                        labels = c('0', '0.125', '0.25')) +
  scale_fill_gradient2('Resource availability', low = LOW, mid = MID,
                       high = HIGH, limits = 0:1, breaks = 0:1,
                       labels = c('Low', 'High')) +
  scale_alpha_continuous(range = 0:1) +
  guides(alpha = 'none')

# b. variable definition
r_density <-
  tibble(r = seq(0, 10, length.out = 400), dens = dgamma(r, shape = 3, scale = 1)) %>%
  ggplot(aes(r, dens)) +
  geom_area(fill = pal[1], alpha = 0.5, color = pal[1]) +
  scale_x_continuous(expression(italic(r)), breaks = NULL) +
  scale_y_continuous(expression(P(italic(R==r))), breaks = NULL)

s_density <-
  tibble(s = 0:1, dens = dbinom(s, 1, prob = 0.6)) %>%
  ggplot(aes(s, dens)) +
  geom_bar(fill = pal[2], alpha = 0.5, color = pal[2], stat = 'identity', width = 0.5) +
  scale_x_continuous(expression(italic(s)), breaks = 0:1) +
  scale_y_continuous(expression(P(italic(S==s))), limits = 0:1)

u_density <-
  expand_grid(r = seq(0, 10, length.out = 400), s = 0:1) %>%
  mutate(r_dens = dgamma(r, shape = 3, scale = 1),
         s_dens = dbinom(s, size = 1, prob = 0.6),
         u = r * s,
         dens = r_dens * s_dens,
         dens_2 = if_else(u > 2.5, dens, NA_real_)) %>%
  ggplot(aes(u, dens)) +
  coord_cartesian(ylim = c(0, 0.4)) +
  geom_area(fill = 'transparent', color = pal[3]) +
  geom_area(aes(y = dens_2), fill = pal[3], alpha = 0.5, color = pal[3]) +
  scale_x_continuous(expression(italic(u==r%.%s)), breaks = NULL) +
  scale_y_continuous(expression(P(italic(U==u))), breaks = NULL, limits = c(0, 0.3))

p_2b <- plot_grid(plot_expr('italic(R%~%Gamma(k, theta))'),
                  r_density,
                  plot_expr('italic(S%~%Ber(p))'),
                  s_density,
                  plot_expr('italic(U==R%.%S)'),
                  u_density,
                  ncol = 2)

# c. raster of U with animal movement
p_2c <-
  ggplot() +
  coord_equal(ratio = 0.1) +
  geom_raster(aes(r, patch, fill = u), d_2, show.legend = FALSE) +
  geom_path(aes(x, y), sims, color = 'white', size = 1) +
  geom_path(aes(x, y), sims, color = 'black') +
  scale_x_continuous('Resource abundance', expand = c(0, 0), breaks = 0:1,
                     labels = c('Low', 'High')) +
  scale_y_discrete('Patch', expand = c(0, 0)) +
  scale_fill_gradient2('Resource availability', low = LOW, mid = MID,
                       high = HIGH, limits = 0:1, breaks = 0:1,
                       labels = c('Low', 'High'))

# d. raster of E(U) with animal movement
p_2d <-
  ggplot() +
  coord_equal(ratio = 0.1) +
  geom_raster(aes(r, patch, fill = e_u), d_2, show.legend = FALSE) +
  geom_path(aes(x, y), sims, color = 'white', size = 1) +
  geom_path(aes(x, y), sims, color = 'black') +
  scale_x_continuous('Resource abundance', expand = c(0, 0), breaks = 0:1,
                     labels = c('Low', 'High')) +
  scale_y_discrete('Patch', expand = c(0, 0)) +
  scale_fill_gradient2('Resource availability', low = LOW, mid = MID,
                       high = HIGH, limits = 0:1, breaks = 0:1,
                       labels = c('Low', 'High'))

p_2 <- plot_grid(p_2a,
                 plot_grid(p_2b,
                           plot_grid(p_2c, p_2d, labels = c('c.', 'd.'), ncol = 1),
                           labels = c('b.', NA), rel_widths = c(2, 1)),
                 labels = c('a.', NA), ncol = 1); p_2

ggsave('figures/bernoulli-changing-p.png', plot = p_2, width = 7, height = 8, bg ='white')
