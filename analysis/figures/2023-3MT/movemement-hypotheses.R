library('ctmm')    # for movement modeling
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('ggplot2') # for fancy plots
library('cowplot') # for fancy multi-panel plots
source('analysis/figures/default-figure-styling.R') # for common theme and color palette

theme_set(
  theme_classic() +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.line = element_line(
            arrow = arrow(type = 'closed', length = unit(0.12, 'inches')),
            lineend = 'round')))

# hypothesis for change in H over E(R) ----
d <- tibble(mu = seq(0, 5, length.out = 400),
            sigma2 = rev(mu),
            y = exp(5 - mu) + 10)

y_lims <- c(0, 125)
x_breaks <- quantile(range(d$mu), probs = c(0.15, 0.85))
y_breaks <- c(min(d$y), quantile(y_lims, probs = 0.85))

p_mu <-
  ggplot() +
  coord_cartesian(ylim = y_lims) +
  geom_area(aes(mu, y = y), d, col = pal[1], fill = pal[1], alpha = 0.3,
            linewidth = 1) +
  scale_x_continuous('Resource abundance', breaks = x_breaks,
                     labels = c('Low', 'High'), expand = c(0, 0)) +
  scale_y_continuous('Home range size', breaks = y_breaks,
                     labels = c('Low', 'High'), expand = c(0, 0)); p_mu

# hypothesis for change in H over V(R) ----
p_s2 <-
  ggplot() +
  coord_cartesian(ylim = y_lims) +
  geom_area(aes(mu, y = y), d, col = pal[2], fill = pal[2], alpha = 0.3,
            linewidth = 1) +
  scale_x_reverse('Resource unpredictability', breaks = rev(x_breaks),
                  labels = c('Low', 'High'), expand = c(0, 0)) +
  scale_y_continuous('Home range size', breaks = y_breaks,
                     labels = c('Low', 'High'), expand = c(0, 0)); p_s2

# save the plots ----
ggsave('figures/2023-3MT/mean-abundance-hr-hypotheses.png', plot = p_mu,
       width = 6, height = 6, dpi = 600, bg = 'white', scale = 0.5)
ggsave('figures/2023-3MT/variance-abundance-hr-hypotheses.png', plot = p_s2,
       width = 6, height = 6, dpi = 600, bg = 'white', scale = 0.5)
