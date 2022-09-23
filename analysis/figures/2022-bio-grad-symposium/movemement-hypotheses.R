library('ctmm')    # for movement modeling
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('ggplot2') # for fancy plots
library('ggbrace') # for curly braces on fancy plots
library('cowplot') # for fancy multi-panel plots
source('analysis/figures/default-figure-styling.R') # for common theme and color palette

theme_set(theme_get() + theme(legend.position = 'none'))

# hypothesis for change in H over E(R) ----
d <- tibble(mu = seq(0, 5, length.out = 400),
            sigma2 = rev(mu),
            y = exp(sigma2) + 7)

p_mu <-
  ggplot() +
  coord_cartesian(ylim = c(0, 125)) +
  geom_area(aes(mu, y = y), d, col = pal[3], fill = pal[3], alpha = 0.3) +
  scale_x_continuous('Resource abundance', breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous('Home range size', breaks = NULL, expand = c(0, 0)); p_mu

# hypothesis for change in H over V(R) ----
p_s2 <-
  p_mu +
  scale_x_reverse('Resource unpredictability', breaks = NULL,
                  expand = c(0, 0)); p_s2

# save the plots ----
ggsave('figures/2022-bio-grad-symposium/mean-abundance-hr-hypotheses.png',
       plot = p_mu, width = 3, height = 3, dpi = 400, bg = 'white')
ggsave('figures/2022-bio-grad-symposium/variance-abundance-hr-hypotheses.png',
       plot = p_s2, width = 3, height = 3, dpi = 400, bg = 'white')
