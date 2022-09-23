library('ggplot2') # for fancy plots
library('cowplot') # for multi-panel fancy plots
library('mgcv')    # for empirical bayesian modeling
source('analysis/figures/default-figure-styling.R') # defaults for figures (theme, size)
source('analysis/figures/mean-variance-trends-panel-data.R') # create tibble of parameters

days <- readRDS('simulations/days-hrs.rds') %>%
  filter(! mean %in% c('constant', 'erratic'),
         ! variance %in% c('constant', 'erratic')) # remove repeated values
hr_lab <- 'Home range size'

# regression plots ----
reg_mu <-
  ggplot(days) +
  geom_point(aes(mu, hr_95), alpha = 0.2, color = pal[3]) +
  geom_smooth(aes(mu, hr_95), color = pal[1], lwd = 1.5, se = FALSE,
              method = 'gam', formula = y ~ x,
              method.args = list(family = "Gamma")) +
  scale_x_continuous('Resource abundance', breaks = NULL) +
  scale_y_continuous(hr_lab, breaks = NULL) +
  scale_color_manual('Utilization quantile', values = pal[4:5], labels = c('50%', '95%')) +
  theme(legend.position = 'none')

reg_s2 <-
  ggplot(days) +
  geom_point(aes(sigma2, hr_95), alpha = 0.2, color = pal[3]) +
  geom_smooth(aes(sigma2, hr_95), color = pal[2], lwd = 1.5, se = FALSE,
              method = 'gam', formula = y ~ x,
              method.args = list(family = "Gamma")) +
  scale_x_continuous('Resource unpredictability', breaks = NULL) +
  scale_y_continuous(hr_lab, breaks = NULL) +
  scale_color_manual('Utilization quantile', values = pal[4:5], labels = c('50%', '95%')) +
  theme(legend.position = 'none')

regs <- plot_grid(reg_mu, reg_s2, nrow = 1)

ggsave('figures/2022-bio-grad-symposium/simulation-regression-plots.png',
       plot = regs, width = 10, height = 4, dpi = 300, bg = 'white',
       scale = 0.75)
