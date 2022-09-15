library('ggplot2') # for fancy plots
library('cowplot') # for multi-panel fancy plots
library('mgcv')    # for empirical bayesian modeling
source('analysis/figures/default-figure-styling.R') # defaults for figures (theme, size)
source('analysis/figures/mean-variance-trends-panel-data.R') # create tibble of parameters

days <- readRDS('simulations/days-hrs.rds') %>%
  filter(! mean %in% c('constant', 'erratic'),
         ! variance %in% c('constant', 'erratic')) # remove repeated values

# fit a model ----
m <- gam(list(hr_95 ~ s(mu, k = 10, bs = 'tp') + s(sigma2, k = 10, bs = 'tp') +
                ti(mu, sigma2, k = 5, bs = 'tp'),
              ~ s(mu, k = 10, bs = 'tp') + s(sigma2, k = 10, bs = 'tp')),
         family = gammals(),
         data = days,
         method = 'REML',
         control = gam.control(nthreads = 4))

newd <- tibble(mu = seq(min(days$mu), max(days$mu), length.out = 400),
               sigma2 = seq(min(days$sigma2), max(days$sigma2), length.out = 400))
preds <- mutate(newd,
                mu_hr = predict(m, newdata = newd, se.fit = FALSE,
                                terms = 's(mu)', type = 'response')[, 1],
                sigma2_hr = predict(m, newdata = newd, se.fit = FALSE,
                                    terms = 's(sigma2)', type = 'response')[, 1])

# regression plots ----
reg_mu <-
  ggplot() +
  geom_point(aes(mu, hr_95), days, alpha = 0.1) +
  geom_line(aes(mu, mu_hr), preds, color = pal[1], lwd = 1.5) +
  scale_x_continuous('Resource abundance', breaks = NULL) +
  scale_y_continuous('Spatial needs', breaks = NULL)

reg_s2 <-
  ggplot(days) +
  geom_point(aes(sigma2, hr_95), alpha = 0.1) +
  geom_line(aes(sigma2, sigma2_hr), preds, color = pal[2], lwd = 1.5) +
  scale_x_continuous('Resource unpredictability', breaks = NULL) +
  scale_y_continuous('Spatial needs', breaks = NULL)

regs <- plot_grid(reg_mu, reg_s2, nrow = 1)

ggsave('figures/2022-grad-symposium/simulation-regression-plots.png',
       plot = regs, width = 10, height = 4, dpi = 300, bg = 'white', scale = 0.75)
