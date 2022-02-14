library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('purrr')   # for functional programming (e.g., map_*())
library('mgcv')    # for modeling
library('ggplot2') # for fancy plots
library('cowplot') # for multi-panel fancy plots
library('ragg')    # needed for custom alpha with coord_cartesian
# see https://github.com/tidyverse/ggplot2/issues/4029
theme_set(theme_bw() +
            theme(legend.position = 'none',
                  panel.grid = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank()))

# curstom color-vision deficient palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#AA3377', '#EE6677', '#BBBBBB')

# create a 2x2 panel with time and space, stochastic/predictable
# have 4 adaptation types: no response, adapts to mean, adapts to variance, or both
N <- 365 # number of days in a year
PRESENT <- 280 # current day
set.seed(1)
d <- expand_grid(Predictable = factor(c('neither', 'both', 'mean', 'variance')),
                 t = 1:N) %>%
  mutate(mean = case_when(grepl('both', Predictable) ~ 'predictable mean',
                          grepl('mean', Predictable) ~ 'predictable mean',
                          TRUE ~ 'stochastic mean'),
         variance = case_when(grepl('both', Predictable) ~ 'predictable variance',
                              grepl('variance', Predictable) ~ 'predictable variance',
                              TRUE ~ 'stochastic variance'),
         mu = if_else(grepl('predictable', mean),
                      true = (sinpi(t/N * 2 - 0.5) + 1.2) * 2,
                      false = rep(rnorm(N, sd = 0.2) %>% cumsum() %>% abs(), 4)),
         sigma2 = if_else(grepl('predictable', variance),
                          (sinpi((t)/N * 2 - 0.5) + 1.1) * 0.4,
                          ((sinpi(sqrt(t / 5)) + 1.01)) / 2 + rnorm(N * 4, 0.2, 0.2))^2,
         noise = rnorm(N*4, sd = sqrt(sigma2)),
         y = mu + noise,
         past = t < PRESENT)

plot_grid(ggplot(d) +
            facet_grid(mean ~ variance) +
            geom_vline(xintercept = PRESENT, color = 'grey') +
            geom_line(aes(t, mu, color = past), show.legend = FALSE) +
            scale_color_manual(breaks = c(FALSE, TRUE), values = c('grey', 'black')),
          ggplot(d) +
            facet_grid(mean ~ variance) +
            geom_vline(xintercept = PRESENT, color = 'grey') +
            geom_line(aes(t, noise, color = past), show.legend = FALSE) +
            scale_color_manual(breaks = c(FALSE, TRUE), values = c('grey', 'black')))

ggplot(d, aes(t, y)) +
  facet_grid(mean ~ variance) +
  geom_vline(xintercept = PRESENT, color = 'grey') +
  geom_point(alpha = 0.3) +
  scale_x_continuous('Time') +
  scale_y_continuous('Resource availability')

pred <- function(K, fixed, .d) {
  
  sub_d <- filter(.d, past)
  newd <- tibble(t = seq(0, 365, length.out = 500))
  
  map2_dfr(K, fixed, .f = \(k_mu, FX) {
    
    if(k_mu == 0) {
      # constant mean
      m <- gam(formula = list(y ~ 1, ~ s(t, k = 20, bs = 'bs')),
               family = gaulss(),
               data = sub_d,
               knots = list(x = c(0, 366)),
               method = 'REML')
    } else if(k_mu == 1) {
      # GLM for the mean
      m <- gam(formula = list(y ~ t, ~ s(t, k = 20, bs = 'bs')),
               family = gaulss(),
               data = sub_d,
               knots = list(x = c(0, 366)),
               method = 'REML')
    } else {
      # b splines (bs = 'bs') produce more constrained extrapolations
      m <- gam(formula = list(y ~ s(t, k = k_mu, bs = 'bs', fx = FX),
                              ~ s(t, k = 20, bs = 'bs')),
               family = gaulss(),
               data = sub_d,
               knots = list(x = c(0, 366)),
               method = 'REML')
    }
    
    predict(m, newdata = newd, se.fit = TRUE, type = 'response') %>%
      as.data.frame() %>%
      as_tibble() %>%
      bind_cols(newd) %>%
      transmute(t,
                mean = unique(sub_d$mean),
                variance = unique(sub_d$variance),
                mu = fit.1,
                mu_lwr = mu - 2 * se.fit.1,
                mu_upr = mu + 2 * se.fit.1,
                sigma2 = (1/fit.2)^2, # model returns the precision = 1/SD
                Adaptability = k_mu)
  })
}

ks <- c(0, 1, 20, 100) # constant, linear, smooth overfit
fxs <- c(FALSE, FALSE, FALSE, TRUE) # non-penalized spline in fourth model

predictions <- bind_rows(pred(ks, fxs, filter(d, Predictable == 'both')),
                         pred(ks, fxs, filter(d, Predictable == 'mean')),
                         pred(ks, fxs, filter(d, Predictable == 'variance')),
                         pred(ks, fxs, filter(d, Predictable == 'neither'))) %>%
  mutate(Adaptability = factor(Adaptability))

p_mean <-
  ggplot() +
  coord_cartesian(ylim = range(d$y)) + # zoom in
  facet_grid(mean ~ variance, scales = 'free_y') +
  geom_vline(xintercept = PRESENT, color = 'grey', alpha = 0.5, lty = 'dashed') +# present
  geom_point(aes(t, y), d, alpha = 0.1) + # data
  # geom_ribbon(aes(t, ymin = mu_lwr, ymax = mu_upr, fill = Adaptability),
  #             predictions, alpha = 0.2) +
  geom_line(aes(t, mu), d, color = '#000000') + # true trend
  geom_line(aes(t, mu, color = Adaptability), predictions, alpha = 0.9) + # estimates
  scale_x_continuous('Day of year') +
  scale_y_continuous('Resource availability') +
  scale_color_manual(values = pal, aesthetics = c('color', 'fill'),
                     labels = c('none', 'linear', 'smooth', 'rapid'))

p_var <-
  ggplot(predictions) +
  coord_cartesian(ylim = c(0, 4)) +
  facet_grid(mean ~ variance, scales = 'free_y') +
  geom_vline(xintercept = PRESENT, color = 'grey', alpha = 0.5, lty = 'dashed') +
  geom_line(aes(t, sigma2), d, color = '#000000') +
  geom_line(aes(t, sigma2, color = Adaptability), size = 1) +
  scale_x_continuous('Day of year') +
  scale_y_continuous('Variance in resource availability') +
  scale_color_manual(values = pal)

p <- plot_grid(get_legend(p_mean + theme(legend.position = 'top')), p_mean, p_var,
               labels = c(NA, 'a.', 'b.'), ncol = 1, rel_heights = c(0.1, 1, 1))

ggsave('figures/stochasticity-timescale.png', plot = p, width = 5, height = 6,
       bg = 'white', scale = 1.5)
