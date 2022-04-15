library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling (expand_grid())
library('ggplot2') # for fancy plots
library('cowplot') # for multi-panel plots
source('analysis/default-figure-styling.R') # for a consistent figure theme
source('functions/mean-variance-trends.R')  # functions to generate means and variances
source('functions/rgamma2.R')               # rgamma() parameterized by mean and variance
source('functions/qgamma2.R')               # qgamma() parameterized by mean and variance
source('analysis/figures/mean-variance-trends-panel-data.R') # create tibble of parameters
set.seed(3) # for consistent results

N <- 200
types <- c('constant', 'linear', 'cyclical', 'drifting', 'erratic')

d55 <- mutate(d55,
              y = rgamma2(mu = mu, sigma2 = sigma2, N = length(t)), # create samples
              lwr = qgamma2(0.025, mu, sigma2), # calculate true 95% CIs
              upr = qgamma2(0.975, mu, sigma2))

# mean
p_mean <-
  ggplot(d55, aes(t, mu)) +
  facet_grid(. ~ mean) +
  geom_line(color = pal[1], lwd = 1) +
  scale_x_continuous(expression(Mean~italic(U)), breaks = NULL, position = 'top') +
  scale_y_continuous(expression(italic(U)), breaks = NULL) +
  theme(strip.background = element_blank(), axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(colour = 'transparent'))

# variance
p_variance <-
  ggplot(d55, aes(t, sigma2)) +
  facet_grid(variance ~ ., switch = 'y') +
  geom_line(color = pal[2], lwd = 1) +
  scale_x_continuous('', breaks = NULL) +
  scale_y_continuous(expression(Variance~'in'~italic(U)), breaks = NULL) +
  theme(strip.background = element_blank(), axis.title = element_text(face = 'bold'))

# simulation
p55 <-
  ggplot(d55) +
  facet_grid(variance ~ mean, switch = 'both', scales = 'free') +
  geom_point(aes(t, y), alpha = 0.3) +
  geom_ribbon(aes(t, y, ymin = lwr, ymax = upr), fill = 'red3', alpha = 0.3) +
  geom_line(aes(t, mu), color = 'red', lwd = 1) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(expression(Resource~availability~(italic(U))), breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

plot_grid(plot_grid(NULL, p_mean, rel_widths = c(1, 4.4), nrow = 1),
          plot_grid(p_variance, p55, rel_widths = c(1, 4.5), nrow = 1),
          rel_heights = c(1, 4), nrow = 2)

ggsave('figures/mean-variance-5-by-5.png', width = 8, height = 4.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# (constant, constant) panel
filter(d55, mean == 'constant', variance == 'constant') %>%
  ggplot(aes(t, y)) +
  facet_grid(mean ~ variance, switch = 'both', scales = 'free') +
  geom_point(alpha = 0.3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = 'red3', alpha = 0.3) +
  geom_smooth(color = 'red', se = FALSE, method = 'gam', formula = y ~ s(x, k = 50)) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(expression(Resource~availability~(italic(U))), breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

ggsave('figures/c-c-resources.png', width = 3, height = 1.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# (linear, constant) panel
filter(d55, mean == 'linear', variance == 'constant') %>%
  ggplot(aes(t, y)) +
  facet_grid(mean ~ variance, switch = 'both', scales = 'free') +
  geom_point(alpha = 0.3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = 'red3', alpha = 0.3) +
  geom_smooth(color = 'red', se = FALSE, method = 'gam', formula = y ~ s(x, k = 50)) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(expression(Resource~availability~(italic(U))), breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

ggsave('figures/l-c-resources.png', width = 3, height = 1.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# (linear, linear) panel
filter(d55, mean == 'linear', variance == 'linear') %>%
  ggplot(aes(t, y)) +
  facet_grid(mean ~ variance, switch = 'both', scales = 'free') +
  geom_point(alpha = 0.3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = 'red3', alpha = 0.3) +
  geom_smooth(color = 'red', se = FALSE, method = 'gam', formula = y ~ s(x, k = 50)) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(expression(Resource~availability~(italic(U))), breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

ggsave('figures/l-l-resources.png', width = 3, height = 1.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# (***, constant) panel
p15 <-
  ggplot(filter(d55, variance == 'constant')) +
  facet_grid(variance ~ mean, switch = 'both', scales = 'free') +
  geom_point(aes(t, y), alpha = 0.3) +
  geom_ribbon(aes(t, y, ymin = lwr, ymax = upr), fill = 'red3', alpha = 0.3) +
  geom_line(aes(t, mu), color = 'red', lwd = 1) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(expression(Resource~availability~(italic(U))), breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

# variance
p_variance <-
  ggplot(filter(d55, variance == 'constant'), aes(t, sigma2)) +
  facet_grid(variance ~ ., switch = 'y') +
  geom_line(color = pal[2], lwd = 1) +
  scale_x_continuous('', breaks = NULL) +
  scale_y_continuous(expression(Variance~'in'~italic(U)), breaks = NULL) +
  theme(strip.background = element_blank(), axis.title = element_text(face = 'bold'))

plot_grid(plot_grid(NULL, p_mean, rel_widths = c(1, 4.4), nrow = 1),
          plot_grid(p_variance, p15, rel_widths = c(1, 4.5), nrow = 1),
          rel_heights = c(1, 1), nrow = 2)

ggsave('figures/mean-variance-1-by-5.png', width = 8, height = 2, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# notify when done
beepr::beep(sound = 2)
