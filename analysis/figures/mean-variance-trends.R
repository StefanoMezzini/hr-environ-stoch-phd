library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling (expand_grid())
library('ggplot2') # for fancy plots
library('cowplot') # for multi-panel plots
source('analysis/default-figure-styling.R')

N <- 200
types <- c('constant', 'linear', 'cyclical', 'drifting', 'erratic')
set.seed(1)
d55 <-
  expand_grid(mean = types, # create a nested dataset with all combinations of `types`
              variance = types,
              t = list(tibble(t = 1:N))) %>%
  mutate( # assign trends in means and variances depending on the type
    mu = case_when(mean == 'constant' ~ list(tibble(mu = 0)),
                   mean == 'linear' ~ list(tibble(mu = seq(-20, 20, length.out = N))),
                   mean == 'cyclical' ~ list(tibble(mu = sin(1:N / 10) * 20)),
                   mean == 'drifting' ~ list(tibble(mu = cumsum(rnorm(N, sd = 4))) %>%
                                               mutate(mu = mu - mean(mu))),
                   mean == 'erratic' ~ list(tibble(mu = (-2)^(round(1:N/80)) * 4))),
    sigma2 =
      case_when(variance == 'constant' ~ list(tibble(sigma2 = 100)),
                variance == 'linear' ~ list(tibble(sigma2 = seq(0, 250, length.out = N))),
                variance == 'cyclical' ~ list(tibble(sigma2 = sin(1:N/15) + 1)^3 *100+30),
                variance == 'drifting' ~ list(tibble(sigma2 = cumsum(rnorm(N,sd=2))^2+9)),
                variance == 'erratic' ~ tibble(sigma2=((-2)^(round(1:N/80))+2.2)*50) %>%
                  list()))%>%
  unnest(c(t, mu, sigma2)) %>% # unnest the nested data frames to crease a single tibble
  mutate(mean = factor(mean, levels = types), # convert to factor to plot in order
         variance = factor(variance, levels = types),
         y = rnorm(length(t), mean = mu, sd = sqrt(sigma2)), # create samples
         lwr = mu - 2 * sqrt(sigma2), # calculate the true 2SE confidence intervals
         upr = mu + 2 * sqrt(sigma2))

# mean
p_mean <-
  ggplot(d55, aes(t, mu)) +
  facet_grid(mean ~ ., scales = 'free_y', switch = 'y') +
  geom_point(aes(y = y), alpha = 0) +
  geom_line(color = pal[1], lwd = 1) +
  scale_x_continuous('', breaks = NULL) +
  scale_y_continuous(expression(Mean~italic(R)), breaks = NULL) +
  theme(strip.background = element_blank(), axis.title = element_text(face = 'bold'))

# variance
p_variance <-
  ggplot(d55, aes(t, sigma2)) +
  facet_wrap(. ~ variance, nrow = 1) +
  geom_line(color = pal[2], lwd = 1) +
  scale_x_continuous(expression(Variance~'in'~italic(R)), breaks = NULL, position='top') +
  scale_y_continuous('', breaks = NULL) +
  theme(strip.background = element_blank(), axis.title = element_text(face = 'bold'))

# simulation
p55 <-
  ggplot(d55, aes(t, y)) +
  facet_grid(mean ~ variance, switch = 'both', scales = 'free') +
  geom_point(alpha = 0.3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = 'red3', alpha = 0.3) +
  geom_smooth(color = 'red', se = FALSE, method = 'gam', formula = y ~ s(x, k = 15)) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous('Resource availability', breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

plot_grid(plot_grid(NULL, p_variance, rel_widths = c(1, 4.5), nrow = 1),
          plot_grid(p_mean, p55, rel_widths = c(1, 4.5), nrow = 1),
          rel_heights = c(1, 4), nrow = 2)

ggsave('figures/mean-variance-5-by-5.png', width = 8, height = 4.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# (constant, constant) panel
filter(d55, mean == 'constant', variance == 'constant') %>%
  ggplot(aes(t, y)) +
  facet_grid(mean ~ variance, switch = 'both', scales = 'free') +
  geom_point(alpha = 0.3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = 'red3', alpha = 0.3) +
  geom_smooth(color = 'red', se = FALSE, method = 'gam', formula = y ~ s(x, k = 15)) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous('Resource availability', breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

ggsave('figures/c-c-resources.png', width = 3, height = 1.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# (linear, constant) panel
filter(d55, mean == 'linear', variance == 'constant') %>%
  ggplot(aes(t, y)) +
  facet_grid(mean ~ variance, switch = 'both', scales = 'free') +
  geom_point(alpha = 0.3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = 'red3', alpha = 0.3) +
  geom_smooth(color = 'red', se = FALSE, method = 'gam', formula = y ~ s(x, k = 15)) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous('Resource availability', breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

ggsave('figures/l-c-resources.png', width = 3, height = 1.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# (linear, linear) panel
filter(d55, mean == 'linear', variance == 'linear') %>%
  ggplot(aes(t, y)) +
  facet_grid(mean ~ variance, switch = 'both', scales = 'free') +
  geom_point(alpha = 0.3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = 'red3', alpha = 0.3) +
  geom_smooth(color = 'red', se = FALSE, method = 'gam', formula = y ~ s(x, k = 15)) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous('Resource availability', breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

ggsave('figures/l-l-resources.png', width = 3, height = 1.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')
