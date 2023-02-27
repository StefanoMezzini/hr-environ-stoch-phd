library('dplyr')   # for data wrangling
library('ggplot2') # for fancy figures
library('cowplot') # for fancy multi-panel figures

theme_set(theme_bw()) # change default ggplot theme

d <- tibble(t = 1:1e3,
            mu_1 = sinpi(t / 200) * 2 + 20,
            s2_2 = sinpi(t / 200) * 2 + 20,
            y_1 = rnorm(n = length(t), mean = mu_1, sd = 1),
            y_2 = rnorm(n = length(t), mean = 0, sd = (mu_1 - 14)^2))

plot_grid(
  ggplot(d) +
    geom_point(aes(t, y_1), alpha = 0.3) +
    geom_line(aes(t, mu_1), color = '#ED7D31', linewidth = 2) +
    scale_x_continuous('Time', breaks = NULL) +
    scale_y_continuous('Resource abundance', breaks = NULL),
  ggplot(d) +
    geom_point(aes(t, y_2), alpha = 0.3) +
    geom_line(aes(t, 0), color = '#ED7D31', linewidth = 2) +
    scale_x_continuous('Time', breaks = NULL) +
    scale_y_continuous('Resource abundance', breaks = NULL),
  ncol = 1)

ggsave('figures/biol-417-lecture/change-in-mean-vs-stochasticity.png',
       width = 5, height = 6, bg = 'white', scale = 0.8)
