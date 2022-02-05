library('dplyr')   # for data wrangling
library('mgcv')    # for modeling
library('ggplot2') # for fancy plots
library('cowplot') # for multi-panel fancy plots
theme_set(theme_bw() +
            theme(legend.position = 'none',
                  panel.grid.minor = element_blank(),
                  axis.ticks.x = element_blank()))

# curstom color-vision deficient palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#AA3377', '#EE6677', '#BBBBBB')

set.seed(1)
d <- tibble(t = -20:120,
            y = case_when(t < 25 ~ 1, t < 50 ~ 2, t < 75 ~ 4, TRUE ~ 5) +
              rnorm(length(t), sd = 0.1))

ggplot(d) +
  geom_point(aes(t, y)) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous('Resource availability', limits = c(0, NA), breaks = NULL)

pred <- function(K) {
  
  newd <- tibble(t = seq(min(d$t), max(d$t), length.out = 500))
  
  if(K > 1) {
    m <- gam(formula = list(y ~ s(t, k = K), ~ s(t, k = K)),
             family = gaulss(),
             data = d,
             method = 'REML')
  } else {
    m <- gam(formula = list(y ~ t, ~ t), family = gaulss(),
             data = d,
             method = 'REML')
  }
  m %>%
    predict(newdata = newd, se.fit = FALSE, type = 'response') %>%
    as_tibble(.name_repair = 'unique') %>%
    transmute(t = newd$t,
              mean = ...1,
              variance = (1/...2)^2,
              K = K) %>%
    left_join(d, by = 't') %>%
    mutate(e = y - mean)
}

predictions <- bind_rows(pred(1), pred(6), pred(60)) %>% mutate(K = factor(K))

p_mean <-
  ggplot(d) +
  geom_point(aes(t, y), alpha = 0.5) +
  geom_line(aes(t, mean, color = K, group = K), predictions, lwd = 1) +
  scale_x_continuous('Time', labels = NULL) +
  scale_y_continuous('Resource availability', limits = c(0, NA)) +
  scale_color_manual('Time scale', values = pal, labels = c('short', 'medium', 'long')) +
  theme(axis.ticks = element_blank(), axis.text = element_text(color = 'transparent'))

p_var <-
  ggplot(predictions) +
  geom_line(aes(t, variance, color = K, group = K), lwd = 1) +
  scale_x_continuous('Time', labels = NULL) +
  scale_y_continuous('Variance in resource availability', limits = c(0, NA), breaks = 0) +
  scale_color_manual(values = pal)

plot_grid(get_legend(p_mean + theme(legend.position = 'top')),
          plot_grid(p_mean, p_var, labels = c('a.', 'b.')),
          ncol = 1, rel_heights = c(0.1, 1), label_y = 1.07)

ggsave('figures/stochasticity-timescale.png', width = 6, height = 3, bg = 'white',
       scale = 1.5)
