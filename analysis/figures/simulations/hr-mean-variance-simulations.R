library('ggplot2') # for fancy plots
library('cowplot') # for multi-panel fancy plots
library('mgcv')    # for empirical bayesian modeling
source('analysis/figures/default-figure-styling.R') # defaults for figures (theme, size)
source('analysis/figures/mean-variance-trends-panel-data.R') # create tibble of parameters

days <- readRDS('simulations/days-hrs.rds')

# main simulation figure of change in home range (5x5 plot) ----
hr_lab <- expression(Home~range~size~(italic(H))) # label for y axis
p_sim <-
  ggplot(days) +
  facet_grid(variance ~ mean, drop = FALSE) + # facet by trends in mean and variance
  # area between 50% and 95% HRs
  geom_ribbon(aes(animal, ymin = hr_50, ymax = hr_95), fill = pal[3], alpha = 0.2) +
  # add lines for 50% and 95% HRs
  geom_line(aes(animal, hr_50), color = pal[3], lwd = 1) +
  geom_line(aes(animal, hr_95), color = pal[3]) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(hr_lab, breaks = NULL, limits = c(0, NA)) +
  # remove facet names since they appear in the marginal panels
  theme(strip.background = element_blank(), strip.text = element_blank())

# trends in the mean
p_mean <-
  ggplot(d55, aes(animal, mu)) +
  facet_grid(. ~ mean) +
  geom_line(color = pal[1], lwd = 1) +
  scale_x_continuous('\U1D53C(\U1D445)', breaks = NULL, position = 'top') +
  scale_y_continuous(hr_lab, breaks = NULL) +
  theme(strip.background = element_blank(),
        axis.title.x = element_text(family = 'Symbola'), 
        axis.title.y = element_text(family = 'Symbola', colour = 'transparent'))

# trends in the variance
p_variance <-
  ggplot(d55, aes(animal, sigma2)) +
  facet_grid(variance ~ ., switch = 'y') +
  geom_line(color = pal[2], lwd = 1) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous('\U1D54D(\U1D445)', breaks = NULL) +
  theme(strip.background = element_blank(),
        axis.title.x = element_text(color = 'transparent'),
        axis.title.y = element_text(family = 'Symbola'))

# create a single plot
## convert to grid graphical objects (grobs)
grobs <- lapply(list(p_mean, p_variance, p_sim), as_grob)

## align left margins of all plots
aligned_widths <- align_margin(lapply(grobs, function(x) {x$widths}), 'first')

## align bottom margins of all plots
aligned_heights <- align_margin(lapply(grobs[-1], function(x) {x$heights}), 'last')

## set the dimensions of plots to the aligned dimensions
for (i in seq_along(grobs)) {
  grobs[[i]]$widths <- aligned_widths[[i]]
}

for(i in seq_along(grobs[-1])) {
  grobs[[i + 1]]$heights <- aligned_heights[[i]]
}

## create final plot
p_full <- plot_grid(plot_grid(NULL, grobs[[1]], rel_widths = c(1, 4.5), nrow = 1),
                    plot_grid(grobs[[2]], grobs[[3]], rel_widths = c(1, 4.5), nrow = 1),
                    rel_heights = c(1, 3.5), nrow = 2)
p_full

# save the plot as a png
ggsave('figures/mean-variance-5-by-5-hr-sims.png', p_full, width = 10, height = 5.625,
       scale = 1, units = 'in', dpi = 'print', bg = 'white')

# estimate fraction of animals not satisfied if only mean is accounted for ----
days_suff <-
  left_join(days,
            filter(days, variance == 'constant') %>%
              select(mean, animal, hr_50, hr_95) %>%
              rename(hr_50_x = hr_50, hr_95_x = hr_95),
            by = c('mean', 'animal')) %>%
  # see if the animal has enough space based on the 95% quantile
  mutate(enough_50 = hr_50 <= hr_95_x,
         enough_95 = hr_95 <= hr_95_x,
         additional = if_else(enough_95, hr_95, hr_95_x))

p_sim_suff <-
  ggplot(days_suff) +
  facet_grid(variance ~ mean, drop = FALSE) + # facet by trends in mean and variance
  # area between 50% and 95% HRs
  geom_ribbon(aes(animal, ymin = hr_50_x, ymax = hr_95_x), fill = pal[3], alpha = 0.2) +
  # area missing to satisfy needs
  geom_ribbon(aes(animal, ymin = hr_95, ymax = additional), fill = pal[7], alpha = 0.2) +
  # add lines for 50% and 95% HRs
  geom_line(aes(animal, hr_50, color = enough_50, group = 1), lwd = 1) +
  geom_line(aes(animal, hr_95, color = enough_95, group = 1)) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(hr_lab, breaks = NULL, limits = c(0, NA)) +
  scale_color_manual('Space is sufficient', values = pal[c(7, 3)],
                     labels = c('No', 'Yes'), aesthetics = c('color', 'fill')) +
  # remove facet names since they appear in the marginal panels
  theme(strip.background = element_blank(), strip.text = element_blank(),
        legend.position = 'top')

p_full_suff <-
  plot_grid(get_legend(p_sim_suff),
            plot_grid(NULL, p_mean, rel_widths = c(1, 4.4), nrow = 1),
            plot_grid(p_variance, p_sim_suff + theme(legend.position = 'none'),
                      rel_widths = c(1, 4.5), nrow = 1),
            rel_heights = c(0.2, 1, 4), nrow = 3)
p_full_suff

# save the plot as a png
ggsave('figures/mean-variance-5-by-5-hr-sims-sufficient.png', p_full_suff, width = 8,
       height = 4.68, scale = 2, units = 'in', dpi = 'print', bg = 'white')

# regression plots ----
days_long <- pivot_longer(days, c(hr_50, hr_95), names_to = 'quantile', values_to = 'hr')

reg_mu <-
  ggplot(days_long) +
  geom_point(aes(mu, hr, color = quantile), alpha = 0.3) +
  geom_smooth(aes(mu, hr, group = quantile), method = 'gam', formula = y ~ s(x),
              color = 'black') +
  scale_x_continuous('\U1D53C(\U1D445)', breaks = NULL) +
  scale_y_continuous(hr_lab, breaks = NULL) +
  scale_color_manual('Utilization quantile', values = pal[4:5], labels = c('50%', '95%')) +
  theme(legend.position = 'none')

reg_s2 <-
  ggplot(days_long) +
  geom_point(aes(sigma2, hr, color = quantile), alpha = 0.3) +
  geom_smooth(aes(sigma2, hr, group = quantile), method = 'gam', formula = y ~ s(x),
              color = 'black') +
  scale_x_continuous('\U1D54D(\U1D445)', breaks = NULL) +
  scale_y_continuous(hr_lab, breaks = NULL) +
  scale_color_manual('Utilization quantile', values = pal[4:5], labels = c('50%', '95%')) +
  theme(legend.position = 'none')

regs <- plot_grid(get_legend(reg_mu + theme(legend.position = 'top')),
                  plot_grid(reg_mu, reg_s2, nrow = 1, labels = c('a.', 'b.')),
                  ncol = 1, rel_heights = c(0.15, 1))

ggsave('figures/simulations/simulation-regression-plots.png', plot = regs, width = 6,
       height = 2, dpi = 600, bg = 'white', scale = 1.5)
