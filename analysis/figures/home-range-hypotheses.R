library('ctmm')    # for movement modeling
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('ggplot2') # for fancy plots
library('cowplot') # for fancy multi-panel plots
source('analysis/figures/default-figure-styling.R') # for common theme and color palette

hr_lab <- expression(Home~range~size~(italic(H))) # label for y axis

# simulate animal movement ----
p_track <-
  simulate(ctmm(tau = c(Inf, 1), sigma = 1), t = seq(1, 400, by = 0.05), seed = 5) %>%
  SpatialPoints.telemetry() %>%
  data.frame() %>%
  ggplot(aes(x, y)) +
  coord_equal() +
  geom_path() +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(plot.background = element_blank())

# hypothesis for change in H over E(R) ----
p_mu <-
  tibble(mu = seq(0, 1, length.out = 400),
         h_1 = -log(mu),
         h_2 = if_else(mu < 0.2, 4, sinpi(0.5 - (mu - 0.2) / 2)^10 * 4),
         h_3 = 2 - mu * 4) %>%
  pivot_longer(-mu, names_to = 'behavior', values_to = 'h') %>%
  mutate(h = h + 0.4) %>%
  ggplot(aes(mu, h, group = behavior, color = behavior, fill = behavior)) +
  geom_area(position = 'identity', show.legend = FALSE, alpha = 0.1) +
  labs(x = '\U1D53C(\U1D445)', y = hr_lab) +
  scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous(breaks = 0, expand = c(0, 0)) +
  scale_color_manual(values = pal[-(1:3)], aesthetics = c('color', 'fill'))

# hypothesis for change in H over V(R) ----
# vary color of lines by certainty ([0, 1]), with 1 being black and 0 being black w 0% alpha
p_s2 <-
  tibble(sigma2 = seq(0, 1, length.out = 400),
         h_0 = sigma2,
         h_1 = exp(sigma2 * 0.5) - 1,
         h_2 = 0.5 / (1 + exp(5 - sigma2 * 12))) %>%
  pivot_longer(-sigma2, names_to = 'behavior', values_to = 'h') %>%
  mutate(h = h + 0.1) %>%
  ggplot(aes(sigma2, h, group = behavior, color = behavior, fill = behavior)) +
  geom_area(position = 'identity', show.legend = FALSE, alpha = 0.1) +
  labs(x = '\U1D54D(\U1D445)', y = hr_lab) +
  scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous(breaks = 0, expand = c(0, 0)) +
  scale_color_manual(values = pal[-(1:3)], aesthetics = c('color', 'fill'))

# add the animal tracks ---
p_e <-
  ggdraw(p_mu) +
  draw_plot(p_track, x = 1, y = 1.0335, width = 0.45, height = 0.45, hjust = 1, vjust = 1)

p_v <-
  ggdraw(p_s2) +
  draw_plot(p_track, x = 0.096, y = 1.0335, width = 0.45, height = 0.45, hjust = 0,
            vjust = 1)

# save the plots
ggsave('figures/mean-abundance-hr-hypotheses.png', plot = p_e, width = 4, height = 4,
       dpi = 'retina', bg = 'white')
ggsave('figures/variance-abundance-hr-hypotheses.png', plot = p_v, width = 4, height = 4,
       dpi = 'retina', bg = 'white')
