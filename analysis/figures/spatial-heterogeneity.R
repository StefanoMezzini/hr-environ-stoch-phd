library('dplyr')   # for data wrangling (mutate(), %>%)
library('purrr')   # for functional programming (map(), map_*())
library('tidyr')   # for data wrangling (pivot_*())
library('ggplot2') # for fancy plots
library('cowplot') # for fancy plot grids
theme_set(theme_bw() +
            theme(strip.background = element_blank(), axis.title = element_blank(),
                  axis.text = element_blank(), axis.ticks = element_blank(),
                  legend.position = c(0, 0.6), legend.title.align = 0))
# legend justification only works with short legend titles (only "Mean")

DIM <- 3e2

set.seed(3) # for consistent results

h_raster <-
  expand_grid(x = 1:DIM, y = 1:DIM) %>%
  mutate(x_100 = ceiling(x / 100),
         y_100 = ceiling(y / 100) - 1,
         x_10 = ceiling(x / 10),
         y_10 = ceiling(y / 10) - 1,
         offset_100 = sample(seq(1, 20, length.out = DIM^2/100))[x_100 + y_100 * 3],
         offset_10 = sample(seq(1, 30, length.out = DIM^2))[x_10 + y_10 * 30],
         z_1 = offset_100 + offset_10 + rnorm(n = DIM^2,
                                              sd = sqrt(2 * offset_100 + offset_10/4)),
         z_1 = z_1 - min(z_1)) %>%
  group_by(x_10, y_10) %>%
  mutate(z_10 = mean(z_1),
         var_1 = var(z_1),
         var.smooth_1 = var_1) %>%
  ungroup() %>%
  group_by(x_100, y_100) %>%
  mutate(z_100 = mean(z_1),
         var_10 = var(z_1),
         var.smooth_10 = var(z_10)) %>%
  ungroup() %>%
  mutate(var_100 = var(z_1),
         var.smooth_100 = var(z_100)) %>%
  pivot_longer(cols = c(z_1, z_10, z_100, var_1, var_10, var_100, var.smooth_1,
                        var.smooth_10, var.smooth_100),
             names_to = c('.value', 'k'), # produce 2 cols of values & send names to k
             names_pattern = "(.+)_(.+)") %>% # "to .value" _ "to k"
  mutate(k = if_else(k == 1, '1 cell', paste(k, 'cells')))

p <-
  ggplot(h_raster, aes(x, y, fill = z)) +
  coord_equal() +
  facet_wrap(~ k, strip.position = 'bottom') +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_distiller('Mean', type = 'seq', palette = 7, direction = 1,
                       breaks = range(h_raster$z), labels = c('low', 'high'))

p_s2 <-
  h_raster %>%
  mutate(k = case_when(k == '1 cell' ~ '10 cells',
                       k == '10 cells' ~ '100 cells',
                       k == '100 cells' ~ paste(DIM, 'cells'))) %>%
  ggplot(aes(x, y, fill = var)) +
  coord_equal() +
  facet_wrap(~ k, strip.position = 'bottom') +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_distiller('Heterogeneity', type = 'seq', palette = 1, direction = 1,
                       breaks = range(h_raster$var), labels = c('low', 'high'))

p_s2.s <-
  h_raster %>%
  mutate(k = case_when(k == '1 cell' ~ '10 cells',
                       k == '10 cells' ~ '100 cells',
                       k == '100 cells' ~ paste(DIM, 'cells'))) %>%
  ggplot(aes(x, y, fill = var.smooth)) +
  coord_equal() +
  facet_wrap(~ k, strip.position = 'bottom') +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_distiller(bquote(atop('Perceived', 'heterogeneity')), type = 'seq',
                       palette = 1, direction = 1, limits = range(h_raster$var),
                       breaks = range(h_raster$var), labels = c('low', 'high'))

p_full <- plot_grid(p + theme(legend.position = 'none'),
                    get_legend(p),
                    p_s2 + theme(legend.position = 'none'),
                    get_legend(p_s2),
                    p_s2.s + theme(legend.position = 'none'),
                    get_legend(p_s2.s),
                    grid::textGrob('Scale'),
                    NULL,
                    ncol = 2, rel_widths = c(10, 1), rel_heights = c(1, 1, 1, 0.15),
                    labels = c('a.', '', 'b.', '', 'c.', ''))

ggsave('figures/spatial-heterogeneity.png', plot = p_full, width = 6, height = 5.5,
       bg = 'white', scale = 1.4)
