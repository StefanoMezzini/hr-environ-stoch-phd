library('dplyr')     # for data wrangling
library('purrr')     # for functional programming
library('tidyr')     # for data wrangling
library('ctmm')      # for movement modeling
library('mgcv')      # for empirical Bayesian modeling
library('lubridate') # for smoother date wrangling
library('ggplot2')   # for fancy figures
library('cowplot')   # for fancy multi-panel plots

source('analysis/figures/default-figure-styling.R') # for color palettes

# import location-scale model for predictions of mean and variance in NDVI
m_ndvi <- readRDS('models/tapir-moving-window/CE_31_ANNA-mgcv-ndvi-gaulss.rds')

# function to make predictions from the model
ndvi_preds <- function(.data) {
  .data <- .data %>%
    data.frame() %>% # convert telemetry data to a data.frame
    mutate(year = year(timestamp),
           doy = yday(timestamp))
  
  predict.gam(m_ndvi, newdata = .data, type = 'response', se.fit = FALSE) %>%
    data.frame() %>%
    tibble() %>%
    transmute(mu = X1,
              sigma2 = (1/X2)^2) %>%
    return()
}

# import tapir moving window data
tapir <- readRDS('../tapirs/models/tapirs-final.rds') %>%
  filter(name.short == 'ANNA')
tel <- tapir$data[[1]]

if(FALSE) {
  m_ouf <- tapir$model[[1]]
  track <-
    predict(m_ouf, data = tel, dt = 100, complete = TRUE) %>%
    data.frame() %>%
    select(timestamp, longitude, latitude) %>%
    rename(long = longitude, lat = latitude) %>%
    mutate(dec_date = decimal_date(timestamp))
  
  saveRDS(track, 'models/tapir-moving-window/CE_31_ANNA-track-dt-100.rds')
} else {
  track <- readRDS('models/tapir-moving-window/CE_31_ANNA-track-dt-100.rds')
}

tel <- data.frame(tel) %>%
  rename(long = longitude, lat = latitude) %>%
  mutate(dec_date = decimal_date(timestamp))
tel <- bind_cols(tel, ndvi_preds(tel))

tapir <-
  readRDS('models/tapir-moving-window/CE_31_ANNA-window-7-days-dt-1-days.rds') %>%
  mutate(preds = map(dataset, \(.d) filter(tel, timestamp %in% .d$timestamp)),
         mu = map_dbl(preds, \(.d) mean(.d$mu)),
         sigma2 = map_dbl(preds, \(.d) mean(.d$sigma2))) %>%
  select(t_center, mu, sigma2, hr_lwr_50, hr_est_50, hr_upr_50, hr_lwr_95,
         hr_est_95, hr_upr_95) %>%
  pivot_longer(c(hr_lwr_50, hr_est_50, hr_upr_50, hr_lwr_95, hr_est_95,
                 hr_upr_95), names_to = c('.value', 'quantile'),
               names_pattern = '(.+)_(.+)') %>%
  mutate(t_center = as.POSIXct(t_center, origin = '1970-01-01'),
         quantile = paste0(quantile, '%'),
         quantile = factor(quantile))

# create the figure
date_labs <- range(tel$timestamp) %>% as.Date()

grobs <-
  lapply(
    list(
      ggplot() +
        coord_equal() +
        geom_path(aes(long, lat), track, alpha = 0.3) +
        geom_point(aes(long, lat, color = dec_date), tel) +
        scale_color_viridis_c(NULL, breaks = range(tel$dec_date),
                              labels = date_labs) +
        labs(x = NULL, y =NULL),
      ggplot(tapir, aes(t_center, mu)) +
        geom_line(color = pal[1]) +
        labs(x = NULL, y = '\U1D707(t)'),
      ggplot(tapir, aes(t_center, sigma2)) +
        geom_line(color = pal[2]) +
        labs(x = NULL, y = '\U1D70E\U00B2(t)'),
      ggplot(tapir) +
        geom_line(aes(t_center, hr_est, group = quantile, size = quantile),
                  color = pal[3], show.legend = FALSE) +
        geom_ribbon(aes(t_center, ymin = `50%`, ymax = `95%`),
                    tapir %>%
                      select(t_center, hr_est, quantile) %>%
                      pivot_wider(values_from = hr_est, names_from = quantile),
                    fill = pal[3], color = 'transparent', alpha = 0.2,
                    inherit.aes = FALSE) +
        labs(x = NULL, y = expression(Home~range~size~(km^2)~'   ')) +
        scale_size_manual(values = c(1, 0.5))),
    as_grob) # convert to grid graphical objects (grobs)

# align left margins of all plots
aligned_widths <- align_margin(lapply(grobs, function(x) {x$widths}), 'first')

# Setting the dimensions of plots to the aligned dimensions
for (i in seq_along(grobs)) {
  grobs[[i]]$widths <- aligned_widths[[i]]
}
# Draw aligned plots
p_left <- plot_grid(plotlist = grobs, ncol = 1, labels = paste0(letters, '.'))

# add estimated effects of E(R) and V(R) on home range size
YLIMS <- c(0, 13)

reg_mu <-
  ggplot(tapir) +
  coord_cartesian(ylim = YLIMS) +
  geom_point(aes(mu, hr_est, group = quantile, shape = quantile), alpha = 0.3,
             color = pal[3]) +
  geom_smooth(aes(mu, hr_est, group = quantile, size = quantile),
              color = pal[1], se = FALSE, method = 'gam',
              formula = y ~ x, method.args = list(family = "Gamma")) +
  scale_x_continuous('Resource abundance') +
  scale_y_continuous(expression(Home~range~size~(km^2)), expand = c(0, 0)) +
  scale_size_manual(values = c(1, 0.5)) +
  theme(legend.position = 'none')

# variance
reg_s2 <-
  ggplot(tapir) +
  coord_cartesian(ylim = YLIMS) +
  geom_point(aes(sigma2, hr_est, group = quantile, shape = quantile),
             alpha = 0.3, color = pal[3]) +
  geom_smooth(aes(sigma2, hr_est, group = quantile, size = quantile),
              color = pal[2], se = FALSE, method = 'gam',
              formula = y ~ x, method.args = list(family = "Gamma")) +
  scale_x_continuous('Resource unpredictability') +
  scale_y_continuous(expression(Home~range~size~(km^2)), expand = c(0, 0)) +
  scale_size_manual('Quantile', values = c(1, 0.5)) +
  scale_shape('Quantile') +
  theme(legend.position = 'none')

p_regs <- (ggplot(tapir) +
             geom_smooth(aes(sigma2, hr_est, group = quantile, size = quantile),
                         se = FALSE, method = 'gam', color = 'black',
                         formula = y ~ x, method.args = list(family = "Gamma")) +
             geom_point(aes(sigma2, hr_est, group = quantile, shape = quantile),
                        color = pal[3]) +
             scale_size_manual('Utilization quantile', values = c(1, 0.5)) +
             scale_shape('Utilization quantile') +
             theme(legend.position = 'top')) %>%
  get_legend() %>%
  plot_grid(reg_mu, reg_s2, ncol = 1, rel_heights = c(0.1, 1, 1),
            labels = c('', 'e.', 'f.'))

# group all the plots together
p <- plot_grid(p_left, p_regs)

ggsave('figures/variance-raster/tapir-example.png', p, scale = 2.1, height = 4,
       width = 6, dpi = 'retina', bg = 'white')
