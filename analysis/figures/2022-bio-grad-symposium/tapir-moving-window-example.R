library('dplyr')     # for data wrangling
library('purrr')     # for functional programming
library('tidyr')     # for data wrangling
library('ctmm')      # for movement modeling
library('mgcv')      # for empirical Bayesian modeling
library('lubridate') # for smoother date wrangling
library('ggplot2')   # for fancy figures
library('cowplot')   # for multi-panel fancy figures

source('analysis/figures/default-figure-styling.R') # for color palettes

theme_set(theme_get() + theme(panel.grid = element_blank()))

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
anna <- readRDS('../tapirs/models/tapirs-final.rds') %>%
  filter(name.short == 'ANNA')
tel <- anna$data[[1]]

if(FALSE) {
  m_ouf <- anna$model[[1]]
  track <-
    predict(m_ouf, data = tel, dt = 100, complete = TRUE) %>%
    data.frame() %>%
    transmute(timestamp,
              long = longitude,
              lat = latitude) %>%
    mutate(dec_date = decimal_date(timestamp))
  
  saveRDS(track, 'models/tapir-moving-window/CE_31_ANNA-track-dt-100.rds')
} else {
  track <- readRDS('models/tapir-moving-window/CE_31_ANNA-track-dt-100.rds')
}

tel <- data.frame(tel) %>%
  rename(long = longitude, lat = latitude) %>%
  mutate(dec_date = decimal_date(timestamp))
tel <- bind_cols(tel, ndvi_preds(tel))

# check mean-variance relationship
tel %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarize(mu = mean(mu), sigma2 = mean(sigma2)) %>%
  ggplot(aes(mu, sigma2)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 4))

tapir <-
  readRDS('models/tapir-moving-window/CE_31_ANNA-window-7-days-dt-1-days.rds') %>%
  mutate(preds = map(dataset, \(.d) filter(tel, timestamp %in% .d$timestamp)),
         mu = map_dbl(preds, \(.d) mean(.d$mu)),
         sigma2 = map_dbl(preds, \(.d) mean(.d$sigma2))) %>%
  select(t_center, mu, sigma2, hr_est_95) %>%
  mutate(t_center = as.POSIXct(t_center, origin = '1970-01-01'))

# create the figure
date_labs <- range(tel$timestamp) %>% as.Date() # date labels
hr_lab <- expression(Home~range~size~(km^2))

p_mu <- ggplot(tapir, aes(t_center, mu)) +
  geom_line(color = pal[1], lwd = 1.5) +
  labs(x = NULL, y = 'Resource abundance')
p_s2 <- ggplot(tapir, aes(t_center, sigma2)) +
  geom_line(color = pal[2], lwd = 1.5) +
  labs(x = NULL, y = 'Resource unpredictability')
p_hr <- ggplot(tapir) +
  geom_line(aes(t_center, hr_est_95), color = pal[3], lwd = 1.5) +
  labs(x = NULL, y = hr_lab) +
  ylim(c(0, NA))

grobs <- lapply(list(p_mu, p_s2, p_hr), as_grob)

# align left margins of all plots
aligned_widths <- align_margin(lapply(grobs, function(x) {x$widths}), 'first')

# set the dimensions of plots to the aligned dimensions
for (i in seq_along(grobs)) {
  grobs[[i]]$widths <- aligned_widths[[i]]
}

p_tapir <-
  plot_grid(
    plot_grid(
      NULL,
      ggplot() +
        coord_equal() +
        geom_path(aes(long, lat), track, alpha = 0.3) +
        geom_point(aes(long, lat), tel, alpha = 0.3) +
        scale_fill_gradientn(NULL, colours = ndvi_pal, limits = c(-1, 1)) +
        scale_x_continuous(NULL, expand = c(0.05, 0)) +
        scale_y_continuous(NULL, expand = c(0.05, 0)), ncol = 1),
      plot_grid(plotlist = grobs, ncol = 1),
    ncol = 2)

ggsave('figures/2022-bio-grad-symposium/tapir-moving-window-examples.png',
       plot = p_tapir, height = 6, width = 12, dpi = 300, bg = 'white')

# regression plots ----
YLIMS <- c(0, 13)

reg_mu <-
  ggplot() +
  coord_cartesian(ylim = YLIMS) +
  geom_point(aes(mu, hr_est_95), tapir, alpha = 0.5, color = pal[3]) +
  geom_smooth(aes(mu, hr_est_95), tapir, color = pal[1], lwd = 1.5, se = FALSE,
              method = 'gam', formula = y ~ x,
              method.args = list(family = "Gamma")) +
  scale_x_continuous('Resource abundance') +
  scale_y_continuous(hr_lab, expand = c(0, 0))

# variance
reg_s2 <-
  ggplot() +
  coord_cartesian(ylim = YLIMS) +
  geom_point(aes(sigma2, hr_est_95), tapir, alpha = 0.5, color = pal[3]) +
  geom_smooth(aes(sigma2, hr_est_95), tapir, color = pal[2], lwd = 1.5,
              se = FALSE, method = 'gam', formula = y ~ x,
              method.args = list(family = "Gamma")) +
  scale_x_continuous('Resource unpredictability') +
  scale_y_continuous(hr_lab, expand = c(0, 0))

regs <- plot_grid(reg_mu, reg_s2, nrow = 1); regs

ggsave('figures/2022-bio-grad-symposium/tapir-regression-plots.png',
       plot = regs, width = 10, height = 4, dpi = 300, bg = 'white',
       scale = 0.75)
