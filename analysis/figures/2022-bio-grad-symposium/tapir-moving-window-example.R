library('dplyr')     # for data wrangling
library('purrr')     # for functional programming
library('tidyr')     # for data wrangling
library('ctmm')      # for movement modeling
library('mgcv')      # for empirical Bayesian modeling
library('lubridate') # for smoother date wrangling
library('ggplot2')   # for fancy figures
library('cowplot')   # for multi-panel fancy figures

source('analysis/figures/default-figure-styling.R') # for color palettes

# import location-scale model for predictions of mean and variance in NDVI
m_ndvi <- readRDS('models/tapir-moving-window/CE_31_ANNA-mgcv-ndvi-gulss.rds')

# function to make predictions from the model
ndvi_preds <- function(.data) {
  .data <- .data %>%
    data.frame() %>% # convert telemetry data to a data.frame
    mutate(year = year(timestamp),
           doy = yday(timestamp))
  
  predict.gam(m_ndvi, newdata = .data, type = 'link', se.fit = FALSE) %>%
    data.frame() %>%
    tibble() %>%
    transmute(mu = m_ndvi$family$linfo[[1]]$linkinv(X1),
              sigma2 = m_ndvi$family$linfo[[2]]$linkinv(X2)) %>%
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
    rename(long = longitude, lat = latitude)
  track <- track %>%
    mutate(dec_year = decimal_date(timestamp)) %>%
    bind_cols(ndvi_preds(track))
  
  saveRDS(track, 'models/tapir-moving-window/CE_31_ANNA-track-dt-100.rds')
} else {
  track <- readRDS('models/tapir-moving-window/CE_31_ANNA-track-dt-100.rds')
}

tel <- data.frame(tel) %>%
  rename(long = longitude, lat = latitude) %>%
  mutate(year = decimal_date(timestamp),
         doy = yday(timestamp))
tel <- bind_cols(tel, ndvi_preds(tel))

# check mean-variance relationship
track %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarize(mu = mean(mu), sigma2 = mean(sigma2)) %>%
  ggplot() +
  geom_point(aes(mu, sigma2), alpha = 0.3)

tapir <-
  readRDS('models/tapir-moving-window/CE_31_ANNA-window-7-days-dt-1-days.rds') %>%
  mutate(preds = map(dataset, \(.d) filter(tel, timestamp %in% .d$timestamp)),
         mu = map_dbl(preds, \(.d) mean(.d$mu)),
         sigma2 = map_dbl(preds, \(.d) mean(.d$sigma2))) %>%
  select(t_center, mu, sigma2, hr_est_95) %>%
  mutate(t_center = as.POSIXct(t_center, origin = '1970-01-01'))

# calculate median NDVI for background
newd <- expand_grid(long = seq(min(tel$long), max(tel$long), length.out = 100),
                    lat = seq(min(tel$lat), max(tel$lat), length.out = 100),
                    year = 0, doy = 0) # year and doy are ignored, but still needed

preds <-
  bind_cols(select(newd, long, lat),
            predict.gam(object = m_ndvi, newdata = newd, type = 'link', se.fit = FALSE,
                        terms = c('s(long,lat)', 's.1(long,lat)')) %>%
              data.frame() %>%
              transmute(mu = m_ndvi$family$linfo[[1]]$linkinv(X1)))

# create the figure
date_labs <- range(tel$timestamp) %>% as.Date() # date labels

p_tapir <-
  plot_grid(
    plot_grid(
      NULL,
      ggplot() +
        coord_equal() +
        # geom_raster(aes(long, lat, fill = mu), preds, show.legend = FALSE) +
        geom_path(aes(long, lat), track, alpha = 0.3) +
        geom_point(aes(long, lat), tel, alpha = 0.3) +
        scale_fill_gradientn(NULL, colours = ndvi_pal, limits = c(-1, 1)) +
        scale_x_continuous(NULL, expand = c(0.05, 0)) +
        scale_y_continuous(NULL, expand = c(0.05, 0)), ncol = 1),
    plot_grid(
      ggplot(tapir, aes(t_center, mu)) +
        geom_line(color = pal[1], lwd = 1.5) +
        labs(x = NULL, y = 'Resource abundance'),
      ggplot(tapir, aes(t_center, sigma2)) +
        geom_line(color = pal[2], lwd = 1.5) +
        labs(x = NULL, y = 'Resource unpredictability'),
      ggplot(tapir) +
        geom_line(aes(t_center, hr_est_95), color = pal[3], lwd = 1.5) +
        labs(x = NULL, y = 'Spatial needs (km)') +
        ylim(c(0, NA)),
      ncol = 1), ncol = 2)

ggsave('figures/2022-grad-symposium/tapir-moving-window-examples.png', plot = p_tapir,
       height = 6, width = 12, dpi = 300, bg = 'white')

# main relationships ---
# fit a model
m <- gam(list(hr_est_95 ~ s(mu, k = 4, bs = 'tp') + s(sigma2, k = 4, bs = 'tp') +
                ti(mu, sigma2, k = 4, bs = 'tp'),
              ~ s(mu, k = 4, bs = 'tp') + s(sigma2, k = 4, bs = 'tp')),
         family = gammals(),
         data = tapir,
         method = 'REML',
         control = gam.control(nthreads = 4))

newd <- tibble(mu = seq(min(tapir$mu), max(tapir$mu), length.out = 400),
               sigma2 = seq(min(tapir$sigma2), max(tapir$sigma2), length.out = 400))
preds <- mutate(newd,
                mu_hr = predict(m, newdata = newd, se.fit = FALSE,
                                terms = 's(mu)', type = 'response')[, 1],
                sigma2_hr = predict(m, newdata = newd, se.fit = FALSE,
                                    terms = 's(sigma2)', type = 'response')[, 1])

# regression plots ----
reg_mu <-
  ggplot() +
  geom_point(aes(mu, hr_est_95), tapir, alpha = 0.1) +
  geom_line(aes(mu, mu_hr), preds, color = pal[1], lwd = 1.5) +
  scale_x_continuous('Resource abundance') +
  scale_y_continuous('Home range size (km)')

reg_s2 <-
  ggplot() +
  geom_point(aes(sigma2, hr_est_95), tapir, alpha = 0.1) +
  geom_line(aes(sigma2, sigma2_hr), preds, color = pal[2], lwd = 1.5) +
  scale_x_continuous('Resource unpredictability', limits = c(NA, 22.51)) +
  scale_y_continuous('Home range size (km)')

regs <- plot_grid(reg_mu, reg_s2, nrow = 1)

ggsave('figures/2022-grad-symposium/tapir-regression-plots.png',
       plot = regs, width = 10, height = 4, dpi = 300, bg = 'white', scale = 0.75)
