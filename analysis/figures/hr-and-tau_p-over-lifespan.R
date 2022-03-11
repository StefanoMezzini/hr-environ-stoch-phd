library('dplyr')        # for data wrangling
library('tidyr')        # for data wrangling
library('ctmm')         # for movement modeling
library('lubridate')    # makes working with dates smoother
library('purrr')        # for functional programming (map_***())
library('sp')           # for spatial objects
library('ggplot2')      # for fancy plots
library('cowplot')      # for multi-panel plots
library('rphylopic')    # for tapir silhouette
theme_set(theme_bw())

# how much data do we need to estimate HR if the centroid moves over time?
tapir <- read.csv('../tapirs/data/cleaned/atlantica.csv') %>%
  filter(individual.local.identifier == 'AF_01_JOANA') %>%
  mutate(dec_date = decimal_date(date(timestamp)))

ggplot(tapir) +
  facet_wrap(~ year(timestamp), nrow = 1) +
  coord_equal() +
  geom_path(aes(location.long, location.lat))

crs <- as.telemetry(tapir)@info$projection # use as a common projection

# function to calculate AKDE for each panel
fit_akde <- function(.data) {
  fit <- ctmm.fit(data = .data)
  m <- ctmm.select(.data, CTMM = fit, verbose = FALSE)
  akde(data = .data, CTMM = m) %>%
    # need to specify CRS to have the same projection as the data
    SpatialPolygonsDataFrame.UD(level.UD = 0.95, proj4string = crs) %>%
    fortify() %>%
    as_tibble() %>%
    filter(grepl('est', id))
}

# create a separate dataset for each period
datasets <-
  tibble(start = c(1997.56, 1998.75),
         end = decimal_date(date_decimal(start) + 100 * 60^2 * 24),
         period = c('1', '2'),
         data = map2(start, end,
                     .f = \(s, e) filter(tapir, dec_date > s, dec_date < e)),
         tel = map(data,
                   \(x) as.telemetry(x,
                                     timeformat = '%Y-%m-%d %H:%M',
                                     projection = crs))) %>%
  bind_rows(tibble(start = min(tapir$dec_date),
                   end = max(tapir$dec_date),
                   period = 'Full dataset',
                   data = list(tapir),
                   tel = list(as.telemetry(data[[1]],
                                           timeformat = '%Y-%m-%d %H:%M',
                                           projection = crs)))) %>%
  mutate(data = map(tel, as_tibble))

akdes <- transmute(datasets,
                   period, # keep period column
                   color = case_when(period == '1' ~ 'goldenrod',
                                     period == '2' ~ 'dodgerblue',
                                     period == 'Full dataset' ~ 'black'),
                   hr = map(tel, fit_akde)) %>%
  unnest(hr)

datasets <- datasets %>%
  mutate(tel = map(tel, as_tibble)) %>%
  select(start, end,period, tel) %>%
  unnest(tel) %>%
  mutate(dec_date = decimal_date(timestamp))

ggplot() +
  facet_wrap(~ period) +
  coord_equal() +
  geom_polygon(aes(long, lat, group = group), akdes, alpha = 0.3, fill = 'red') +
  geom_point(aes(x, y), datasets, alpha = 0.3) +
  geom_path(aes(x, y), datasets, alpha = 0.3) +
  scale_color_continuous('Date')

plot_movement <- function(.period) {
  .data <- filter(datasets, period == .period[[1]])
  .akde <- filter(akdes, period %in% .period) %>%
    mutate(group = paste(group, period))
  
  .breaks <- c(floor(min(tapir$dec_date)),
               ceiling(max(tapir$dec_date)))
  
  p <-
    ggplot() +
    coord_equal() +
    geom_path(aes(x, y, color = dec_date), .data, alpha = 0.5) +
    scale_x_continuous(NULL, breaks = NULL) +
    scale_y_continuous(NULL, breaks = NULL) +
    scale_color_viridis_c('Year', breaks = c(1997, 2000.5), labels = c('1997', '2000.5'),
                          option = 'cividis', end = 0.85, direction = -1,
                          limits = c(1997, 2000.5)) +
    theme(legend.position = 'none')
  
  # add each AKDE separately and with different colors
  for(i in rev(unique(.akde$color))) {
    p <- p + geom_polygon(aes(long, lat, group = group),
                          filter(.akde, color == i), alpha = 0.1, color = i, lwd = 1)
  }
  p
}

# create "boxes" for data in panels a and b
boxes <-
  bind_rows(akdes %>%
              filter(period == '1') %>%
              summarize(xmin = min(long) - 50, xmax = max(long) + 50,
                        ymin = min(lat) - 50, ymax = max(lat) + 50) %>%
              mutate(group = 'a'),
            akdes %>%
              filter(period == '2') %>%
              summarize(xmin = min(long) - 50, xmax = max(long) + 50,
                        ymin = min(lat) - 50, ymax = max(lat) + 50) %>%
              mutate(group = 'b'))

# tapir silhouette ----
silhouette <- image_data('8f6b8802-52f9-4f16-8429-0b86ea4a4aa8', size = '512')[[1]]

p_tapir <- ggplot() +
  add_phylopic(silhouette, alpha = 1, x = 0.5, y = 0.5, ysize = 0.9) +
  theme_void(); p_tapir

# full plot
p_full <-
  plot_grid(
    plot_grid(get_legend(plot_movement('Full dataset') + theme(legend.position = 'top')),
              p_tapir),
    plot_grid(
      plot_grid(plot_movement('1'),
                plot_movement('2'),
                labels = c('a.', 'b.'), nrow = 1, rel_widths = c(4, 6)),
      plot_movement(c('Full dataset', '1', '2')) +
        geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group = group),
                  boxes, fill = NA, color = '#00000050', alpha = 0.2, inherit.aes =FALSE),
      nrow = 2, labels = c(NA, 'c.')),
    ncol = 1, rel_heights = c(0.15, 1))

ggsave('figures/hr-and-lifespan.png', p_full, width = 3, height = 3, scale = 2,
       bg = 'white')
