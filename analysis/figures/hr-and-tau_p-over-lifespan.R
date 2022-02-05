library('dplyr')        # for data wrangling
library('lubridate')    # makes working with dates smoother
library('adehabitatHR') # for minimum convex polygon HR estimation 
library('sp')           # for spatial objects
library('ggplot2')      # for fancy plots
library('cowplot')      # for multi-panel plots
theme_set(theme_bw())

# how much data do we need to estimate HR if taup approx. lifespan
set.seed(1)
N <- 4e3
d <- tibble(timestamp = seq(2012, 2022, length.out = N) %>% date_decimal(),
            t = seq(1/N, 1, length.out = N),
            location.long = sinpi(4 * t) * t^(-.01) + cumsum(rnorm(N, sd = .05)),
            location.lat = cospi(4 * t) * t^(-.01) + cumsum(rnorm(N, sd = .05)),
            individual.local.identifier = 1,
            outlier = FALSE)

plot_movement <- function(.data, .mcp) {
  p <-
    ggplot(.data, aes(location.long, location.lat)) +
    geom_polygon(aes(group = group), .mcp, alpha = 0.1, color = 'black') +
    geom_path(aes(color = t), alpha = 0.4) +
    coord_equal() +
    scale_x_continuous(NULL, breaks = NULL) +
    scale_y_continuous(NULL, breaks = NULL) +
    scale_color_viridis_c('Age', labels = c('birth', 'death'), breaks = range(d$t),
                          option = 'cividis', end = 0.85, direction = -1,
                          limits = range(d$t)) +
    theme(legend.position = 'none')
}

# create "boxes" for data in panels a and b
boxes <-
  bind_rows(d %>%
              filter(t < 0.07, t > 0) %>%
              summarize(xmin = min(location.long) - 0.1, xmax = max(location.long) + 0.1,
                        ymin = min(location.lat) - 0.1, ymax = max(location.lat) + 0.1)%>%
              mutate(g = 'a'),
            d %>%
              filter(t > 0.4, t < 0.6) %>%
              summarize(xmin = min(location.long) - 0.1, xmax = max(location.long) + 0.1,
                        ymin = min(location.lat) - 0.1, ymax = max(location.lat) + 0.1)%>%
              mutate(g = 'b'))

# calculate AKDE for each panel
fit_akde <- function(.data) {
  if(! any(grepl('telemetry', class(.data)))) .data <- as.telemetry(.data)
  svf <- variogram(.data)
  guess <- ctmm.guess(data = .data, variogram = svf, interactive = FALSE)
  models <- ctmm.select(.data, CTMM = guess, verbose = FALSE, cores = -1)
}

# calculate MCP for each panel
fit_mcp <- function(.data) {
  if(! any(grepl('telemetry', class(.data)))) .data <- as.telemetry(.data)
  
  .data %>%
    SpatialPoints.telemetry() %>%
    mcp(percent = 95) %>%
    spTransform(CRS("+proj=longlat")) %>%
    fortify() %>%
    rename(location.long = long, location.lat = lat) %>%
    suppressWarnings()
}

mcp_a <- fit_mcp(d %>% filter(t < 0.07, t > 0))
mcp_b <- fit_mcp(d %>% filter(t > 0.4, t < 0.6))
mcp_c <- bind_rows(mcp_a %>% mutate(group = paste(group, 'a')),
                   mcp_b %>% mutate(group = paste(group, 'b')),
                   fit_mcp(d) %>% mutate(group = paste(group = 'c')))

# full plot
full_p <-
  plot_grid(
    get_legend(plot_movement(d, mcp_c) + theme(legend.position = 'top')),
    plot_grid(
      plot_grid(plot_movement(filter(d, t < 0.07), mcp_a),
                plot_movement(filter(d, t > 0.4, t < 0.6), mcp_b),
                ncol = 1, labels = c('a.', 'b.')),
      plot_movement(d, mcp_c) +
        geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group = g),
                  boxes, fill = NA, color = '#00000030', alpha = 0.2, inherit.aes =FALSE),
      nrow = 1, labels = c(NA, 'c.')),
    ncol = 1, rel_heights = c(0.1, 1))

ggsave('figures/hr-and-lifespan.png', full_p, width = 5, height = 3, scale = 2,
       bg = 'white')
