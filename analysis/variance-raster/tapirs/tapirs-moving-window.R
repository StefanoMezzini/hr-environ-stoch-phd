library('dplyr')   # for data wrangling
library('purrr')   # for functional programming
library('tidyr')   # for data wrangling
library('ggplot2') # for fancy plots
library('cowplot') # for fancy multi-panel plots
library('ctmm')    # for movement modeling
source('functions/window_hr.R') # function to calculate HRs and create figures
theme_set(theme_bw()) # change default theme

# import tapir data from https://github.com/StefanoMezzini/tapirs
tapirs <- readRDS('../tapirs/models/tapirs-final.rds') %>%
  select(region, name, name.short, data) %>%
  filter(region != 'atlantica') # Atlantic Forest uses VHF => irregular sampling

# check ranges of tracking data
tapirs <-
  tapirs %>%
  mutate(start = map_chr(data, \(d) min(d$timestamp) %>% as.character()),
         end = map_chr(data, \(d) max(d$timestamp) %>% as.character()),
         days = map_dbl(data, \(d) diff(range(d$timestamp))),
         clicks = map_dbl(data, nrow),
         daily_clicks = clicks / days)%>%
  arrange(days, clicks, desc(daily_clicks))
if(FALSE) {
  View(tapirs)
  
  # plot the tapir data
  tapirs %>%
    filter(region == 'cerrado') %>%
    mutate(data = map(data, data.frame)) %>% # convert to df for unnesting
    unnest(data) %>%
    slice(seq(1, n(), by = 10)) %>%
    ggplot() +
    facet_wrap(~ name, scales = 'free') +
    geom_path(aes(longitude, latitude), alpha = 0.05) +
    geom_point(aes(longitude, latitude, color = timestamp), alpha = 0.1) +
    scale_color_viridis_c('Date')
}

# find some good examples
# Cerrado ----
ce_proj <- '+proj=utm +zone=22 +datum=NAD83 +units=m'

cerrado <- filter(tapirs, region == 'cerrado')
select(cerrado, name, days, clicks, daily_clicks)

cerrado %>%
  filter(name.short %in% # filter to only a few good examples
           c('ANNA', 'DONALINA', 'KURUKA', 'LOU', 'ZEFA', 'ZACA', 'TITI', 'ZIGGY')) %>%
  pull(data) %>% # extract data column
  map(function(.d) {
    projection(.d) <- ce_proj # add projection based on mean long and lat
    cat('Running', .d@info$identity, '\n')
    .d <- window_hr(.d,
                    window = 7 %#% 'day', # 1 week of data for sufficient sample size
                    dt = 1 %#% 'day', # move window over by a single day each time
                    fig_path = 'figures/variance-raster/tapir-moving-window',
                    rds_path = 'models/tapir-moving-window',
                    cores = 1)
  })
