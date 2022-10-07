setwd('/scratch/st-mnoonan-1/stefano/hr-environ-stoch-phd') # if in sockeye
library('sf')        # for simple feature objects
library('sp')        # for spatial objects
library('raster')    # for working with rasters
library('dplyr')     # for data wrangling (bind_rows(), mutate(), %>%, ...)
library('purrr')     # for functional programming (map(), etc.)
library('tidyr')     # for data wrangling (e.g., unnest(), pivot_*())
library('lubridate') # for smoother date wrangling

EPS <- .Machine$double.eps * 100 # constant to move values away from boundaries

# import NDVI data
files <- list.files(path = 'data/ndvi-rasters/world/nasa-rasters',
                    pattern = '.rds', full.names = TRUE)
d <-
  map(files,
      function(.file) { # map (apply) the function to each file
        out <- readRDS(.file) # import the data
        # change layer name to the date
        names(out) <-
          substr(x = .file,
                 start = nchar('data/ndvi-rasters/world/nasa-rasters/MOD_NDVI_16_x'),
                 stop = nchar(.file) - 4)
        return(out)
      }) %>%
  brick() %>% # convert all rasters to a single brick to speed up computations
  mask(st_transform(spData::world, '+proj=longlat')) # remove points not on land

for(i in seq_len(length(files))) {
  d[[i]] %>%
    rasterToPoints() %>% # convert to a matrix of x, y, and values
    data.frame() %>% # convert to a data.frame for easier referencing
    as_tibble() %>%
    pivot_longer(cols = -c(x, y)) %>% # change to x, y, column name, value format
    transmute(long = x, # rename coordinates
              lat = y,
              date = substr(x = name, start = 2, stop = nchar(name)) %>%
                gsub(pattern = '\\.', replacement = '-') %>% # replace "." with "-"
                date(),
              year = year(date), # extract year from date
              doy = yday(date) / 366, # extract day of year and rescale to (0, 1]
              ndvi = value, # rename the column
              ndvi_scaled = (ndvi + 1) / 2, # rescale from [-1, 1] to (0, 1)
              ndvi_scaled = if_else(ndvi_scaled == 0, EPS, ndvi_scaled),
              ndvi_scaled = if_else(ndvi_scaled == 1, ndvi_scaled - EPS, ndvi_scaled)) %>%
    saveRDS(paste0('data/ndvi-rasters/world/nasa-rasters/temp/',
                   substr(names(d)[[i]], 2, nchar(names(d)[[i]])), '.rds'))
  cat('Saved file', files[i], '\n')
}

map_dfr(list.files(path = 'data/ndvi-rasters/world/nasa-rasters/temp', pattern = '.rds',
                   full.names = TRUE), readRDS) %>%
  saveRDS('data/ndvi-rasters/world/nasa-rasters/all-nasa-rasters.rds')
