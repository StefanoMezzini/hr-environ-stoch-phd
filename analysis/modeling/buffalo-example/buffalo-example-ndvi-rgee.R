library('ctmm')     # for movement modeling
library('dplyr')    # for data wrangling
library('purrr')    # for functional programming
library('sf')       # for spatial features
library('sp')       # for spatial data
library('raster')   # for working with rasters
library('rgee') # for downloading NDVI
library('ggplot2')  # for fancy plots
# source('earthdata-login-info.R') # import personal login credentials for EarthData
theme_set(theme_bw()) # change default ggplot theme

#' run this if you just installed `rgee`; you may have to install `Miniconda`
#' see https://github.com/r-spatial/rgee for help
if(FALSE) rgee::ee_install()

# check if everything is ok
rgee::ee_check()

data('buffalo') # attach the buffalo data
cilla <- buffalo$Cilla # see ?buffalo for data sources

# download the NDVI raster data ----
# create an sf object of Cilla's range
extent(cilla)

boundary <-
  tibble(long = c(31.8, 32.0, 32.0, 31.8),
         lat = c(-25.2, -25.2, -24.8, -24.8)) %>%
  Polygon() %>%
  list() %>%
  Polygons(ID = '1') %>%
  list() %>%
  SpatialPolygons() %>%
  as.sf() %>%
  st_set_crs(value = cilla@info$projection)

plot(boundary, col = 'forestgreen')

# file path to save downloaded spatial file in
PATH <- 'data/ndvi-rasters/cilla'

# # save downloaded spatial file locally
# st_write(boundary, paste0(PATH, '/cilla-boundary.shp'))
# 
# # download NDVI
# MODIStsp_get_prodnames()[grepl('Indexes_16Days_250m', MODIStsp_get_prodnames())]
# 
# MODIStsp(gui = FALSE, # don't open GUI before processing
#          out_folder = PATH, # main output folder
#          out_folder_mod = PATH, # output folder (original HDF storage)
#          selprod = 'Vegetation Indexes_16Days_250m (M*D13Q1)', # terrestrial only
#          bandsel = 'NDVI', # Normalized Difference Vegetation Index MODIS layer
#          user = USERNAME, # your Earthdata username (see urs.earthdata.nasa.gov/home)
#          password = PASSWORD, # your Earthdata password
#          start_date = format(as.Date(min(cilla$timestamp)) - 16, # 16 days before start
#                              '%Y.%m.%d'), # yyyy.mm.dd format
#          end_date = format(as.Date(max(cilla$timestamp)) + 16, '%Y.%m.%d'),
#          verbose = TRUE, # print progress messages
#          spatmeth = 'file', # see `spafile` argument 
#          spafile = paste0(PATH, '/cilla-boundary.shp'), # spatial file for raster extent
#          out_format = 'GTiff') # output format: "ENVI" or "GTiff"
# 
# # create the dataset ----
# # subset the NDVI rasters by where Cilla was to account for movement over time
# NDVI_PATH <- 'data/ndvi-rasters/cilla/cilla-boundary/VI_16Days_250m_v6/NDVI/'
# cilla_ndvi <-
#   tibble(file = list.files(NDVI_PATH)) %>%
#   mutate(r = map(file, \(f) {
#     r <-
#       raster(paste0(NDVI_PATH, f)) %>%
#       rasterToPoints() %>%
#       as.data.frame() %>%
#       as_tibble()
#     colnames(r) <- c('x', 'y', 'ndvi')
#     r
#   })) %>%
#   tidyr::unnest(r) %>%
#   mutate(ndvi = ndvi / 1e6)
# 
# # convert cilla to a tibble for plotting
# cilla_tibble <- data.frame(cilla) %>% tibble()
# 
# ggplot() +
#   coord_equal() +
#   geom_path(aes(longitude, latitude, color = t), cilla_tibble) +
#   scale_color_viridis_c(NULL, option = 'C', breaks = range(cilla_tibble$t),
#                         labels = c('start', 'end'))
# 
# # join cilla and ndvi_tibble by position, keeping all values of cilla only
# left_join(cilla_tibble, cilla_ndvi)
