# see https://rspatialdata.github.io/vegetation.html for example code and more info
#' install `rgeoboundaries` with `remotes::install_github('wmgeolab/rgeoboundaries')`
#' install `MODIStsp` with `remotes::install_github('ropensci/MODIStsp')`
#' `MODIStsp` version >= 2.0.7 from github fixes login issue due to 
library('rgeoboundaries') # for country/world boundaries
library('sf')             # for spatial features
library('MODIStsp')       # for downloading NDVI rasters
source('earthdata-login-info.R') # import personal login credentials for EarthData

# use a bounding box from tracking data ----
library('ctmm') # for movement data and models
data('buffalo')

bbox <-
  buffalo$Cilla %>%
  SpatialPolygonsDataFrame.telemetry() %>%
  st_as_sf() %>%
  st_transform(crs = '+proj=longlat') %>%
  st_bbox()

# download NDVI
MODIStsp(gui = FALSE, # do not use the browser GUI, only run in R
         out_folder = 'data/ndvi-rasters/buffalo-cilla/',
         selprod = 'Vegetation Indexes_16Days_250m (M*D13Q1)', # can't specify Terra here
         prod_version = '061', # 2022 raster version
         bandsel = 'NDVI', # Normalized Difference Vegetation Index layer only
         sensor = 'Terra', # only terrestrial values, ignore main bodies of water
         user = USERNAME, # your Earthdata username (for urs.earthdata.nasa.gov/home)
         password = PASSWORD, # your Earthdata password
         start_date = '2000.01.01', # earliest data is 2000-02-18
         end_date = format(Sys.Date(), '%Y.%m.%d'), # download all available data
         spatmeth = 'bbox', # see `spafile` argument
         bbox = bbox, # spatial file for raster extent
         out_projsel = 'User Defined', # use specified projection instead of default
         output_proj = '+proj=longlat', # download unprojected raster
         resampling = 'bilinear', # method for resampling raster if changing projection
         delete_hdf = TRUE, # delete HDF files after download is complete
         scale_val = TRUE, # convert from integers to floats within [-1, 1]
         ts_format = 'R RasterStack', # also save as an R RasterStack as an Rda file
         out_format = 'GTiff', # output format: 'ENVI' (.hdr) or 'GTiff' (.tif)
         n_retries = 10, # number of times to try again if download fails before aborting
         verbose = TRUE, # print processing messages
         parallel = TRUE) # use TRUE for automatic number of cores (max 8), or specify

# check rasters ----
if(FALSE) {
  library('raster') # to import and save rasters
  load('data/ndvi-rasters/buffalo-cilla/VI_16Days_250m_v61/Time_Series/RData/Terra/NDVI/MOD13Q1_NDVI_209_2005_337_2005_RData.RData')
  tel_poly <-
    buffalo$Cilla %>%
    SpatialPoints.telemetry() %>%
    adehabitatHR::mcp() %>% # MCPs aren't great, but they're good enough for a check...
    st_as_sf() %>%
    st_transform(crs = '+proj=longlat')
  raster_ts %>%
    mask(tel_poly) %>%
    crop(tel_poly) %>%
    plot()
}
