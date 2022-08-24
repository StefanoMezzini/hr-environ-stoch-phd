# function to rescale value to [-1, 1] and rename each image based on the date
rescale_rename_raster <- function(image) {
  date <- ee$Date(image$get('system:time_start'))$format('YYYY')
  
  return(image$multiply(1e-4)$select('NDVI')$rename('date'))
}
