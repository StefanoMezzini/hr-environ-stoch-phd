library('sp')      # for spatial points data
library('sf')      # required for filter(spData::world, ...)
library('raster')  # for raster data
library('dplyr')   # for data wrangling
library('ggplot2') # for fancy plots

setwd('~/Uni/2021-03/biol501/project-presentation')

theme_set(ggdark::dark_theme_void() +
            theme(plot.background = element_rect(color = '#000000'),
                  axis.text = element_blank()))

# bc polygon
can <- read_sf(dsn = '~/Uni/UofR/2020/202003/Geog203/GEOG203_MezziniStefano_1',
               layer = 'Canada') %>%
  st_transform(crs = CRS('+proj=longlat +datum=WGS84'))
bc <- filter(can, PRENAME == 'British Columbia')

# extract hfi raster for bc only
if(FALSE) {
  hfi <- raster('~/GitHub/tapirs/data/hfi-layers/ml_hfi_v1_2019.nc')
  crs(hfi) <- '+proj=longlat +datum=WGS84 +no_defs'
  
  hfi <-
    hfi %>%
    crop(bc) %>%        # crop to the extent to speed up computation 
    mask(bc) %>%        # crop to the multipolygon exactly
    rasterToPoints() %>% # convert to a spatial points object
    data.frame() %>%
    fortify() %>%
    rename(hfi = X__xarray_dataarray_variable__)
  
  saveRDS(hfi, 'hfi.rds')
} else {
  hfi <- readRDS('hfi.rds')
}

kelowna <- filter(hfi,
                  x > -119.6681, x < -119.3519,
                  y > 49.77667, y < 49.96722)


p_hfi <-
  ggplot() +
  geom_tile(aes(x, y, fill = hfi), hfi) +
  geom_sf(data = bc, color = 'white', fill = 'transparent', size = 0.05) +
  coord_sf() +
  theme(legend.position = c(0.85, 0.8), legend.direction = 'vertical',
        legend.text.align = 0.5) +
  scale_fill_viridis_c( 'ml-HFI', option = 'D', limits = c(0, 1),
                        breaks = c(0, 1))

ggsave('hfi-map.png', plot = p_hfi, width = 6.25, height = 5, dpi = 300)
beepr::beep() # notify when complete
