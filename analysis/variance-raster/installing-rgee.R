#' see `https://github.com/r-spatial/rgee` for more info or if you use `Python`
#' install the latest CRAN version...
install.packages('rgee') # using github version

#' ... or install the developmental version from GitHub (requires the `remotes` package)
remotes::install_github('r-spatial/rgee') # using version >= 1.1.3.9000

#' install Python packages: `numpy` and `ee`
rgee::ee_install()

#' Sync `rgee` with other `Python` packages
#' Integrate `rgee` with `geemap`.
library('reticulate')
library('rgee')

#' initialize the `Python` environment
ee_Initialize()

#' install `geemap` in the same `Python` environment as `rgee`
py_install('geemap')
gm <- import('geemap')

# Check non-R dependencies
rgee::ee_check()
