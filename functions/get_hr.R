#' function to get home range estimate starting from positional variance (`sigma`)
#' **NOTE:** only works when AKDE is isotropic and Gaussian!
get_hr <- function(.sigma, quantile) -2 * log(1 - quantile) * pi * .sigma
