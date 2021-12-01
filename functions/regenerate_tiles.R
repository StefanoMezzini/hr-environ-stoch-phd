library('dplyr') # for data wrangling
library('tidyr') # for data wrangling

regenerate_tiles <- function() {
  # create a new raster of (potentially) regenerated food
  turns %>%
    filter(t == max(t)) %>%
    unnest(raster) %>%
    mutate(regenerating = if_else(food == 0, rgamma2(mu = 1, sigma2 = s2), 0),
           food = if_else(regenerating >= 10, # mean regen time == 10 turns
                          map2_dbl(mu, s2,
                                   \(x, y) rgamma2(mu = x, sigma2 = y, N = 1)),
                          food)) %>%
    select(- t) # not removing will cause naming conflicts
}
