library('dplyr') # for data wrangling (e.g., %>%) and tibble()

# mean functions
mean_constant <- function(generations) {
  tibble(mu = rep(10, length(generations))) %>%
    list() %>%
    return()
}

mean_linear <- function(generations) {
  tibble(mu = generations * 0.045 + 6) %>%
    list() %>%
    return()
}

mean_cyclical <- function(generations) {
  tibble(mu = sin(generations / 10) * 4.5 + 10.5) %>%
    list() %>%
    return()
}

mean_drifting <- function(generations, seed = 8) {
  set.seed(seed = seed)
  tibble(mu = cumsum(rnorm(generations, sd = 0.5))) %>%
    mutate(mu = mu - min(mu) + 6) %>%
    list() %>%
    return()
}

mean_erratic <- function(generations) {
  tibble(mu = case_when(generations < quantile(generations, 0.25) ~ 7,
                        generations < quantile(generations, 2/3) ~ 3,
                        TRUE ~ 10) + 5) %>%
    list() %>%
    return()
}

# variance functions
variance_constant <- function(generations) {
  tibble(sigma2 = rep(25, length(generations))) %>%
    list() %>%
    return()
}

variance_linear <- function(generations) {
  tibble(sigma2 = generations * 0.2 + 10) %>%
    list() %>%
    return()
}

variance_cyclical <- function(generations) {
  tibble(sigma2 = (sin(generations / 15) + 1)^3 * 5 + 10) %>%
  list() %>%
  return()
}

variance_drifting <- function(generations, seed = 1) {
  set.seed(seed)
  tibble(sigma2 = cumsum(rnorm(length(generations), sd = 2.5)) %>% abs() + 10) %>%
    list() %>%
    return()
}

variance_erratic <- function(generations) {
  tibble(sigma2 = case_when(generations < quantile(generations, 0.2) ~ 35,
                            generations < quantile(generations, 0.6) ~ 50,
                            TRUE ~ 15)) %>%
  list() %>%
  return()
}

# check ranges of functions
if(FALSE) {
  mean_constant(1:200)[[1]]$mu %>% unique()
  mean_linear(c(1, 200))[[1]]$mu
  mean_cyclical(1:200)[[1]]$mu %>% range()
  mean_drifting(1:200)[[1]]$mu %>% range()
  mean_erratic(1:200)[[1]]$mu %>% range()
  
  variance_constant(1:200)[[1]]$sigma2 %>% unique()
  variance_linear(c(1, 200))[[1]]$sigma2
  variance_cyclical(1:200)[[1]]$sigma2 %>% range()
  variance_drifting(1:200)[[1]]$sigma2 %>% range()
  variance_erratic(1:200)[[1]]$sigma2 %>% range()
}
