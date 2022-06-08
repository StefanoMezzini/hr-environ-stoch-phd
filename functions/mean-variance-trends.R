library('dplyr') # for data wrangling (e.g., %>%) and tibble()

# mean functions
min_mu <- 10
max_mu <- 100

mean_constant <- function(generations) {
  rep(mean(c(min_mu, max_mu)), length(generations))
}

mean_linear <- function(generations) {
  b <- (max_mu - min_mu) / (200 - 1)
  
  generations * b + min_mu - b
}

mean_cyclical <- function(generations) {
  sin(generations / 10) * (max_mu - min_mu) / 2 + mean(c(min_mu, max_mu))
}

mean_drifting <- function(generations, seed = 8) {
  set.seed(seed = seed)
  mu <- cumsum(rnorm(length(generations)))
  mu <- mu - min(mu) # set minimum to 0
  mu <- mu / max(mu) # shrink the range to [0, 1]
  mu <- mu * (max_mu - min_mu) # scale to the range of mu
  mu <- mu + min_mu # shift to the right intercept
  return(mu)
}

mean_erratic <- function(generations) {
  case_when(generations < quantile(generations, 0.25)~mean(c(min_mu, max_mu)),
                        generations < quantile(generations, 2/3) ~ min_mu,
                        TRUE ~ max_mu)
}

# variance functions
min_s2 <- 5
max_s2 <- 200

variance_constant <- function(generations) {
  rep(mean(c(min_s2, max_s2)), length(generations))
}

variance_linear <- function(generations) {
  b <- (max_s2 - min_s2) / (200 - 1)
  
  generations * b + min_s2 - b
}

variance_cyclical <- function(generations) {
  ((sin(generations / 15) + 1)/2)^3 * (max_s2 - min_s2) + min_s2
}

variance_drifting <- function(generations, seed = 1) {
  set.seed(seed)
  sigma2 <- length(generations) %>%
    rnorm() %>%
    cumsum() %>%
    abs()
  sigma2 <- sigma2 - min(sigma2) # ranges [0, Inf)
  sigma2 <- sigma2 / max(sigma2) # to [0, 1]
  sigma2 <- sigma2 * (max_s2 - min_s2) + min_s2 # to [min_s2, max_s2]
  return(sigma2)
}

variance_erratic <- function(generations) {
  case_when(generations < quantile(generations, 0.2) ~ (max_s2 + min_s2) / 1.75,
                     generations < quantile(generations, 0.6) ~ max_s2,
                     TRUE ~ min_s2)
}

# check ranges of functions
if(FALSE) {
  mean_constant(1:200) %>% unique()
  mean_linear(c(1, 200))
  mean_cyclical(1:200) %>% range()
  mean_drifting(1:200) %>% range()
  mean_erratic(1:200) %>% range()
  
  variance_constant(1:200) %>% unique()
  variance_linear(c(1, 200))
  variance_cyclical(1:200) %>% range()
  variance_drifting(1:200) %>% range()
  variance_erratic(1:200) %>% range()
}
