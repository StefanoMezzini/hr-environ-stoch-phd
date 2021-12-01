# re-parameterize rgamma() as a function of mean and variance
rgamma2 <- function(mu, sigma2, N = n()) {
  # mean = k * theta
  # sigma^2 = k * theta^2
  
  theta <- sigma2 / mu # scale parameter
  k <- mu / theta # shape parameter
  
  rgamma(n = N, shape = k, scale = theta)
}
