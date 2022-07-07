# function that loops through a moving window of X days of tracking data
# computes estimated HR size and CIs for each window
# plots ML point estimate of HR size and 95% CIs
# written by Stefano Mezzini based on code by Mark Bidwell and Chris Fleming

library('ctmm')  # for continuous movement modeling
library('dplyr') # for data wrangling
library('purrr') # for functional programming

window_hr <- function(tel, window, dt, fig_path = NULL){
  
  # moving window created at the beginning of t; slides forward as far as possible:
  # |---|..... --> .|---|.... --> ..|---|... --> ...|---|.. --> ....|---|. --> .....|---|
  
  times <- seq(min(tel$t), max(tel$t) - window, by = dt)
  N <- length(times)
  
  out <-
    tibble(
      # add start and end times
      t_start = times, # left bound
      t_end = t_start + window, # left bound
      # subset times within window (t to t + window; can't use filter() bc telemetry)
      dataset = map2(t_start, t_end, \(t_1, t_2) tel[tel$t >= t_1 & tel$t <= t_2, ]),
      # find initial guesses for models
      guess = imap(dataset,
                   \(x, i){
                     cat('Fitting variogram ', i, ' of ', N, '.\n', sep = '')
                     return(ctmm.guess(data = x, interactive = FALSE))
                   }),
      # select best model based on subset of tel
      model = imap(dataset,
                   \(x, i){
                     cat('Fitting movement model ', i, ' of ', N, '.\n', sep = '')
                     return(try(ctmm.select(data = x, CTMM = guess[[i]])))
                   }),
      # estimate autocorrelated kernel density estimate
      akde = imap(dataset,
                  \(x, i){
                    cat('Fitting movement AKDE ', i, ' of ', N, '.\n', sep = '')
                    return(akde(data = x, CTMM = model[[i]]))
                  }),
      # find home range estimate
      hr_est =
        map_dbl(akde,
                \(a) summary(a, units = FALSE)$CI['area (square meters)', ]['est']) / 1e6,
      hr_lwr =
        map_dbl(akde,
                \(a) summary(a, units = FALSE)$CI['area (square meters)', ]['low']) / 1e6,
      hr_upr =
        map_dbl(akde,
                \(a) summary(a, units = FALSE)$CI['area (square meters)', ]['high']) /1e6,
      t_center = (t_start + t_end) / 2,
      posixct = as.POSIXct(t_center, origin = '1970-01-01', tz = tel@info$timezone),
      date = as.Date(posixct))
  
  # plot results
  library('ggplot2') # for fancy plots
  theme_set(theme_bw() + theme(legend.position = 'none'))
  
  plt_a <-
    ggplot(tel) +
    geom_point(aes(longitude, latitude, color = timestamp)) +
    scale_color_viridis_c() +
    labs(x = '', y = NULL)
  
  plt_b <-
    ggplot(out) +
    geom_errorbar(aes(date, ymin = hr_lwr, ymax = hr_upr), width = 0) +
    geom_point(aes(date, hr_est), size = 2) +
    geom_point(aes(date, hr_est, color = posixct)) +
    scale_x_date('Date', date_labels = '%b %Y') +
    scale_color_viridis_c('Date') +
    labs(y = expression(Home~range~(m^2)))
  
  plt <- cowplot::plot_grid(plt_a, plt_b, labels = c('a.', 'b.'), nrow = 1)
  
  if (! is.null(fig_path)) {
    # Save figure as a png using the animal's name
    ggsave(filename = file.path(fig_path, paste0(tel@info[1], '.png')),
           plot = plt, units = 'in', width = 7, height = 3,
           dpi = 600, bg = 'white')
  } else {
    print(plt)
  }
  return(out)
}

if(FALSE) {
  # Test the function on the buffalo data
  data('buffalo')
  
  # Apply to a single individual
  test <- window_hr(tel = buffalo$Queen, # buffalo with smallest dataset
                    window = 7 %#% 'day', # size of the moving window
                    dt = 1 %#% 'day', # step size for the moving window
                    fig_path = NULL) # can specify where to save the figure
  
  # apply to all buffaloes in the dataset
  results <- lapply(buffalo,
                    window_hr,
                    # arguments to pass to window_hr
                    window = 30 %#% 'day',
                    dt = 2 %#% 'day')
}
