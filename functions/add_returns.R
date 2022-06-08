library('ctmm')  # for movement modeling
library('dplyr') # for data wrangling

add_returns <- function(.tel, t_end_seconds, .model = model) {
  # exploration path to reach satiety
  expl <- .tel[.tel$t <= t_end_seconds, ]
  # start and end of exploration path (needed for return)
  ends <- expl[c(1, nrow(expl)), ]
  # convert `expl` to data.frame to be bound to other data
  expl <- data.frame(expl)
  # add a return to the exploration so the animal loops back
  loop <-
    # return takes as long as exploration; same sampling frequency
    simulate(object = .model, data = ends, t = unique(expl$t)) %>%
    # convert for data wrangling
    data.frame() %>%
    # convert for ease of use
    tibble() %>%
    # reverse time so animal returns home instead of exploring
    mutate(t = rev(t) + max(ends$t), path = 'return') %>%
    # add the exploration track
    bind_rows(mutate(expl, path = 'exploration')) %>%
    # arrange by time so exploration comes before return
    arrange(t)
  return(loop)
}