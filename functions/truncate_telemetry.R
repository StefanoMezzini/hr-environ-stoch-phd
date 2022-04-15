library('ctmm') # for continuous-time movement modeling

truncate_telemetry <- function(.tel, t_end_seconds) {
  .tel %>%
    as.data.frame() %>% # convert to data.frame for filtering
    as_tibble() %>% # convert to tibble for easier data wrangling
    filter(t <= t_end_seconds) # truncate the track to when the animal got full
}
