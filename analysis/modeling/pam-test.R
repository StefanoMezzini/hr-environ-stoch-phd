library('dplyr')     # for data wrangling
library('lubridate') # for working with dates smoothly
library('pammtools') # for modeling 
library('survival')  # for survival functions
library('ggplot2')   # for plotting

tapir <- readr::read_csv('~/GitHub/tapirs/data/cleaned/atlantica.csv',
                         col_types = 'dcddcld') %>%
  filter(individual.local.identifier == 'AF_01_JOANA',
         ! Outlier) %>%
  rename(long = location.long, lat = location.lat) %>%
  mutate(timestamp = as.POSIXct(timestamp),
         year = year(timestamp),
         doy = yday(timestamp) + hour(timestamp)/24 + minute(timestamp)/24/60,
         observed = 1) %>% # always observed
  select(-c(X, row, Outlier, individual.local.identifier)) %>%
  filter(year <= 1998)

# change data to PED format
tapir2 <- rename(tapir, day = doy)
tapir_ped <-
  as_ped(list(tapir, tapir2),
         formula = Surv(doy, observed) ~ year|concurrent(long, lat, tz_var = 'day')) %>% # censored or not
  tibble()
tapir_ped # coordinates not showing up

m <- pamm(ped_status ~
            s(tend, bs = 'cr', k = 25) +
            s(long, lat, bs = 'ds', k = 25),
          data = tapir_ped,
          method = 'REML',
          control = mgcv::gam.control(nthreads = 4))

layout(matrix(1:4, ncol = 2))
mgcv::gam.check(m)
layout(1)

plot(m, pages = 1, scheme = 2)

newd <-
  make_newdata(tapir_ped,
               tend = unique(tend)) %>%#,
               #location.long = seq_range(location.long, n = 10),
               #location.lat = seq_range(location.lat, n = 10)) %>%
  mutate(intlen = tend - tstart) %>%
  group_by(location.long, location.lat, tend) %>%
  add_surv_prob(object = m, ci = FALSE) %>%
  mutate(p = 1 - surv_prob)

# S(X) ranges don't make sense:
range(newd$surv_prob)

range(newd$p)

ggplot(newd, aes(location.long, location.lat, fill = p)) +
  facet_wrap(~ tend) +
  geom_tile()
