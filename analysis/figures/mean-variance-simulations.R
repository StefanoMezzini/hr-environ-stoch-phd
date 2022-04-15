library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling (expand_grid())
library('ggplot2') # for fancy plots
library('cowplot') # for multi-panel plots
library('purrr')   # for functional programming
library('future')  # for parallel computing
library('furrr')   # for parallel functional programming
source('analysis/default-figure-styling.R') # for a consistent figure theme
source('functions/mean-variance-trends.R')  # functions to generate means and variances
source('functions/rgamma2.R')               # rgamma() parameterized by mean and variance
source('functions/qgamma2.R')               # qgamma() parameterized by mean and variance
source('analysis/figures/mean-variance-trends-panel-data.R') # create tibble of parameters

# see 'analysis/figures/mean-variance-trends.R' for reference figures
N <- 200 # number of animals
REPS <- 5e3 # number of replicate runs ("days")
required <- 50 # amount required for satiety
MAX_VISITS <- 200 # max number of visits calculated
types <- c('constant', 'linear', 'cyclical', 'drifting', 'erratic')


# plot means and variances
plot_grid(ggplot(d55) +
            facet_grid(mean ~ ., switch = 'y') +
            geom_line(aes(t, mu), color = pal[1], lwd = 1),
          ggplot(d55) +
            facet_grid(variance ~ ., switch = 'y') +
            geom_line(aes(t, sqrt(sigma2)), color = pal[2], lwd = 1))

# plot coefficient of variation = SD / mean
p_cv <-
  ggplot(d55) +
  facet_grid(variance ~ mean, switch = 'y') +
  geom_line(aes(t, sqrt(sigma2)/mu)) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(expression(Coefficient~of~variation~'in'~italic(H)),
                     breaks = NULL); p_cv

#' model assumes:
#' - infinite behavioral plasticity, 
#' - instantaneous response times,
#' - 
eat <- function(mu, sigma2, reps = REPS) {
  x <-
    expand_grid(replicate = 1:reps, n_visits = 1:MAX_VISITS) %>% # initial tibble
    group_by(replicate) %>% # do the following for each replicate independently
    # generate rgamma2() resources for each row, then sum cumulatively
    mutate(intake = rgamma2(N = n(), mu = mu, sigma2 = sigma2) %>% cumsum()) %>%
    filter(intake > required) %>% # remove all visits before animal reached satiety
    assertr::verify(nrow(.) > 0) %>% # ensure there's at least one row
    filter(intake == min(intake)) %>% # take the first (i.e., smallest) visit count
    ungroup() # remove grouping by replicate
  
  return(x)
}

if(FALSE) {
  print(Sys.time()) # print time at the beginning of the computation
  plan(multisession, workers = 4) # ~ 21 minutes
  
  #' use `furrr_options(seed = NULL)` to ensure statistically sound simulations
  tictoc::tic() # to measure computation time
  d <- d55 %>%
    mutate(visits = future_map2(.x = mu, .y = sigma2, .f = eat,
                                .options = furrr_options(seed = NULL))) %>%
    unnest(visits)
  print(tictoc::toc()); beepr::beep(sound = 2) # notify when calculations are complete
  plan(sequential) # back to sequential computing to avoid crashes
  saveRDS(d, 'data/mean-variance-simulations-2e4-reps.rds') # save simulations
} else {
  d <- readRDS('data/mean-variance-simulations-2e4-reps.rds')
}

# check if any simulation was unable to finish
if(any(d$intake == required)) {
  stop(paste0(sum(d$intake < required), ' (', mean(d$intake < required),
              '%) of animals did *NOT* reach satiety!'))
} else {cat('All animals reached satiety.')}

# summarize simulations as mean (0.5) and 95% HR
d_summarized <-
  d %>%
  group_by(mean, variance, t, mu, sigma2) %>%
  summarize(mean_visits = mean(n_visits),
            median_visits = median(n_visits),
            sd_visits = sd(n_visits),
            upr = quantile(n_visits, 0.95),
            .groups = 'drop')

# simulation figure
hr_lab <- expression(Home~range~size~(italic(H)))
p_sim_55 <-
  ggplot(d_summarized) +
  facet_grid(variance ~ mean, scales = 'free_y') +
  geom_ribbon(aes(t, ymin = mean_visits, ymax = upr), fill = pal[3], alpha = 0.2) +
  geom_line(aes(t, mean_visits), color = pal[3], lwd = 1) +
  geom_line(aes(t, upr), color = pal[3]) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(hr_lab, breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

# trends in the mean
p_mean <-
  ggplot(d55, aes(t, mu)) +
  facet_grid(. ~ mean) +
  geom_line(color = pal[1], lwd = 1) +
  scale_x_continuous(expression(Mean~italic(U)), breaks = NULL, position = 'top') +
  scale_y_continuous(expression(italic(U)), breaks = NULL) +
  theme(strip.background = element_blank(), axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(colour = 'transparent'))

# trends in the variance
p_variance <-
  ggplot(d55, aes(t, sigma2)) +
  facet_grid(variance ~ ., switch = 'y') +
  geom_line(color = pal[2], lwd = 1) +
  scale_x_continuous('', breaks = NULL) +
  scale_y_continuous(expression(Variance~'in'~italic(U)), breaks = NULL) +
  theme(strip.background = element_blank(), axis.title = element_text(face = 'bold'))

plot_grid(plot_grid(NULL, p_mean, rel_widths = c(1, 4.4), nrow = 1),
          plot_grid(p_variance, p_sim_55, rel_widths = c(1, 4.5), nrow = 1),
          rel_heights = c(1, 4), nrow = 2)

ggsave('figures/mean-variance-5-by-5-sims.png', width = 8, height = 4.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# figure of SD(visits)
p_sd <-
  ggplot(d_summarized) +
  facet_grid(variance ~ mean) +
  geom_area(aes(t, sd_visits), color = 'black', alpha = 0.3) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(expression(Standard~deviation~of~italic(H)), breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

plot_grid(plot_grid(NULL, p_mean, rel_widths = c(1, 4.3), nrow = 1),
          plot_grid(p_variance, p_sd, rel_widths = c(1, 4.5), nrow = 1),
          rel_heights = c(1, 4), nrow = 2)
ggsave('figures/mean-variance-5-by-5-sd-visits.png', width = 8, height = 4.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# plot coefficient of variation
plot_grid(plot_grid(NULL, p_mean, rel_widths = c(1, 4.5), nrow = 1),
          plot_grid(p_variance, p_cv + theme(strip.background = element_blank(),
                                         strip.text = element_blank()),
                    rel_widths = c(1, 4.5), nrow = 1),
          rel_heights = c(1, 4), nrow = 2)
ggsave('figures/mean-variance-5-by-5-cv.png', width = 8, height = 4.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# (constant, constant) panel
filter(d_summarized, mean == 'constant', variance == 'constant') %>%
  ggplot() +
  facet_grid(mean ~ variance, scales = 'free_y') +
  geom_ribbon(aes(t, ymin = mean_visits, ymax = upr), fill = pal[3], alpha = 0.2) +
  geom_line(aes(t, mean_visits), color = pal[3], lwd = 1) +
  geom_line(aes(t, upr), color = pal[3]) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous('Home range size', breaks = NULL, limits=c(0,100))+
  theme(strip.background = element_blank(), strip.text = element_blank())

ggsave('figures/c-c-hr.png', width = 3, height = 1.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# (linear, constant) panel
filter(d_summarized, mean == 'linear', variance == 'constant') %>%
  ggplot() +
  facet_grid(mean ~ variance, scales = 'free_y') +
  geom_ribbon(aes(t, ymin = mean_visits, ymax = upr), fill = pal[3], alpha = 0.2) +
  geom_line(aes(t, mean_visits), color = pal[3], lwd = 1) +
  geom_line(aes(t, upr), color = pal[3]) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous('Home range size', breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

ggsave('figures/l-c-hr.png', width = 3, height = 1.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# (linear, linear) panel
filter(d_summarized, mean == 'linear', variance == 'linear') %>%
  ggplot() +
  facet_grid(mean ~ variance, scales = 'free_y') +
  geom_ribbon(aes(t, ymin = mean_visits, ymax = upr), fill = pal[3], alpha = 0.2) +
  geom_line(aes(t, mean_visits), color = pal[3], lwd = 1) +
  geom_line(aes(t, upr), color = pal[3]) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous('Home range size', breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

ggsave('figures/l-l-hr.png', width = 3, height = 1.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')
