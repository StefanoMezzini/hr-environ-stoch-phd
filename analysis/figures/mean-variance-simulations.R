library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling (expand_grid())
library('ggplot2') # for fancy plots
library('cowplot') # for multi-panel plots
library('purrr')   # for functinal programming
library('future')  # for parallel computing
library('furrr')   # for parallel functional programming
source('functions/rgamma2.R') # rgamma() parameterized by mean and variance
source('analysis/default-figure-styling.R') # for default figure styling and colors
theme_set(theme_bw())

# see 'analysis/figures/mean-variance-trends.R' for reference figures
N <- 200 # number of generations
REPS <- 2e4 # number of replicate runs
required <- 1e3 # amount required for satiety
types <- c('constant', 'linear', 'cyclical', 'drifting', 'erratic')
set.seed(8) # set seed for consistent values in d55

# create a tibble of parameters
d55 <-
  expand_grid(mean = types,
              variance = types,
              t = list(tibble(t = 1:N))) %>%
  mutate(
    mu = case_when(mean == 'constant' ~ list(tibble(mu = 0)),
                   mean == 'linear' ~ list(tibble(mu = seq(-20, 20, length.out = N))),
                   mean == 'cyclical' ~ list(tibble(mu = sin(1:N / 10) * 20)),
                   mean == 'drifting' ~ list(tibble(mu = cumsum(rnorm(N, sd = 2.25))) %>%
                                               mutate(mu = mu - mean(mu))),
                   mean == 'erratic' ~ list(tibble(mu = (-2)^(round(1:N/80)+2) * 2.2))),
    sigma2 =
      case_when(variance == 'constant' ~ list(tibble(sigma2 = 450)),
                variance == 'linear' ~ list(tibble(sigma2 = seq(0, 900, length.out = N))),
                variance == 'cyclical' ~ list(tibble(sigma2 = sin(1:N/15) + 1)^3 *100+50),
                variance == 'drifting' ~ list(tibble(sigma2 = cumsum(rnorm(N, sd=50)) %>%
                                                       abs())),
                variance == 'erratic' ~ tibble(sigma2=((-2)^(round(1:N/80))+4)*100)%>%
                  list())) %>%
  unnest(c(t, mu, sigma2)) %>%
  mutate(mu = mu + 50,
         sigma2 = sigma2 * 16,
         mean = factor(mean, levels = types),
         variance = factor(variance, levels = types))

# plot means and variances
plot_grid(ggplot(d55) + facet_grid(mean ~ .) + geom_line(aes(t, mu)),
          ggplot(d55) + facet_grid(variance ~ .) + geom_line(aes(t, sqrt(sigma2))))

# plot coefficient of variation = mean / SD
p_cv <-
  ggplot(d55) +
  facet_grid(mean ~ variance) +
  geom_line(aes(t, sqrt(sigma2)/mu)) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(expression(Coefficient~of~variation~'in'~italic(H)),
                     breaks = NULL); p_cv

#' model assumes:
#' - infinite behavioral plasticity, 
#' - instantaneous response times,
#' - 
eat <- function(mu, sigma2, reps = REPS) {
  expand_grid(replicate = 1:reps, n_visits = 1:100) %>% # 100 visits for reach replicate
    group_by(replicate) %>% # do the following for each replicate independently
    # generate rgamma() values for each row, then sum cumulatively
    mutate(intake = rgamma2(N = n(), mu = mu, sigma2 = sigma2) %>% cumsum()) %>%
    filter(intake > required) %>% # remove all visits before animal reached satiety
    # filter(n_visits == min(n_visits)) %>% # take the smallest visit count
    slice(1) %>% # take the first (i.e., smallest) visit count
    ungroup() %>%
    select(n_visits)
}

if(FALSE) {
  plan(multisession, workers = 7) # ~ 40 minutes
  
  #' use `furrr_options(seed = NULL)` to ensure statistically sound simulations
  tictoc::tic() # to measure computation time
  d <- d55 %>%
    mutate(visits = future_map2(.x = mu, .y = sigma2, .f = eat,
                                .options = furrr_options(seed = NULL))) %>%
    unnest(visits)
  tictoc::toc(); beepr::beep() # notify when calculations are complete
  plan(sequential) # back to sequential coputing to avoid crashes
  saveRDS(d, 'data/mean-variance-simulations-2e4-reps.rds') # save simulations
} else {
  d <- readRDS('data/mean-variance-simulations-2e4-reps.rds')
}

# summarize simulations as means, and means +/- 2 SD
d_summarized <-
  d %>%
  group_by(mean, variance, t, mu, sigma2) %>%
  summarize(mean_visits = mean(n_visits),
            sd_visits = sd(n_visits),
            lwr = quantile(n_visits, 0.05),
            upr = quantile(n_visits, 0.95),
            .groups = 'drop')

# simulation figure
hr_lab <- expression(Home~range~size~(italic(H)))
p_sim_55 <-
  ggplot(d_summarized) +
  facet_grid(mean ~ variance, scales = 'free_y') +
  geom_ribbon(aes(t, ymin = mean_visits, ymax = upr), fill = pal[3], alpha = 0.2) +
  geom_line(aes(t, mean_visits), color = pal[3], lwd = 1) +
  geom_line(aes(t, upr), color = pal[3]) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(hr_lab, breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

# trends in the mean
p_mean <-
  ggplot(d55, aes(t, mu)) +
  facet_grid(mean ~ ., switch = 'y') +
  geom_line(color = pal[1], lwd = 1) +
  scale_x_continuous(' ', breaks = NULL) +
  scale_y_continuous(expression(Mean~italic(U)), breaks = NULL) +
  theme(strip.background = element_blank(), axis.title = element_text(face = 'bold'),
        axis.text = element_text(color = 'transparent'), axis.ticks = element_blank(),
        panel.grid = element_blank())

# trends in the variance
p_variance <-
  ggplot(d55, aes(t, sigma2)) +
  facet_wrap(. ~ variance, nrow = 1) +
  geom_line(color = pal[2], lwd = 1) +
  scale_x_continuous(expression(Variance~'in'~italic(U)), breaks = NULL, position = 'top') +
  scale_y_continuous('', breaks = NULL) +
  theme(strip.background = element_blank(), axis.title = element_text(face = 'bold'),
        axis.text = element_text(color = 'transparent'), axis.ticks = element_blank(),
        panel.grid = element_blank())

plot_grid(plot_grid(NULL, p_variance, rel_widths = c(1, 4.4), nrow = 1),
          plot_grid(p_mean, p_sim_55, rel_widths = c(1, 4.5), nrow = 1),
          rel_heights = c(1, 4), nrow = 2)

ggsave('figures/mean-variance-5-by-5-sims.png', width = 8, height = 4.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# figure of SD(visits)
p_sd <-
  ggplot(d_summarized) +
  facet_grid(mean ~ variance, scales = 'free_y') +
  geom_area(aes(t, sd_visits), color = 'black', alpha = 0.3) +
  scale_x_continuous('Time', breaks = NULL) +
  scale_y_continuous(expression(Standard~deviation~of~italic(H)), breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_blank())

plot_grid(plot_grid(NULL, p_variance, rel_widths = c(1, 4.3), nrow = 1),
          plot_grid(p_mean, p_sd, rel_widths = c(1, 4.5), nrow = 1),
          rel_heights = c(1, 4), nrow = 2)
ggsave('figures/mean-variance-5-by-5-sd-visits.png', width = 8, height = 4.5, scale = 2,
       units = 'in', dpi = 'print', bg = 'white')

# plot coefficient of variation
plot_grid(plot_grid(NULL, p_variance, rel_widths = c(1, 4.5), nrow = 1),
          plot_grid(p_mean, p_cv + theme(strip.background = element_blank(),
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
  scale_y_continuous('Home range size', breaks = NULL, limits=c(20,40))+
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

# see https://github.com/NoonanM/BallisticMotion -----------------------------------------
