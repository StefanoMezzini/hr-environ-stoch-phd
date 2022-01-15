# figures based on the work of Southwood (1977)
library('tibble')   # for fancy data frames
library('dplyr')    # for data wrangling (e.g., %>%, mutate())
library('tidyr')    # for data wrangling (e.g., expand_grid())
library('purrr')    # for functional programming (e.g., map_***(), map2_***())
library('ggplot2')  # for fancy plots
library('ggridges') # for ridgeline plots
theme_set(theme_bw() + # change default ggplot theme
            theme(panel.grid = element_blank()))

# source custom functions and default parameters
source('analysis/default-figure-styling.R') # default figure parameters
source('functions/rgamma2.R') # rgamma parameterized by mean and variance

# habitat types (adapted from figure 7 of Southwood 1977) ---
add generation times (tau)
types <-
  expand_grid(t = seq(4, 45, length.out = 200),
              type = c('Predictable', 'Unpredictable', 'Ephemeral')) %>%
  mutate(r = case_when(type == 'Predictable' ~ dnorm(t, round(t, -1)) /
                         (round(t/10) %% 2 + 1),
                       type == 'Unpredictable' ~ dnorm(t, 11) +
                         dnorm(t, 26, 0.5) + dt(t - 37, 4) * 2,
                       type == 'Ephemeral' ~ dgamma(t, 74, 4) +
                         dgamma(t, 300, 18)),
         type = paste(type, 'habitat') %>%
           factor(levels = c('Predictable habitat', 'Unpredictable habitat',
                             'Ephemeral habitat')))
p_types <-
  ggplot(types) +
  facet_grid(type ~ ., scales = 'free_y') +
  geom_area(aes(x = t, y = r), alpha = 0.75, color = 'forestgreen',
            fill = 'forestgreen') +
  scale_x_continuous(breaks = NULL, name = 'Time') +
  scale_y_continuous(breaks = NULL, name = 'Habitat favorableness'); p_types

save_plot(plt = p_types, file_name = 'habitat-favorableness-types.png')

# discretized space of stochasticity & resources ----
# set colors for raster palette
LOW <- '#744700'
MID <- '#d9bb94'
HIGH <- 'darkgreen'

n_steps <- 50
set.seed(1) # for constant results
habitats <- expand_grid(resources = c(25, 100),
                        stochasticity = c(25, 200),
                        t = as.character(0:5)) %>%
  mutate(habitat = map2(resources, stochasticity,
                        \(m, s2) expand_grid(x = 1:n_steps,
                                             y = 1:n_steps) %>%
                          mutate(value = rgamma2(mu = m, sigma2 = s2,
                                                 N = n_steps^2))),
         res_lab = if_else(resources == min(resources), 'Low', 'High') %>%
           factor(levels = c('Low', 'High')),
         stoch_lab = if_else(stochasticity == min(stochasticity),
                             'Low', 'High') %>%
           # in opposite direction so low/low is in the bottom left
           factor(levels = c('High', 'Low'))) %>%
  unnest(cols = 'habitat')

# 2D view of the areas at time t == 0
p0 <-
  habitats %>%
  filter(t == 1) %>%
  ggplot(aes(x, y)) +
  facet_grid(stoch_lab ~ res_lab, switch = 'both') +
  geom_raster(aes(fill = value)) +
  scale_x_continuous('Resources') +
  scale_y_continuous('Stochasticity') +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH) +
  theme(legend.position = 'none',
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(face = 'bold'),
        panel.grid = element_blank(),
        strip.background = element_blank()); p0

# changes in habitats over time ----
habitats %>%
  filter(y == 1) %>%
  ggplot(aes(x = x, y = t, height = value, fill = as.numeric(t))) +
  facet_grid(stoch_lab ~ res_lab, switch = 'both') +
  geom_density_ridges(stat = 'identity', panel_scaling = FALSE, scale = 0.75) +
  scale_x_continuous('Resources') +
  scale_y_discrete('Stochasticity') +
  scale_fill_viridis_c(option = 'A', direction = -1)

# polymorphism figure ----
set.seed(2)
temperatures <- tibble(region = sort(rep(c('a', 'b', 'c'), 100)),
                       mu = case_when(region == 'a' ~ 5,
                                      region == 'b' ~ 10,
                                      region == 'c' ~ 15),
                       sigma = case_when(region == 'a' ~ 1,
                                         region == 'b' ~ 3,
                                         region == 'c' ~ 1),
                       temperature = rt(n = 300, df = 20) * sigma + mu)

ggplot(temperatures) +
  geom_density(aes(temperature, fill = region, color = region), alpha = 0.5) +
  scale_x_continuous(expression('Temperature (\u00B0C)'), limits = c(0, 20)) +
  scale_y_continuous('Density', breaks = NULL) +
  scale_fill_brewer('Region', type = 'qual', palette = 6,
                    aesthetics = c('fill', 'color'), direction = -1)
