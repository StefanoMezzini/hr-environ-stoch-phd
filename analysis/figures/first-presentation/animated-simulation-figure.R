library('ggplot2') # for fancy plots
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('purrr')   # for functional programming
library('grid')    # for arrow heads on axis

# set colors for raster palette
LOW <- '#744700'
MID <- '#d9bb94'
HIGH <- 'darkgreen'

# choose default theme
LIGHT.THEME <- TRUE

if(LIGHT.THEME) {
  # use ggdark::invert_geom_defaults() to invert geom defaults (black/white)
  
  # make light theme default
  theme_set(theme_bw())
  
  IMG_NAME <- 'stochasticity-figure-light.png' # file name
} else {
  # make dark theme the default, and remove white background 
  theme_set(ggdark::dark_theme_minimal() +
              theme(plot.background = element_rect(color = '#000000')))
  
  IMG_NAME <- 'stochasticity-figure-dark.png' # file name
}

# source custom functions
source('functions/rgamma2.R') # rgamma() as a function of mean and variance
source('functions/regenerate_tiles.R') # to regenerate tiles already "eaten"
source('functions/eat.R') # to eat food in each tile
source('functions/create_animated_raster.R') # to create standard animation

# set up parameters ----
# E(food) and V(food) cannot be independent bc E(food) == 0 => V(food) == 0
# food ~ Gamma(k, theta) => mean = k * theta, var = k * theta^2
# using alternative parameterization: food ~ Gamma(E(food), V(food))
set.seed(5)                          # for constant results
steps <- 100                         # number of rows & cols - 1
required <- 2500                     # min food for satiety
min.mu <- 1                          # minimum mean
max.mu <- 100                        # maximum mean
step.mu <- (max.mu - min.mu) / steps # step size for mean
min.s2 <- 0.1                        # minimum variance
max.s2 <- 50                         # maximum variance
step.s2 <- (max.s2 - min.s2) / steps # step size for variance
shift <- round(steps / 10)           # 4 animals start a 1/10th from each edge
small.mu <- min.mu + step.mu * shift # mean values
big.mu <- max.mu - step.mu * shift
small.s2 <- min.s2 + step.s2 * shift # variance values
big.s2 <- max.s2 - step.s2 * shift

# set up matrix of means, variances, food (animals can re-visit a location) ----
m <-
  expand_grid(mu = seq(min.mu, max.mu, by = step.mu),
              s2 = seq(min.s2, max.s2, by = step.s2)) %>%
  mutate(food = rgamma2(mu = mu, sigma2 = s2),
         regenerating = 0)

# ensure the big and small values exist in the raster
c(small.mu, big.mu) %in% unique(m$mu)
c(small.s2, big.s2) %in% unique(m$s2)

# static base plot on turn 0 ----
p0 <-
  ggplot(m, aes(mu, s2)) +
  geom_raster(aes(fill = food)) +
  scale_x_continuous('Resource abundance', breaks = NULL, expand = c(0, 0),
                     limits = c(NA, max.mu + step.mu * 2))+
  scale_y_continuous('Environmental stochasticity', breaks = NULL,
                     limits = c(NA, max.s2 + step.s2 * 2),
                     expand = c(0, 0)) +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH) +
  theme(legend.position = 'none', panel.border = element_blank(),
        axis.line = element_line(arrow = arrow(length = unit(0.3, 'cm'),
                                               ends = 'last',
                                               type = 'closed')),
        text = element_text(face = 'bold')); p0

# crate a nested tibble with a raster for each turn ----
turns <- tibble(t = 0, # start at time t = 0
                raster = list(m)) # initial spatial raster
unnest(turns, cols = raster) # the tibble can be unnested later...

# create 5 hungry "animals" ----
# starting points
mov0 <-
  bind_rows(tibble(animal = 1, mu = small.mu, s2 = small.s2),
            tibble(animal = 2, mu = small.mu, s2 = big.s2,),
            tibble(animal = 3, mu = big.mu,   s2 = small.s2,),
            tibble(animal = 4, mu = big.mu,   s2 = big.s2,),
            tibble(animal = 5, mu = (min.mu + max.mu) / 2,
                   s2 = (min.s2 + max.s2) / 2)) %>%
  left_join(m %>% 
              select(mu, s2, food) %>%
              rename(satiety = food),
            by = c('mu', 's2'))

# plot with starting positions
p0 +
  geom_point(data = mov0, pch = 16, size = 3) + # starting points
  geom_point(data = mov0, pch = 16, size = 1.5,
             color = if_else(LIGHT.THEME, 'white', 'black'))
ggsave(paste0('figures/first-presentation/static-movement-all-',
              if_else(LIGHT.THEME, 'light', 'dark'), '.png'), height = 4, width = 4)
  
# tibble with movement data
mov <- mutate(mov0, t = 0)

# remove the food from current positions
m <- mutate(m,
            food = if_else(paste(mu, s2) %in% paste(mov0$mu, mov0$s2), 0, food))

# generate movement data ----
eat(1) # start the first turn

# total number of steps required for satiety (keep `t` to make labels dynamic)
total.steps <-
  mov %>%
  group_by(animal) %>%
  mutate(n_steps = n()) %>%
  filter(t == max(t)) %>%
  select(animal, t, n_steps) %>%
  full_join(mov0, by = 'animal')

# create plot of static raster and movement ----
p1 <-
  p0 +
  geom_point(data = mov0, pch = 16, size = 3) + # starting points
  geom_point(data = mov0, pch = 16, size = 1.5,
             color = if_else(LIGHT.THEME, 'white', 'black')) +
  geom_path(aes(group = animal), mov) + # movement lines
  geom_point(data = mov %>% group_by(animal) %>% filter(t == max(t)),
             pch = 16, size = 3) + # final points
  geom_point(data = mov %>% group_by(animal) %>% filter(t == max(t)),
             pch = 16, size = 1.5,
             color = if_else(LIGHT.THEME, 'white', 'black')) +
  
  geom_label(aes(label = n_steps), total.steps, size = 3); p1

# save the final figure
ggsave(filename = IMG_NAME, height = 4, width = 4, scale = 1.5,
       path = 'figures/first-presentation')

# animate the plot ----
library('gganimate')

# create a tibble of all of the rasters
turns_unnested <- unnest(turns, cols = raster)

p.anim <- create_animated_raster(movement_data = mov,
                                 raster_data = turns_unnested)

# animate with custom parameters
anim <- animate(p.anim, start_pause = 3, end_pause = 10, duration = 20,
                nframes = max(turns$t), height = 1000, width = 1000, res = 300)
anim_save(paste0('figures/first-presentation/animated-movement-all',
                 if_else(LIGHT.THEME, '-light', 'dark'), '.gif'))

# animate only bottom right animal with custom parameters
mov4 <- filter(mov, animal == 4)
m4 <- filter(turns_unnested, mu >= 75, s2 >= 43, t <= max(mov4$t))

# static first frame
p_static_0 <-
  ggplot(mapping = aes(mu, s2)) +
  geom_raster(aes(fill = food), filter(m4, t == 0)) +
  # add starting points
  geom_point(data = mov0 %>% filter(animal == 4), pch = 16, size = 3) +
  geom_point(data = mov0 %>% filter(animal == 4), pch = 16, size = 1.5,
             color = if_else(LIGHT.THEME, 'white', 'black')) +
  # improve plot theme
  scale_x_continuous('Resource abundance', breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous('Environmental stochasticity', breaks = NULL,
                     expand = c(0, 0)) +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH) +
  theme(legend.position = 'none', panel.border = element_blank(),
        axis.line = element_line(arrow = arrow(length = unit(0.3, 'cm'),
                                               ends = 'last',
                                               type = 'closed')),
        text = element_text(face = 'bold')); p_static_0
ggsave(paste0('figures/first-presentation/static-movement-single',
              if_else(LIGHT.THEME, '-light', '-dark') , '.png'),
       plot = p_static_0, width = 1000, height = 1000, units = 'px',
       dpi = 'print')

# animated movement
p_anim_single <-
  ggplot(mapping = aes(mu, s2)) + # default aesthetics
  # add resource raster
  geom_raster(aes(fill = food, group = interaction(mu, s2)), m4) +
  # add lines and points for animal 4
  geom_path(data = mov4) +
  geom_point(data = mov4, pch = 16, size = 3) +
  geom_point(data = mov4, pch = 16, size = 1.5,
             color = if_else(LIGHT.THEME, 'white', 'black')) +
  # add starting points
  geom_point(data = mov0 %>% filter(animal == 4), pch = 16, size = 3) +
  geom_point(data = mov0 %>% filter(animal == 4), pch = 16, size = 1.5,
             color = if_else(LIGHT.THEME, 'white', 'black')) +
  # add final label
  geom_label(aes(label = n_steps), total.steps %>% filter(animal == 4),
             size = 5) +
  # improve plot theme
  scale_x_continuous('Resource abundance', breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous('Environmental stochasticity', breaks = NULL,
                     expand = c(0, 0)) +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH) +
  theme(legend.position = 'none', panel.border = element_blank(),
        axis.line = element_line(arrow = arrow(length = unit(0.3, 'cm'),
                                               ends = 'last',
                                               type = 'closed')),
        text = element_text(face = 'bold')) +
  # animate the plot
  transition_reveal(along = t) +
  ease_aes(default = 'elastic-in')

anim_single <-
  animate(p_anim_single, start_pause = 15, end_pause = 30, duration = 10,
          height = 1000, width = 1000, res = 300)
anim_save(paste0('figures/first-presentation/animated-movement-single',
                 if_else(LIGHT.THEME, '-light', '-dark'), '.gif')) # save the animation
