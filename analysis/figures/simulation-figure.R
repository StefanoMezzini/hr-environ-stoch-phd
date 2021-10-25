library('ggplot2') # for fancy plots
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('purrr')   # for functional programming
library('grid')    # for arrow heads on axis

if(TRUE) {
  # make light theme default
  theme_set(theme_bw())
  LOW <- '#744700'
  MID <- '#d9bb94'
  HIGH <- 'darkgreen'
  GRID_COLOR <- 'grey10'
  IMG_NAME <- 'stochasticity-figure-light.png'
} else {
  # make dark theme the default, and remove white background 
  theme_set(ggdark::dark_theme_minimal() +
              theme(plot.background = element_rect(color = '#000000')))
  LOW <- '#744700'
  MID <- '#d9bb94'
  HIGH <- 'darkgreen'
  GRID_COLOR <- '#ffffff99'
  IMG_NAME <- 'stochasticity-figure-dark.png'
}

# food availability and stochasticity need to be independent
set.seed(11)     # for constant results
steps <- 25     # number of rows/cols
required <- 200 # min food for satiety
# food availability needs to be an integer so rbinom(food, size, prob) works
min.num <- 0
max.num <- steps
step.num <- 1
n.sources <- 1                       # number of sources of food/square
min.s2 <- 0                          # minimum variance 
max.s2 <- 2.5                        # max var, bound by sqrt(1 - 4*s2/n)
step.s2 <- (max.s2 - min.s2) / steps # step size for variance

# set up matrix of means, variances, food
m <-
  expand_grid(num = seq(min.num, max.num, by = step.num),
              s2 = seq(min.s2, max.s2, by = step.s2)) %>%
  mutate(p = (1 + sqrt(1 - 4 * s2 / 10)) / 2,
         p = abs(p),
         food = map2_dbl(num, p, function(x, y) rbinom(1, x, y)),
         visited = FALSE) %>% # prevent animals from visiting same tile twice
  select(-p) # not needed for plotting and movement

# base plot
p0 <-
  ggplot(m, aes(num, s2)) +
  geom_tile(aes(fill = food), color = GRID_COLOR) +
  scale_x_continuous('Resource abundance', breaks = NULL, expand = c(0, 0),
                     limits = c(NA, max.num + step.num * 2))+
  scale_y_continuous('Environmental stochasticity', breaks = NULL,
                  limits = c(NA, max.s2 + step.s2 * 2),
                  expand = c(0, 0)) +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 10) +
  theme(legend.position = 'none', panel.border = element_blank(),
        axis.line = element_line(arrow = arrow(length = unit(0.3, 'cm'),
                                               ends = 'last',
                                               type = 'closed')),
        text = element_text(face = 'bold')); p0

# add four starting corners
shift <- 2

# mean values
small.num <- min.num + step.num * shift
big.num <- max.num - step.num * shift

# sd values
small.s2 <- min.s2 + step.s2 * shift
big.s2 <- max.s2 - step.s2 * shift

# create 5 hungry "animals"
a1 <- tibble(num = small.num, s2 = small.s2, satiety = 0, t = 0)
a2 <- tibble(num = small.num, s2 = big.s2,   satiety = 0, t = 0)
a3 <- tibble(num = big.num,   s2 = small.s2, satiety = 0, t = 0)
a4 <- tibble(num = big.num,   s2 = big.s2,   satiety = 0, t = 0)
a5 <- tibble(num = min.num + step.num * ceiling(steps / 2), # central "animal"
             s2 = min.s2 + step.s2 * ceiling(steps / 2),
             satiety = 0, t = 0)
# starting.points <- bind_rows(a1, a2, a3, a4, a5) %>%
#   transmute(x = paste(num, s2)) %>%
#   pull(x)
# 
# # mark starting points as visited
# m <- mutate(m, visited = paste(food, s2) %in% starting.points)

# movement function, reaches satiety at required
feeding <- function(x){
  h <- x[nrow(x), 'num'] %>% as.numeric()     # current horizontal position
  v <- x[nrow(x), 's2']  %>% as.numeric()     # current vertical position
  s <- x[nrow(x), 'satiety'] %>% as.numeric() # current satiety
  
  # eat all the food at in current tile (<<- makes `m` in global environment)
  tmp <- m
  # tmp$food[(m$num == h & m$s2 == v)] <- 0
  tmp$visited[(m$num == h & m$s2 == v)] <- TRUE
  m <<- tmp
  
  # if not satiated, move to new location
  if(s < required) {
    # get info on surrounding cells
    adjacent <-
      filter(m,
             num >= h - step.num & num <= h + step.num, # only adjacent tiles
             s2 >= v - step.s2 & s2 <= v + step.s2,
             !visited) %>% # cannot re-visit tiles
      select(-visited)
    
    if(nrow(adjacent) == 0) {
      print(x)
      stop('No visitable adjacent tiles')
    }
    
    # move to a new spot and make a new resulting row of data
    new.row <-
      adjacent %>%
      sample_n(size = 1) %>%
      mutate(food = food + s) %>%
      rename(satiety = food) %>%
      mutate(t = nrow(x))
    
    x <- rbind(x, new.row)
  }
  
  if(new.row$satiety < required) {
    feeding(x)
  } else {
    x
  }
}

# generate movement data
movement <- rbind(mutate(feeding(a1), animal = '1'),
                  mutate(feeding(a2), animal = '2'),
                  mutate(feeding(a3), animal = '3'),
                  mutate(feeding(a4), animal = '4'),
                  mutate(feeding(a5), animal = '5'))

#find starting points
start.point <-
  movement %>%
  group_by(animal) %>%
  filter(t == 0)

# total number of steps required for satiety
total.steps <-
  movement %>%
  group_by(animal) %>%
  mutate(n.steps = n()) %>%
  filter(satiety == max(satiety))

p1 <- 
  p0 +
  geom_path(aes(group = animal), movement) +
  geom_point(data = start.point, pch = 16, size = 3) +
  geom_label(aes(label = n.steps), total.steps, size = 5); p1

ggsave(paste0('figures/', IMG_NAME), width = 8, height = 8, scale = 0.5,
       dpi = 'print')
