library('dplyr') # for data wrangling

# movement function, reaches satiety at `required`
eat <- function(turn){
  
  # or continue moving and eating
  if(sum(turns$t == turn) == 0) { # if the turn hasn't started yet 
    # create new raster, add it to `turns` in `R_GlobalEnv`
    rast <- regenerate_tiles()
    turns <<- bind_rows(turns, tibble(t = turn, raster = list(rast)))
  } else { # otherwise use the existing raster
    rast <- turns %>%
      filter(t == max(t)) %>%
      unnest(raster) %>%
      select(-t) # `t` causes conflicts in nested tables
  }
  
  current <-
    mov %>%
    group_by(animal) %>%
    filter(t == max(t), # get the latest time point for each animal
           satiety < required) %>% # but only for animals which are not "full"
    ungroup() %>%
    filter(t != max(t) | t == min(t)) %>% # remove animals that already went
    filter(animal == min(animal)) # take the lowest animal of the remaining
  
  # extract current values for easy access
  h <- current %>% pull(mu)      # horizontal position
  v <- current %>% pull(s2)      # vertical position
  s <- current %>% pull(satiety) # satiety
  a <- current %>% pull(animal)  # animal ID
  
  # move to a new location and eat again
  new.row <-
    # get info on surrounding cells (at most 3 * 3 - 1 cells)
    filter(m, # without round(x, 2), animals don't move far from startig point
           round(abs(h - mu), 2) <= round(step.mu, 2) &
             round(abs(v - s2), 2) <= round(step.s2, 2),
           ! (round(mu, 2) == round(h, 2) &
                round(s2, 2) == round(v, 2))) %>% # except current tile
    # move to a new random spot and make a new row of data
    sample_n(size = 1) %>%
    mutate(satiety = s + food, # current satiety + new food
           t = turn,
           animal = a) %>%
    select(- c(food, regenerating)) # not needed in the movement dataset
  
  mov <<- bind_rows(mov, new.row)
  
  # remove food from new location (`<<-` assigns to `m` in R_GlobalEnv)
  turns$raster[turns$t == turn] <<-
    mutate(rast, # raster for time `t`
           food = if_else(mu == new.row$mu & s2 == new.row$s2, # same position
                          true = 0, false = food)) %>%
    list()
  
  # if not all animals are satisfied, move and eat again
  n_satiated <- mov %>%
    group_by(animal) %>%
    summarize(full = max(satiety) >= required) %>%
    pull(full) %>%
    sum()
  
  # find which animals were in the current turn or not full yet
  remaining <- mov %>%
    group_by(animal) %>%
    summarize(satiety = max(satiety),
              t = max(t)) %>%
    filter(t == max(t) | satiety < required) %>%
    pull(animal)
  
  new_turn <- if_else(a == max(remaining), # if it's last animal in the turn
                      true = turn + 1,     # start a new turn
                      false = turn)        # otherwise continue the turn
  
  if(n_satiated < n_distinct(mov0$animal)) eat(new_turn)
}
