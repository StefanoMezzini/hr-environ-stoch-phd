library('ggplot2') # for fancy plots

# create animated stochasticity raster
create_animated_raster <- function(movement_data, raster_data) {
  ggplot(mapping = aes(mu, s2)) +
    
    # add environmental raster
    geom_raster(aes(fill = food, group = interaction(mu, s2)), raster_data) +
    
    # add static starting points
    geom_point(data = mov0, pch = 16, size = 3) +
    geom_point(data = mov0, pch = 16, size = 1.5,
               color = if_else(LIGHT.THEME, 'white', 'black')) +
    
    # add movement lines and points
    geom_path(aes(group = animal), movement_data) +
    geom_point(aes(group = animal), movement_data, pch = 16, size = 3) +
    geom_point(aes(group = animal), movement_data, pch = 16, size = 1.5,
               color = if_else(LIGHT.THEME, 'white', 'black')) +
    
    # add final labels
    geom_label(aes(label = n_steps, group = animal), total.steps, size = 3) +
    
    # improve axis labels, remove ticks, remove space between raster to the axes
    scale_x_continuous('Resource abundance', breaks = NULL, expand = c(0, 0)) +
    scale_y_continuous('Environmental stochasticity', breaks = NULL,
                       expand = c(0, 0)) +
    
    # add a custom color scale for the raster
    scale_fill_gradient2(low = LOW, mid = MID, high = HIGH) +
    
    # no legend or outer border
    theme(legend.position = 'none', panel.border = element_blank()) +
    
    # animate the plot
    transition_reveal(along = t)
}
