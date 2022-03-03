library('ggplot2') # for fancy figures
library('stringi') # for working with strings

theme_set(theme_bw())

# custom color-blind palette
pal <- c('#ff8c00', '#4477AA', '#009900', '#66CCEE',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

WIDTH <- 5
HEIGHT <- 3

save_plot <- function(plt, file_name, dir = 'figures/', w = WIDTH, h = HEIGHT,
                      scale = 2, theme = 'both') {
  if(theme == 'light' | theme == 'both') {
    ggsave(filename = stri_replace_last_fixed(file_name, '.png', '-light.png'),
           plot = plt, path = dir, scale = scale, width = w, height = h,
           dpi = 'print')
  }
  
  if(theme == 'dark' | theme == 'both') {
    ggsave(filename = stri_replace_last_fixed(file_name, '.png', '-dark.png'),
           plot = plt + ggdark::dark_mode(), path = dir, scale = scale,
           width = w, height = h, dpi = 'print')
    ggdark::invert_geom_defaults()
    message('\nInverted geom defaults back to original ones.')
  }
}
