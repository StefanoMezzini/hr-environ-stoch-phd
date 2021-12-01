library('readr')        # for read_csv()
library('dplyr')        # for data wrangling
library('tidyr')        # for data wrangling
library('sp')           # for spatial data
library('adehabitatHR') # for minimum convex polygon HR estimation 
library('ctmm')         # for continuous-time movement modeling
library('lubridate')    # makes working with dates smoother
library('purrr')        # for functional programming
library('ggplot2')      # for fancy plots
library('gganimate')    # for animated fancy plots

setwd('~/Uni/2021-03/biol501/project-presentation') # change working directory

# change default gif parameters
options(gganimate.dev_args = list(width = 4, height = 4, units = 'in',
                                  res = 300))

theme_set(ggdark::dark_theme_minimal() +
            theme(plot.background = element_rect(color = '#000000')))

# choose a fuzzy gralber (private data)

# estimate Minimum Convex Polygon
fg_mcp <- fg_data %>%
  SpatialPoints.telemetry() %>%
  mcp(percent = 95) %>%
  spTransform(CRS("+proj=longlat")) %>%
  fortify()

# estimate Kernel Density Estimate
fg_kde <- fg_data %>%
  SpatialPoints.telemetry() %>%
  kernelUD(h = 'href') %>%
  getverticeshr(percent = 95) %>%
  spTransform(CRS("+proj=longlat")) %>%
  fortify()

# figure of movement alone
p0 <-
  # add default aesthetics
  ggplot(mapping = aes(long, lat)) +
  # add invisible kde and akde to set the standard axis limits
  geom_polygon(data = fg_kde, alpha = 0) +
  geom_polygon(data = filter(fg_akde, grepl('high', group)), alpha = 0) +
  # add data
  geom_path(data = fg_data, alpha = 0.5) +
  geom_point(data = fg_data, size = 2) +
  # add axes but remove ticks
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL); p0

# animate the movement data
p0_anim <- p0 + transition_reveal(dec_date)
anim_0 <- animate(p0_anim, duration = 15, start_pause = 1, end_pause = 15)
anim_0
anim_save(filename = 'fuzzy-gralber-data.gif', anim_0)

# add MCP
p1 <-
  p0 +
  geom_polygon(data = fg_mcp, color = 'red', fill = 'red',
               alpha = 0.2, size = 1)

p1_anim <- p1 + transition_reveal(dec_date)
anim_1 <- animate(p1_anim, duration = 15, end_pause = 15)
anim_1
anim_save(filename = 'fuzzy-gralber-mcp.gif', anim_1)

# add KDE
p2 <-
  p1 + 
  geom_polygon(aes(group = piece), fg_kde, size = 1, color = 'goldenrod',
               fill = 'goldenrod', alpha = 0.2); p2
p2_anim <- p2 + transition_reveal(dec_date)
anim_2 <- animate(p2_anim, duration = 15, end_pause = 15)
anim_2
anim_save(filename = 'fuzzy-gralber-kde.gif', animation = anim_2)

# add AKDE
p3 <-
  p2 +
  geom_polygon(aes(group = piece), filter(fg_akde, type == 'Estimate'),
               size = 1, color = '#66CCEE', fill = '#66CCEE', alpha = 0.2) +
  scale_size_manual(values = c(0.5, 1, 0.5)); p3
p3_anim <- p3 + transition_reveal(dec_date)
anim_3 <- animate(p3_anim, duration = 15, end_pause = 15)
anim_3
anim_save(filename = 'fuzzy-gralber-akde.gif', animation = anim_3)

# add AKDE CIs
p4 <-
  p2 + 
  geom_polygon(aes(group = group, size = type, lty = type, alpha = type),
               fg_akde, color = '#66CCEE', fill = '#66CCEE',
               show.legend = FALSE) +
  scale_size_manual(values = c(0.5, 1)) +
  scale_linetype_manual(values = c(2, 1)) +
  scale_alpha_manual(values = c(0, 0.2)); p4
p4_anim <- p4 + transition_reveal(dec_date)
anim_4 <- animate(p4_anim, duration = 15, end_pause = 15)
anim_4
anim_save(filename = 'fuzzy-gralber-akde-cis.gif', animation = anim_4)

# static AKDE with CIs
p5 <-
  ggplot(mapping = aes(long, lat)) +
  geom_polygon(aes(group = group, size = type, lty = type, alpha = type),
               fg_akde, color = '#66CCEE', fill = '#66CCEE',
               show.legend = FALSE) +
  geom_path(data = fg_data, alpha = 0.5) +
  geom_point(data = fg_data, size = 2) +
  
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  scale_size_manual(values = c(0.5, 1)) +
  scale_linetype_manual(values = c(2, 1)) +
  scale_alpha_manual(values = c(0, 0.2)); p5

ggsave('fuzzy-gralber-static-akde-cis.png', p5, height = 4, width = 4, units = 'in')
