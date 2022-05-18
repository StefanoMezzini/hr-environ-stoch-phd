library('tibble')    # for fancy data frames
library('dplyr')     # for piping and data wrangling
library('lubridate') # for working with dates
library('stringi')   # for working with character strings
library('ggplot2')   # for plotting the timeline
# library('ggrepel')   # for non-overlapping ggplot labels
theme_set(theme_classic() +
            theme(axis.line.y = element_blank(),
                  legend.position = 'none'))

# curstom color-vision deficient palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#AA3377', '#EE6677', '#BBBBBB')

t_breaks <- tibble(dates = date(c('2022-01-01', '2022-05-01', '2022-09-01',
                                  '2023-01-01', '2023-05-01', '2023-09-01')),
                   dec = decimal_date(dates),
                   strings = paste(month.abb[month(dates)], year(dates)))

tasks <-
  readr::read_csv('writing/thesis/timeline.csv', col_types = 'ccDD') %>%
  mutate(header = purrr::map2_lgl(section, task, grepl),
         section = factor(section,
                          levels = c('Other', 'Chapter 4', 'Chapter 3',
                                     'Chapter 2', 'Chapter 1', 'Preparatory',
                                     'Coursework'))) %>%
  arrange(section, desc(start), desc(end)) %>%
  mutate(dec_start = lubridate::decimal_date(start),
         dec_end = lubridate::decimal_date(end),
         dec_mid = purrr::map2_dbl(dec_start, dec_end, \(x, y) (x + y) / 2),
         task = factor(task, levels = unique(task)),
         id = 1:n(),
         completed = end <= Sys.Date()) %>%
  filter(! section %in% c('Other', 'Coursework'))

ggplot(tasks, aes(y = id)) +
  geom_hline(aes(yintercept = id + 0.5), filter(tasks, header), alpha = 0.15) +
  geom_vline(xintercept = decimal_date(Sys.Date()), lty = 'dashed', alpha = 0.5) +
  geom_errorbar(aes(xmin = dec_start, xmax = dec_end, color = section),
                width = 0, lwd = 2, alpha = 0.75) +
  geom_text(aes(2021, label = task, color = header), hjust = 'left', size = 3,
            parse = FALSE) +
  scale_x_continuous(NULL, breaks = t_breaks$dec, labels = t_breaks$strings,
                     expand = c(0, 0)) +
  scale_y_continuous(NULL, breaks = NULL) +
  scale_color_manual(NULL, values = c('black', 'grey40', pal),
                     breaks = c(TRUE, FALSE, levels(tasks$section)))

ggsave('figures/proposal-defense/proposal-defense-timeline.png', width = 10, height = 5)
