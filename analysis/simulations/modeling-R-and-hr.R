library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('mgcv')    # for empirical Bayes GAMs
library('ggplot2') # for fancy plots
library('cowplot') # for fancy multi-panel plots
source('analysis/figures/default-figure-styling.R') # for color palette
theme_set(theme_bw() + theme(legend.position = 'none'))

e_r <- 'Resource abundance (\U1D53C(\U1D445))'
v_r <- 'Resource unpredictability (\U1D54D(\U1D445))'

sims <- readRDS('simulations/days-hrs.rds') %>%
  tibble() %>%
  pivot_longer(c(hr_50, hr_95), names_to = 'quantile', values_to = 'hr') %>%
  mutate(quantile = factor(quantile))

m <- gam(hr ~ quantile + mu + sigma2 + quantile:mu + quantile:sigma2,
         family = Gamma(link = 'log'),
         data = sims)

plot(m, all.terms = TRUE, pages = 1)

# estimate predictions ----
# effect of mu
newd_mu <- expand_grid(mu = seq(min(sims$mu), max(sims$mu), length.out = 400),
                       sigma2 = mean(sims$sigma2),
                       quantile = unique(sims$quantile))

preds_mu <-
  mutate(newd_mu,
         predict(m, newdata = newd_mu, type = 'link', se.fit = TRUE) %>%
           data.frame() %>%
           transmute(hr = exp(fit)))

# effect of sigma2
newd_sigma2 <-
  expand_grid(mu = mean(sims$mu),
              sigma2 = seq(min(sims$sigma2), max(sims$sigma2), length.out =400),
              quantile = unique(sims$quantile))

preds_sigma2 <-
  mutate(newd_sigma2,
         predict(m, newdata = newd_sigma2, type = 'link', se.fit = TRUE) %>%
         data.frame() %>%
           transmute(hr = exp(fit)))

# final figure
preds <- bind_rows(mutate(preds_mu, x = e_r) %>%
                     rename(value = mu) %>%
                     select(-sigma2),
                   mutate(preds_sigma2, x = v_r) %>%
                     rename(value = sigma2) %>%
                     select(-mu))

sims_l <- sims %>%
  pivot_longer(c(mu, sigma2)) %>%
  mutate(x = if_else(name == 'mu', e_r, v_r))

ggplot() +
  facet_grid(. ~ x, scales = 'free', switch = 'x') +
  geom_point(aes(value, hr), sims_l, alpha = 0.1, color = pal[3]) +
  geom_line(aes(value, hr, group = quantile, color = x), preds, linewidth = 1) +
  scale_color_manual(values = pal) +
  scale_x_continuous(NULL, breaks = NULL) + 
  scale_y_continuous('Home range size', breaks = NULL) +
  theme(strip.background = element_blank(), strip.text = element_text(size = 12))

ggsave('figures/simulations/simulation-regression-plots.png',
       width = 6, height = 3, dpi = 'print', bg = 'white', scale = 1.5)
