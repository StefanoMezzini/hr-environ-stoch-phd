# setup ----
library('dplyr')   # for data wangling
library('ggplot2') # for fancy figures
library('cowplot') # for multi-panel figures
theme_set(theme_bw())

# import energetics functions written by M. Noonan, see the script for references
source('functions/energetics-functions.R')
#' `pred.SIG()` # Generate predator `var[position]` based on mass (in g)
#' `pred.tau_p()` # Generate predator `E[tau_p]` based on mass (in g)
#' `pred.tau_v()` # Generate predator `E[tau_v]` based on mass (in g)

# generate movement and energetics parameters for a given mass, with no variance ----
d <- tibble(mass_pred_g = 1e5, # 100 kg = 100,000 g
            var_pos_pred = pred.SIG(mass = mass_pred), # HR size variance
            tau_p_pred = pred.tau_p(mass = mass_pred), # HR crossing time
            tau_v_pred = pred.tau_v(mass = mass_pred))
d

