###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS from previous GAMBAROTA scripts (https://github.com/shared-research/face-mask-gew.git) 
#  Date:        0382022
#  Description: Generate the dataset from Gorilla (https://app.gorilla.sc/) 
#  Experiment CPO_moebius_AMIM1
#
#  Update:      23/08/2022
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyr)
library(dplyr)
library(brms)
library(tidybayes)

# Functions ---------------------------------------------------------------

devtools::load_all()

# Setup -------------------------------------------------------------------

seed <- 2022
set.seed(seed)

# Loading Model -----------------------------------------------------------

fit_list <- load_models(file.path("models","theta"))

# Model Summary -----------------------------------------------------------

tidy_list_fit <- lapply(fit_list, tidy_brm)
tidy_list_priors <- lapply(fit_list, tidy_priors)
fit_info <- lapply(fit_list, get_model_info)

# Posterior Draws ---------------------------------------------------------

# fit_ri_int

data_grid_fit_ri_int <- expand_grid(
  emotion = unique(fit_list$fit_ri_2int$data$emotion),
  intensity = unique(fit_list$fit_ri_2int$data$intensity)
)

# getting posterior predictions. Here the inverse of the link functions are applied
# tan-half for mu and exp for kappa

post_fit_ri_int <- epred_draws(fit_list$fit_ri_2int, newdata = data_grid_fit_ri_int,
                               re_formula = NA)

# getting posterior in degrees. Given that is a linear transformation, results are
# conceptually the same

post_fit_ri_int$angle <- rad_to_deg(post_fit_ri_int$.epred)

post_fit_ri_int <- post_fit_ri_int %>% 
  rename("theta" = .epred)

# computing relevant posterior transformations

post_fit_ri_diff_int <- post_fit_ri_int %>% 
  ungroup() %>% 
  select( emotion, intensity, angle, .draw) %>% 
  pivot_wider(names_from = intensity, values_from = c( angle)) %>% 
  mutate(angle_diff = full - subtle)

# Adding Information Criteria ---------------------------------------------

fit_list$fit_ri_2int <- add_criterion(fit_list$fit_ri_2int, "loo", ndraws = 5000, force_save = TRUE)



loo_list <- list(
  fit_ri_2int = fit_list$fit_ri_2int$criteria$loo
)

# Saving ------------------------------------------------------------------

circular <- list(
  fit_info = fit_info,
  tidy_fit = tidy_list_fit,
  priors = tidy_list_priors,
  tidy_post = list(post_fit_ri_int = post_fit_ri_int,
                   post_fit_ri_diff_int = post_fit_ri_diff_int),
  loo = loo_list
)

saveRDS(circular, file = file.path("objects", "circular_objects.rds"))

#################################################
# 
# END
#
#################################################