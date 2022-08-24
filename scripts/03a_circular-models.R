###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS from previous GAMBAROTA scripts (https://github.com/shared-research/face-mask-gew.git) 
#  Date:        0382022
#  Description: Generate the dataset from Gorilla (https://app.gorilla.sc/) 
#  Experiment CPO_moebius_AMIM1
#
#  Update:      13/08/2022
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(brms)

# Functions ---------------------------------------------------------------

devtools::load_all()

# Setup -------------------------------------------------------------------

seed <- 2022
chains <- 15
iter <- 4000
cores <- chains  
samp_prior <- "yes"

# Data --------------------------------------------------------------------

datasetname<-"dataset"

dat_fit <- readRDS(file = file.path("data",paste0(datasetname,"_fit.rds")))
dat <- readRDS(file = file.path("data",paste0(datasetname,"_valid.rds")))
dat_fit_full <- dat_fit %>% 
  filter(intensity == "full")
dat_fit_subtle <- dat_fit %>% 
  filter(intensity == "subtle")

# Legend ------------------------------------------------------------------

# ri = by-subject random intercept
# int = 2 way interaction (emotion * intensity)
# no2int = no 2 way interaction (emotion + intensity)
# tas_mask = tas * mask
# neu = only neutral

# Circular Models ----------------------------------------------------------

prior_von_mises <- c(
  prior(normal(0, 2), class = "b", dpar = ""), # betas prior
  prior(normal(0, 2), class = "b", dpar = "kappa") # kappa prior
)

# Model 1 - Emotion  * intensity ------------------------------------

form_ri_int <- bf(diff_theta ~ emotion *  intensity + (1|id), 
                  kappa ~  emotion *  intensity + (1|id))

fit_ri_int <- brm(form_ri_int,
                  data = dat_fit,
                  prior = prior_von_mises,
                  family = von_mises(link = "tan_half", link_kappa = "log"),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  sample_prior = samp_prior,
                  file = file.path("models","theta",paste0(datasetname,"_fit_ri_int.rds")),
                  save_pars = save_pars(all = TRUE),
                  seed = seed)

success_step(fit_ri_int)

# Model 2 - Emotion + intensity ------------------------------------

form_ri_no2int <- bf(diff_theta ~ emotion + intensity   + (1|id), 
                     kappa ~ emotion + intensity   + (1|id))
                  
fit_ri_no2int <- brm(form_ri_no2int,
                  data = dat_fit,
                  prior = prior_von_mises,
                  family = von_mises(link = "tan_half", link_kappa = "log"),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  sample_prior = samp_prior,
                  file = file.path("models","theta",paste0(datasetname,"_fit_ri_no2int.rds")),
                  save_pars = save_pars(all = TRUE),
                  seed = seed)

success_step(fit_ri_no2int)

# Model 3 - (neutral faces) ------------------------------------------

prior_von_mises_neu <- c(
  prior(uniform(-3.141593, 3.141593), class = "b", dpar = "", lb = -3.141593, ub = 3.141593), # betas prior
  prior(normal(0, 2), class = "b", dpar = "kappa") # kappa prior
)

dat_neutral <- dat %>% 
  filter(emotion == "neutrality")%>% 
  mutate(id = as.numeric(Pt.code))%>% 
  mutate(theta_cen = theta - pi) # centering pi

form_ri_neu <- bf(theta_cen ~ 0 + Intercept + (1|id), 
                  kappa ~  0 + Intercept + (1|id))

fit_ri_neu <- brm(form_ri_neu,
                  data = dat_neutral,
                  prior = prior_von_mises_neu,
                  family = von_mises(link = "tan_half", link_kappa = "log"),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  sample_prior = samp_prior,
                  file = file.path("models","theta",paste0(datasetname,"_fit_ri_neu.rds")),
                  save_pars = save_pars(all = TRUE),
                  seed = seed)

success_step(fit_ri_neu)

#################################################
# 
# END
#
#################################################