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

# Model 1 - Emotion  * intensity ------------------------------------

prior_gaussian <- c(
  prior(normal(150, 100), class = "b", coef = "Intercept"),
  prior(normal(0, 50), class = "b")
)

form_ri_int <- bf(
  int ~  0 + Intercept + emotion *  intensity + (1|id)
)

fit_ri_int <- brm(form_ri_int,
                  data = dat_fit,
                  prior = prior_gaussian,
                  family = gaussian(),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  file = file.path("models","intensity",paste0(datasetname,"_fit_ri_int.rds")),
                  save_pars = save_pars(all = TRUE),
                  sample_prior = samp_prior,
                  seed = seed)

success_step(fit_ri_int)

# Model 2 - Emotion  + intensity ----------------------------------------

form_ri_no2int <- bf(int ~ 0 + Intercept + emotion + intensity + (1|id))

fit_ri_no2int <- brm(form_ri_no2int,
                  data = dat_fit,
                  prior = prior_gaussian,
                  family = gaussian(),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  file = file.path("models","intensity",paste0(datasetname,"_fit_ri_no2int.rds")),
                  save_pars = save_pars(all = TRUE),
                  sample_prior = samp_prior,
                  seed = seed)

success_step(fit_ri_no2int)

# Model 5 - mask (neutral faces) ------------------------------------------

form_ri_neu <- bf(
  int ~  0 + Intercept +  (1|id)
)

dat_neutral <- dat %>% 
  filter(emotion == "neutrality")%>% 
  mutate(id = as.numeric(Pt.code))

fit_ri_neu <- brm(form_ri_neu,
                  data = dat_neutral,
                  prior = prior_gaussian,
                  family = gaussian(),
                  chains = 4,
                  cores = cores,
                  iter = 10000,
                  file = file.path("models","intensity",paste0(datasetname,"_fit_ri_neu.rds")),
                  save_pars = save_pars(all = TRUE),
                  sample_prior = samp_prior,
                  seed = seed)

success_step(fit_ri_neu)

#################################################
# 
# END
#
#################################################