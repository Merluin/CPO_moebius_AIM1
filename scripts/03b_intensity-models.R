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
library(emmeans)
library(afex)


# Functions ---------------------------------------------------------------

devtools::load_all()

# Setup -------------------------------------------------------------------

seed <- 2022
chains <- 15
iter <- 4000
cores <- 6
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

# Anova ------------------------------------
datanova <- dat%>%
  filter(Wheel.task == "task" & Wheel.name == "GW1")%>%
  mutate(Video.emotion = case_when(Video.emotion == "sad" ~ "sadness",
                                   Video.emotion == "fear"~ "fear",
                                   Video.emotion == "angry"~ "anger",
                                   Video.emotion == "disgusted"~ "disgust",
                                   Video.emotion == "surprised"~ "surprise",
                                   Video.emotion == "neutral"~ "neutral",
                                   Video.emotion == "happy"~ "happiness"),
         correct = ifelse(Video.emotion == resp_emotion_label, 1,0))%>%
  dplyr::select(Pt.code,int,Video.intensity,Video.emotion,resp_emotion_label,Pt.group,correct)%>%
  'colnames<-'(c("subject","valuation" ,"full/subtle", "emotion","resp","group","correct"))%>%
  filter(correct == 1)

a1 <- aov_ez("subject", "valuation", datanova,  within = c("full/subtle", "emotion"), between = c("group"))
m1<-emmeans(a1,pairwise~ `full/subtle`,adjust="bonf")
m3<-emmeans(a1,pairwise~ emotion|`full/subtle`,adjust="bonf")
m4<-emmeans(a1,pairwise~ `full/subtle`|emotion,adjust="bonf")

a2 <- aov_ez("subject", "valuation", datanova%>%filter(`full/subtle` == "full"),  within = c( "emotion"), between = c("group"))
a3 <- aov_ez("subject", "valuation", datanova%>%filter(`full/subtle` == "subtle"),  within = c( "emotion"), between = c("group"))




datanovafull <- dat%>%
  filter(Wheel.task == "task" & Wheel.name == "GW1" & Video.emotion != "neutral")%>%
  dplyr::select(Pt.code,int,Video.intensity,Video.emotion,resp_emotion_label,Pt.group)%>%
  'colnames<-'(c("subject","valuation" ,"full/subtle", "emotion","resp","group"))
a1 <- aov_ez("subject", "valuation", datanovafull,  within = c("full/subtle", "emotion"), between = c("group"))

a1 <- aov_ez("subject", "valuation", datanova,  within = c("full/subtle", "emotion"), between = c("group"))
m1<-emmeans(a1,pairwise~ `full/subtle`,adjust="bonf")



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
                  file = file.path("models","intensity","fit_ri_int.rds"),
                  save_pars = save_pars(all = TRUE),
                  sample_prior = samp_prior,
                  seed = seed)

success_step(fit_ri_int)
summary(fit_ri_int)
m1<-emmeans(fit_ri_int, pairwise ~intensity|emotion)
s1<-m1$contrasts
s1%>%
  as.data.frame()%>%
  flextable()%>% 
  colformat_double(digits = 2) %>% 
  theme_vanilla()
# summary(fit_ri_int)
# emmeans(fit_ri_int, ~intensity|emotion )

# Model 1b - Emotion  * intensity * group ------------------------------------

prior_gaussian <- c(prior(normal(150, 100), class = "b", coef = "Intercept"))

form_ri_3int <- bf(
  int ~  0 + Intercept + emotion *  intensity * Pt.group+ (1|id)
)

fit_ri_3int <- brm(form_ri_3int,
                  data = dat_fit,
                  prior = prior_gaussian,
                  family = gaussian(),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  file = file.path("models","intensity","fit_ri_3int.rds"),
                  save_pars = save_pars(all = TRUE),
                  sample_prior = samp_prior,
                  seed = seed,
                  control=list(adapt_delta=0.99, 
                               max_treedepth=13))

success_step(fit_ri_3int)
summary(fit_ri_3int)
m1<-emtrends(fit_ri_3int, ~intensity|emotion, var = "Pt.sb")
s1<-summary(m1)
s1%>%
  as.data.frame()%>%
  flextable()%>% 
  colformat_double(digits = 2) %>% 
  theme_vanilla()

# emtrends(fit_ri_3int, ~intensity|emotion ,var = "Pt.sb")
# summary(dataset_fit_ri_3int)
# emtrends(dataset_fit_ri_3int, ~intensity|emotion ,var = "Pt.sb")

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

# summary(fit_ri_no2int)
# emmeans(fit_ri_no2int, ~emotion)
# emmeans(fit_ri_no2int, ~intensity)

# Model 5 - (neutral faces) ------------------------------------------

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
# summary(fit_ri_neu)
# emmeans(fit_ri_no2int, ~emotion)

#################################################
# 
# END
#
#################################################