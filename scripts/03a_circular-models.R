###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
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
library(emmeans)
library(flextable)
library(circular)
library(bpnreg)
library(here)

# Functions ---------------------------------------------------------------

#usethis::use_description( check_name = FALSE)
devtools::load_all()

# Setup -------------------------------------------------------------------

seeds <- 101
iter <- 5000
cores <- 6  
burns = 1000
n.lags = 3

#its = iter, burn = burns, n.lag = burns, seed = seeds

# Data --------------------------------------------------------------------

datasetname<-"dataset"

dat_fit <- readRDS(file = file.path("data",paste0(datasetname,"_fit.rds")))
dat <- readRDS(file = file.path("data",paste0(datasetname,"_valid.rds")))
dat_fit_full <- dat_fit %>% 
  filter(intensity == "full")
dat_fit_subtle <- dat_fit %>% 
  filter(intensity == "subtle")
CircularMean <- dat_fit%>%
  mutate(correct = ifelse(emotion == resp_emotion_label, 1,0),
         correct = as.factor(correct),
         circulardiff = circular(diff, units = "degrees" ))%>%
  dplyr::select(id,circulardiff,magnitude,intensity,emotion,Pt.group,correct)%>%
  'colnames<-'(c("subject","degree","magnitude" ,"intensity", "emotion","group","correct"))%>%
  drop_na(degree)%>%
  group_by(subject,group,emotion,intensity)%>%
  summarise(circular_mean = mean.circular(degree),
            rho_mean = rho.circular(degree),
            mag_mean = mean(magnitude))



# Circular mean mixed effects model---------------------------------

# Model 0 - degree ~ (1|subject) 
from.fit0 <- bf(circular_mean ~ (1|subject))

CircularMean.fit0 <- bpnme(from.fit0$formula,
                  data = CircularMean,
                  its = iter, 
                  burn = burns, 
                  n.lag = n.lags, 
                  seed = seeds)

success_step(CircularMean.fit0)
saveRDS(CircularMean.fit0,file.path("models","theta","CircularMean.fit0.rds"))

# # summary(fit_ri_3int)
# m1<-emtrends(fit_ri_3int, ~intensity|emotion ,var = "Pt.sb")
# s1<-summary(m1)
# s1%>%
#   as.data.frame()%>%
#   flextable()%>% 
#   colformat_double(digits = 2) %>% 
#   theme_vanilla()
# summary(fit_ri_int)
# m1<-emmeans(fit_ri_int, ~intensity|emotion )

# Model 1 - degree ~ emotion + (1|subject) 
from.fit1 <- bf(circular_mean ~ emotion + (1|subject))

CircularMean.fit1 <- bpnme(from.fit1$formula,
                           data = CircularMean,
                           its = iter, 
                           burn = burns, 
                           n.lag = n.lags, 
                           seed = seeds)

success_step(CircularMean.fit1)
saveRDS(CircularMean.fit1,file.path("models","theta","CircularMean.fit1.rds"))

# Model 2 - degree ~ intensity + (1|subject) 
from.fit2 <- bf(circular_mean ~ intensity + (1|subject))

CircularMean.fit2 <- bpnme(from.fit2$formula,
                           data = CircularMean,
                           its = iter, 
                           burn = burns, 
                           n.lag = n.lags, 
                           seed = seeds)

success_step(CircularMean.fit2)
saveRDS(CircularMean.fit2,file.path("models","theta","CircularMean.fit2.rds"))

# Model 3 - degree ~ group + (1|subject) 
from.fit3 <- bf(circular_mean ~ group + (1|subject))

CircularMean.fit3 <- bpnme(from.fit3$formula,
                           data = CircularMean,
                           its = iter, 
                           burn = burns, 
                           n.lag = n.lags, 
                           seed = seeds)

success_step(CircularMean.fit3)
saveRDS(CircularMean.fit3,file.path("models","theta","CircularMean.fit3.rds"))

# Model 4 - degree ~ emotion * intensity + (1|subject) 
from.fit4 <- bf(circular_mean ~ emotion * intensity + (1|subject))

CircularMean.fit4 <- bpnme(from.fit4$formula,
                           data = CircularMean,
                           its = iter, 
                           burn = burns, 
                           n.lag = n.lags, 
                           seed = seeds)

success_step(CircularMean.fit4)
saveRDS(CircularMean.fit4,file.path("models","theta","CircularMean.fit4.rds"))

# Model 5 - degree ~ emotion * intensity * group + (1|subject) 
happy<-CircularMean%>%
  filter(emotion == "happiness")
from.fit5 <- bf(circular_mean ~  intensity * group + (1|subject) )

CircularMean.fit5 <- bpnme(from.fit5$formula,
                           data = happy,
                           its = iter, 
                           burn = burns, 
                           n.lag = n.lags, 
                           seed = seeds)

success_step(CircularMean.fit5)
saveRDS(CircularMean.fit5,file.path("models","theta","CircularMean.fit5.rds"))

print(CircularMean.fit5)


# Model 6 - degree ~ emotion + intensity + group + (1|subject) 
from.fit6 <- bf(circular_mean ~ emotion + intensity + group + (1|subject))

CircularMean.fit6 <- bpnme(from.fit6$formula,
                           data = CircularMean,
                           its = iter, 
                           burn = burns, 
                           n.lag = n.lags, 
                           seed = seeds)

success_step(CircularMean.fit6)
saveRDS(CircularMean.fit6,file.path("models","theta","CircularMean.fit6.rds"))



# model comparison
fit<-cbind(model = c("theta ~ (1|subject)",
                     "theta ~ emotion + (1|subject)",
                     "theta ~ intensity + (1|subject)",
                     "theta ~ group + (1|subject)",
                     "theta ~ emotion * intensity + (1|subject)",
                     "theta ~ emotion * intensity * group + (1|subject)",
                     "theta ~ emotion + intensity + group + (1|subject)"),
  rbind(CircularMean.fit0$model.fit,
CircularMean.fit1$model.fit,
CircularMean.fit2$model.fit,
CircularMean.fit3$model.fit,
CircularMean.fit4$model.fit,
CircularMean.fit5$model.fit,
CircularMean.fit6$model.fit))%>%
  data.frame()%>%
  dplyr::select(model,lppd,DIC,WAIC,WAIC2)%>%
  mutate( lppd = round(as.numeric(as.character(lppd)),3),
          DIC = round(as.numeric(as.character(DIC)),3),
          WAIC = round(as.numeric(as.character(WAIC)),3),
          WAIC2 = round(as.numeric(as.character(WAIC2)),3))

saveRDS(fit,file.path("models","theta","modelcomparison_theta.rds"))



# Model neutro - (neutral faces) ------------------------------------------


dat_neutral <- dat %>% 
  filter(Wheel.task == "task", Wheel.name == "GW1" ,emotion == "neutrality") %>% 
  mutate(theta_cen = theta - pi,# centering pi
         theta_cen = theta_cen * ( 180.0 / pi ), # radius to degree 
         theta_cen = circular(theta_cen, units = "degrees" ),
         id = as.numeric(Pt.code),
         intensity = Video.intensity)%>%
  dplyr::select(id,theta_cen,intensity,Pt.group)%>%
  'colnames<-'(c("subject","degree" ,"intensity","group"))%>%
  drop_na(degree)%>%
  group_by(subject,group,intensity)%>%
  summarise(degree = mean.circular(degree))

# Model 7 - degree ~ emotion * group + (1|subject) 
from.fit7 <- bf(degree ~ intensity * group + (1|subject))

CircularMean.fit7 <- bpnme(from.fit7$formula,
                           data = dat_neutral,
                           its = iter, 
                           burn = burns, 
                           n.lag = n.lags, 
                           seed = seeds)

success_step(CircularMean.fit7)
saveRDS(CircularMean.fit7,file.path("models","theta","CircularMean.fit7.rds"))



#################################################
# 
# END
#
#################################################



min_angular_error <- min(CircularMean$circular_mean)
max_angular_error <- max(CircularMean$circular_mean)
CircularMean$normalized_mean = (CircularMean$circular_mean - min_angular_error) / (max_angular_error - min_angular_error)

a1<-aov_ez("subject","circular_mean" , CircularMean , within = c( "emotion" , "intensity") , between= "group" )

emmeans(a1, pairwise ~ emotion)
emmeans(a1, pairwise ~ intensity|emotion)



