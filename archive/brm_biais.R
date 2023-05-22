###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS
#  Date:        02052023
#  Description: Generate the dataset from psychopy3
#  Experiment CPO_moebius_AMIM1
#
#  Update:      02/05/2023
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(CircStats)
library(brms)
library(broom)
library(dplyr) 
library(circular)
library(bpnreg)
library(plotrix)
library(lme4)
# caricamento dei dati -----------------------------------------------------------

dataset <- readRDS("data/dataset_fit.rds")

# filter data
CircularMean <- dataset%>%
  mutate(correct = ifelse(emotion == resp_emotion_label, 1,0),
         correct = as.factor(correct),
         circulardiff = circular(diff, units = "degrees" ))%>%
  dplyr::select(id,circulardiff,int,intensity,emotion,Pt.group,correct)%>%
  'colnames<-'(c("subject","degree","arousal" ,"intensity", "emotion","group","correct"))%>%
  drop_na(degree)%>%
  group_by(subject,group,emotion,intensity)%>%
  summarise(degree = mean.circular(degree))

arousalMean <- dataset%>%
  mutate(correct = ifelse(emotion == resp_emotion_label, 1,0),
         correct = as.factor(correct))%>%
  dplyr::select(id,int,intensity,emotion,Pt.group,correct)%>%
  'colnames<-'(c("subject","arousal" ,"intensity", "emotion","group","correct"))%>%
  drop_na(arousal)%>%
  group_by(subject,group,emotion,intensity)%>%
  summarise_at(vars(arousal),list(mean))

# Circular mean mixed effects model

reg.fit1 <- bpnr(degree ~ emotion, data =  CircularMean, 
                  its = 5000, burn = 1000, n.lag = 3, seed = 101)
                  
reg.fit2 <- bpnr(degree ~ intensity , data =  CircularMean, 
                  its = 5000, burn = 1000, n.lag = 3, seed = 101)

reg.fit3 <- bpnr(degree ~ group  , data =  CircularMean, 
                  its = 5000, burn = 1000, n.lag = 3, seed = 101)

reg.fit4 <- bpnr(degree ~ emotion * intensity , data = CircularMean, 
                  its = 5000, burn = 1000, n.lag = 3, seed = 101)

reg.fit5 <- bpnr(degree ~ intensity + group , data = CircularMean, 
                 its = 5000, burn = 1000, n.lag = 3, seed = 101)

reg.fit6 <- bpnr(degree ~ emotion * intensity * group , data = CircularMean, 
                           its = 5000, burn = 1000, n.lag = 3, seed = 101)

reg.fit7 <- bpnr(degree ~ emotion + intensity + group , data = CircularMean, 
                           its = 5000, burn = 1000, n.lag = 3, seed = 101)

# model comparison
fitreg<-cbind(model = c("emotion",
                     "intensity",
                     "group",
                     "emotion * intensity",
                     "intensity + group",
                     "emotion * intensity * group",
                     "emotion + intensity + group"),
           rbind(
                 reg.fit1$model.fit,
                 reg.fit2$model.fit,
                 reg.fit3$model.fit,
                 reg.fit4$model.fit,
                 reg.fit5$model.fit,
                 reg.fit6$model.fit,
                 reg.fit7$model.fit))%>%
  data.frame()%>%
  dplyr::select(model,lppd,DIC,WAIC,WAIC2)



# Lmer arousal mean mixed effects model
arousal.reg.fit0 <- lmer(arousal ~ (1|subject), data =  arousalMean)

arousal.reg.fit1 <- lmer(arousal ~ emotion + (1|subject), data =  arousalMean)

arousal.reg.fit2 <- lmer(arousal ~ intensity + (1|subject) , data =  arousalMean)

arousal.reg.fit3 <- lmer(arousal ~ group + (1|subject)  , data =  arousalMean)

arousal.reg.fit4 <- lmer(arousal ~ emotion * intensity + (1|subject) , data = arousalMean)

arousal.reg.fit5 <- lmer(arousal ~ intensity + group + (1|subject) , data = arousalMean)

arousal.reg.fit6 <- lmer(arousal ~ emotion * intensity * group + (1|subject) , data = arousalMean)

arousal.reg.fit7 <- lmer(arousal ~ emotion + intensity + group + (1|subject) , data = arousalMean)

AIC(arousal.reg.fit0,
    arousal.reg.fit1,
    arousal.reg.fit2,
    arousal.reg.fit3,
    arousal.reg.fit4,
    arousal.reg.fit5,
    arousal.reg.fit6,
    arousal.reg.fit7)
