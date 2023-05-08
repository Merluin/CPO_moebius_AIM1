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


# caricamento dei dati -----------------------------------------------------------

dataset <- readRDS("data/dataset_valid.rds")%>%
  dplyr::select(-contains(c("started","stopped")))


# selezione dei dati
data <- dataset%>%
  data.frame()%>%
  filter(Wheel.name == "GW1" & Wheel.task == "task")%>%
  mutate(intensity = ifelse(Video.intensity == "full",1,0),
         group = ifelse(Pt.group == "moebius",1,0))

GW1 <- data%>%
  mutate(angle = as.numeric(as.character(angle)),
         Pt.code = as.numeric(as.character(Pt.code)),
         angle_emo = as.factor(angle_emo),
         Pt.group = as.factor(Pt.group),
         intensity = as.factor(intensity))%>%
  drop_na(angle_emo, angle)


# Circular mixed effects model
GW1.fit0 <- bpnme(int + angle ~ (1|Pt.code), data = GW1)
GW1.fit1 <- bpnme(angle ~ angle_emo + (1|Pt.code), data = GW1)
GW1.fit2 <- bpnme(angle ~ angle_emo + Pt.group  + (1|Pt.code), data = GW1)
GW1.fit3 <- bpnme(angle ~ angle_emo + Pt.group + intensity + (1|Pt.code), data = GW1)
