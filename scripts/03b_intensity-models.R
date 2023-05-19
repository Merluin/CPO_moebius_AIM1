###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS
#  Date:        19/05/2023
#  Description: Generate the dataset from Gorilla (https://app.gorilla.sc/) 
#  Experiment CPO_moebius_AMIM1
#
#  Update:      19/05/2023
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(brms)
library(emmeans)
library(afex)
library(lme4)
library(here)
library(flexplot)
library(bayestestR)
library(BayesFactor)
library(phia)
# Functions ---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------

datasetname<-"dataset"

dat_fit <- readRDS(file = file.path("data",paste0(datasetname,"_fit.rds")))

# Calculate mean intensity
intensity_mean <-  dat_fit %>%
  mutate(correct = ifelse(emotion == resp_emotion_label, 1, 0),
         correct = as.factor(correct)) %>%
  dplyr::select(id, magnitude, video_set, emotion, Pt.group, correct) %>%
  'colnames<-'(c("subject", "intensity" ,"video_set", "emotion", "group", "correct")) %>%
  group_by(subject, group, emotion, video_set, correct) %>%
  summarise(int_mean = mean(intensity)) %>%
  drop_na(int_mean)

# Model fitting
emo <- c("anger", "disgust", "fear", "happiness", "sadness", "surprise")

for(i in 1:length(emo)){
  # Fit linear mixed-effects model
  fit <- lmer(int_mean ~ video_set * group * correct + (1|subject),
              data = intensity_mean %>%
                filter(emotion == emo[i]))
  
  # Generate table summary
  table <- tab_model(fit, show.df = TRUE, string.p = "p adjusted", p.adjust = "bonferroni")
  
  # Perform ANOVA
  chiquadro <- car::Anova(fit, type = 3)
  
  # Generate model plot
  plot <- flexplot::visualize(fit, plot = "model", sample = 20) +
    theme(legend.position = "none") +
    ylab("Perceived intensity (px)") +
    xlab(paste("Video", emo[i]))
  
  # Create ANOVA table
  chi_table <- chiquadro %>%
    drop_na(`Pr(>Chisq)`) %>%
    mutate(`Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>%
    kbl(caption = "Anova(model, type = 3)") %>%
    column_spec(4, color = ifelse(chiquadro$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")
  
  # Save the results
  save(fit, table, chiquadro, plot, chi_table, file = file.path("models", "intensity", paste0("intensity_", emo[i], ".RData")))
}


#################################################
# 
# END
#
#################################################
# fit <- lmer(mag_mean ~ intensity * group + (1|subject),
#             data = magnitude_mean%>%filter(correct == 1))
# car::Anova(fit, type = 3)
# 
# 
# testInteractions(fit, pairwise= c("intensity"), fixed= c("group"), adjustment= "fdr")
# 
# mod_acc <- glmer(correct ~  group * intensity + (1|subject),
#                  data = magnitude_mean%>%filter(emotion == "surprise"  ), family= binomial)
# 
# summary(mod_acc)
# 
# a1<-aov_ez("subject","mag_mean" , magnitude_mean%>%filter(correct == 1 & emotion == "fear"  ) , within = c(  "intensity") , between= "group")
# 
# x<-magnitude_mean%>%filter(correct == 1 & emotion != "happiness" & subject != 10)%>%
#   group_by()
# ttestBF(x = x$mag_mean[x$group == "moebius"],y  = x$mag_mean[x$group == "control"], paired=TRUE)
# 
# emmeans(a1, pairwise ~ emotion)
# emmeans(a1, pairwise ~ intensity)
# emmeans(a1, pairwise ~ intensity|emotion)
