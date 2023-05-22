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

library(afex)

library(bayestestR)
library(BayesFactor)
library(brms)
library(bpnreg)

library(cowplot)
library(circular)

library(dplyr)

library(emmeans)

library(flextable)
library(flexplot)

library(ggplot2)

library(here)

library(kableExtra)

library(lme4)

library(magick)

library(phia)
library(purrr)

library(sjPlot)

library(tidybayes)
library(tidyverse)
library(tidyr)

# Functions ---------------------------------------------------------------

#usethis::use_description( check_name = FALSE)
devtools::load_all()

# Data --------------------------------------------------------------------

datasetname<-"dataset"

dat_fit <- readRDS(file = file.path("data",paste0(datasetname,"_fit.rds")))

kappa_mean <- dat_fit%>%
  dplyr::select(id,degree,video_set,emotion,Pt.group)%>%
  'colnames<-'(c("subject","degree","video_set", "emotion","group"))%>%
  drop_na(degree)%>%
  group_by(subject,group,emotion,video_set)%>%
  summarise(kappa_mean = rho.circular(degree),
            log_rho = log(kappa_mean))


# Model fitting
emo <- c("anger", "disgust", "fear", "happiness", "sadness", "surprise")

for(i in 1:length(emo)){
  # Fit linear mixed-effects model
  fit <- lmer(kappa_mean ~ video_set * group + (1|subject),
              data = kappa_mean %>%
                filter(emotion == emo[i]))
  
  # Generate table summary
  table <- tab_model(fit, show.df = TRUE, string.p = "p adjusted", p.adjust = "bonferroni")
  
  # Perform ANOVA
  chiquadro <- car::Anova(fit, type = 3)
  
  # Generate model plot
  plot <- flexplot::visualize(fit, plot = "model", sample = 20) +
    theme(legend.position = "none") +
    ylab("Uncertainty log(kappa)") +
    xlab(paste("Video", emo[i]))
  
  # Create ANOVA table
  chi_table <- chiquadro %>%
    drop_na(`Pr(>Chisq)`) %>%
    mutate(`Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>%
    kbl(caption = "Anova(model, type = 3)") %>%
    column_spec(4, color = ifelse(chiquadro$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")
  
  # Save the results
  save(fit, table, chiquadro, plot, chi_table, file = file.path("models", "kappa", paste0("uncertainty_", emo[i], ".RData")))
}


#################################################
# 
# END
#
#################################################