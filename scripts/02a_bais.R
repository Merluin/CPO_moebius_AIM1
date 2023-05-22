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

emo_order = c("Surprise", "Sadness", "Happiness", "Fear", "Disgust", "Anger")

CircularMean <- dat_fit%>%
  mutate(circulardiff = circular(diff, units = "degrees" ))%>%
  dplyr::select(id,Pt.group,emotion,video_set,circulardiff)%>%
  'colnames<-'(c("subject","group","emotion" ,"video_set", "err_mean"))%>%
  drop_na(err_mean)%>%
  group_by(subject,group,emotion,video_set)%>%
  summarise(err_mean = mean.circular(err_mean))


# Plot Angle ADFES vs JeFFE ----------------------------------------------

plot_angle_a<- CircularMean %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>%
  group_by(subject,group,emotion,video_set) %>%
  summarise(err_mean = mean.circular(err_mean)) %>% 
  ggplot(aes(x = err_mean, y = emotion, fill = group, shape = video_set )) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  stat_halfeye(alpha = 0.8, size = 2) +
  facet_grid(video_set ~ . ) +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.8, 0.15)) +
  xlab("Bias") +
  labs(fill = "Group",
       shape = "video_set") 

plot_angle_b <- CircularMean %>% 
  clean_emotion_names(emotion) %>%
  group_by(subject,emotion, video_set) %>%
  summarise(err_mean = mean.circular(err_mean)) %>% 
  ggplot(aes(x = err_mean, y = emotion, shape = video_set)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  stat_halfeye(size = 1) +
  facet_grid(video_set ~ . ) +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none") +
  xlab(latex2exp::TeX("$\\Delta_{group}$ Bias$"))

plot_mean <- plot_grid(plot_angle_a, plot_angle_b, 
                       labels = "AUTO", rel_widths = c(3, 2), align = "hv")

plot_list <- make_named_list(plot_angle_a, 
                             plot_angle_b,
                             plot_mean)

saveRDS(plot_list, file = "objects/plots_bais.rds")
ggsave_plot(plot_mean,
            name = file.path("figures", "png", "plot_bais"),
            device = "png", width = 16, height = 9)


# Table Angle ADFES vs JeFFE ----------------------------------------------

tab_bais <- dat_fit%>%
  mutate(circulardiff = circular(diff, units = "degrees" ))%>%
  dplyr::select(id,Pt.group,emotion,video_set,circulardiff)%>%
  'colnames<-'(c("subject","group","emotion" ,"video_set", "err_mean"))%>%
  drop_na(err_mean)%>%
  group_by(emotion,group,video_set)%>%
  summarise(err_mean = mean.circular(err_mean))%>%
  spread(video_set,err_mean)%>%
  mutate(set_diff = ang_diff(ADFES,JeFFE))%>%
  'colnames<-'(c("Emotion" ,"Group","Video~ADEFS~", "Video~JeFFE~","Delta~Bias~" ))%>%
  flextable_with_param() %>% 
  align(part = "header", align = "center") %>% 
  align(j = 2, part = "body", align = "center") %>% 
  merge_v(1)

saveRDS(tab_bais, file = here("objects", "paper_tables.rds"))

# Circular mean mixed effects model---------------------------------


# err_mean ~  video_set * group + (1|subject) 
min<-min(CircularMean$err_mean)
max<-max(CircularMean$err_mean)
CircularMean$normalized_mean = ((CircularMean$err_mean - min) / (max - min))

emo <- c("anger", "disgust", "fear", "happiness", "sadness", "surprise")

for(i in 1:length(emo)){
  # Fit linear mixed-effects model
  fit <- lmer(normalized_mean ~ video_set * group + (1|subject),
              data = CircularMean %>%
                filter(emotion == emo[i]))
  
  # fit <- bpnme(normalized_mean ~ video_set * group + (1|subject),
  #             data = CircularMean %>%
  #               filter(emotion == emo[i]),
  #             its = iter, 
  #             burn = burns, 
  #             n.lag = n.lags, 
  #             seed = seeds)}
  
  # Generate table summary
  table <- tab_model(fit, show.df = TRUE, string.p = "p adjusted", p.adjust = "bonferroni")
  
  # Perform ANOVA
  chiquadro <- car::Anova(fit, type = 3)
  
  # Generate model plot
  plot <- flexplot::visualize(fit, plot = "model", sample = 20) +
    theme(legend.position = "none") +
    ylab("Bais normalized angle error") +
    xlab(paste("Video", emo[i]))
  
  # Create ANOVA table
  chi_table <- chiquadro %>%
    drop_na(`Pr(>Chisq)`) %>%
    mutate(`Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>%
    kbl(caption = "Anova(model, type = 3)") %>%
    column_spec(4, color = ifelse(chiquadro$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")
  
  # Save the results
  save(fit, table, chiquadro, plot, chi_table, file = file.path("models", "theta", paste0("bais_", emo[i], ".RData")))
}



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