###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Accuracy measure of the CPO_moebius_AMIM1 experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the response Accuracy.
#
#  Experiment CPO_moebius_AMIM1
#
#  Update:      23/05/2023
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

devtools::load_all()

# Data --------------------------------------------------------------------

datasetname<-"dataset"

# Load the fitted dataset
dat_fit <- readRDS(file = file.path("data",paste0(datasetname,"_fit.rds")))
emo_coords <- readRDS(file.path("objects", "emo_coords.rds"))

# Load the neutral dataset
dat <- readRDS(file.path("data",paste0(datasetname,"_valid.rds")))

# Calculate mean intensity
correct_data <-  dat_fit %>%
  mutate(correct = ifelse(emotion == resp_emotion_label, 1, 0)) %>%
  dplyr::select(id,  video_set, emotion, Pt.group, correct) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion", "group", "correct"))

accuracy<-correct_data%>%
  group_by(subject, group, emotion, video_set) %>%
  summarise(correct = sum(correct),
            acc = correct/8) # 8 = 4 video id * 2 blocks

# Plot accuracy GWE 1 vs GWE 2 ----------------------------------------------

# order as the wheel
dat$resp_emotion_label <- factor(dat$resp_emotion_label, levels = emo_coords$emo_order)
GEW <- c("GW1", "GW2")
dat_summ <- dat %>%
  drop_na(emotion)%>%
  mutate(video_set = Video.intensity)%>%
  mutate(video_set = ifelse(video_set == "full","ADFES" , "JeFFE" ))%>%
  filter(emotion != "neutrality" ) %>%
  group_by(emotion,video_set,Wheel.name, resp_emotion_label,Pt.group) %>%
  summarise(n = n())

for(i in 1:length(GEW)){
plot_gew_discrete <- dat_summ %>% 
  filter(Wheel.name == GEW[i] )%>%
  mutate(video_set = stringr::str_to_title(video_set)) %>% 
  clean_emotion_names(emotion) %>% 
  ggplot(aes(x = resp_emotion_label, y = n, fill = video_set)) +
  geom_col(position = position_dodge()) +
  facet_grid(emotion~Pt.group) +
  cowplot::theme_minimal_hgrid() +
  theme_paper(font_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,
                                   face = ifelse(levels(dat_summ$resp_emotion_label) %in% unique(dat_summ$emotion),
                                                 "bold", "plain"),
                                   size = ifelse(levels(dat_summ$resp_emotion_label) %in% unique(dat_summ$emotion),
                                                 10, 8)),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 10),
        panel.grid.major.x = element_blank()) +
  labs(fill = "video_set")

#save
ggsave_plot(plot_gew_discrete,
            name = file.path("figures", "png", paste0("plot_gew",i,"_discrete")),
            device = "png", width = 15, height = 10)

saveRDS(plot_gew_discrete, file = file.path("objects",  paste0("plot_frequencies_gew",i,".rds")))

}

             
                 
# Accuracy GEW ------------------------------------------------------------

tab_acc_gew <- accuracy %>% 
  group_by(group,emotion, video_set) %>% 
  summarise(acc = mean(acc, na.rm = TRUE)) %>% 
  pivot_wider(names_from = emotion, values_from = acc) %>% 
  flextable() %>% 
  colformat_double(digits = 2) %>% 
  autofit() %>% 
  merge_v(j = 1:2) %>% 
  theme_vanilla() %>% 
  align(align = "center")

saveRDS(tab_acc_gew, file = file.path("objects","table_accuracy.rds"))

# Fit  correct for each emotion--------------------------------------------

emo <- c("anger", "disgust", "fear", "happiness", "sadness", "surprise")

for(i in 1:length(emo)){
  
  fit <- lmer(acc ~  group * video_set + (1|subject),
                   data = accuracy%>%
                filter(emotion == emo[i])%>%na.omit())
  
  # Generate table summary
  table <- tab_model(fit, show.df = TRUE, string.p = "p adjusted", p.adjust = "bonferroni")
  
  # Perform ANOVA
  chiquadro <- car::Anova(fit, type = 3)
  
  # Generate model plot
  plot <- flexplot::visualize(fit, plot = "model") +
    theme(legend.position = "none") +
    ylab("accuracy") +
    xlab(paste("Video", emo[i]))
  
  # Create ANOVA table
  chi_table <- chiquadro %>%
    drop_na(`Pr(>Chisq)`) %>%
    mutate(`Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>%
    kbl(caption = "Anova(model, type = 3)") %>%
    column_spec(4, color = ifelse(chiquadro$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")
  
  # Save the results
  save(fit, table, chiquadro, plot, chi_table, file = file.path("models", "accuracy", paste0("accuracy_", emo[i], ".RData")))
}

#################################################
# 
# END
#
#################################################