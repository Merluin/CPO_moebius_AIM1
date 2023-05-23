###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Bais measure of the CPO_moebius_AMIM1 experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the angle biases between ADFES and JeFFE.
#
#  Experiment CPO_moebius_AMIM1
#
#  Update:      23/05/2023
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

# Loading required packages
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

# Load the description file of the package
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

# Load the fitted dataset
dat_fit <- readRDS(file = file.path("data",paste0(datasetname,"_fit.rds")))
# Load the neutral dataset
dat_neutral  <- readRDS(file = file.path("data",paste0(datasetname,"_neutral.rds")))

# Calculate circular mean for fitted dataset
CircularMean <- dat_fit%>%
  mutate(circulardiff = circular(diff, units = "degrees" ))%>%
  dplyr::select(id,Pt.group,emotion,video_set,circulardiff)%>%
  'colnames<-'(c("subject","group","emotion" ,"video_set", "err_mean"))%>%
  drop_na(err_mean)%>%
  group_by(subject,group,emotion,video_set)%>%
  summarise(err_mean = mean.circular(err_mean))

# Calculate circular mean for neutral dataset
dat_neutral <- dat_neutral%>%
  mutate(theta = circular(theta, units = "degrees" ))%>%
  dplyr::select(subject,group,video_set,theta)%>%
  drop_na(theta)%>%
  group_by(subject,group,video_set)%>%
  summarise(theta_cen = mean.circular(theta))

# Plot Angle ADFES vs JeFFE ----------------------------------------------

# Generate the first plot
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

# Generate the second plot
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
  xlab(latex2exp::TeX("$\\Delta_{group}$ Bias"))

# Create a combined plot
plot_bais <- cowplot::plot_grid(plot_angle_a, plot_angle_b, 
                                labels = "AUTO", rel_widths = c(3, 2), align = "hv")

# Create a list of plots
plot_list <- make_named_list(plot_angle_a, 
                             plot_angle_b,
                             plot_bais)

# Save the list of plots as an RDS file
saveRDS(plot_list, file = "objects/plots_bais.rds")

# Save the combined plot as a PNG file
ggsave_plot(plot_bais,
            name = file.path("figures", "png", "plot_bais"),
            device = "png", width = 16, height = 9)


# Table Angle ADFES vs JeFFE ----------------------------------------------

# Generate a table summarizing the angle biases
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

# Save the table as an RDS file
saveRDS(tab_bais, file = here("objects", "bais_tables.rds"))

# Circular mean mixed effects model---------------------------------


# err_mean ~  video_set * group + (1|subject) 
# normalized data for emotions
min<-min(CircularMean$err_mean)
max<-max(CircularMean$err_mean)
CircularMean$normalized_mean = ((CircularMean$err_mean - min) / (max - min))
# normalized data for neutral
min<-min(dat_neutral$theta_cen)
max<-max(dat_neutral$theta_cen)
dat_neutral$normalized_mean = ((dat_neutral$theta_cen - min) / (max - min))

# Fit linear mixed-effects model for each emotion
emo <- c("anger", "disgust", "fear", "happiness", "sadness", "surprise","neutral")


for(i in 1:length(emo)){
  
  if(emo[i]== "neutral"){
    # Fit the model for neutral data
    fit <- lmer( normalized_mean ~ video_set * group + (1|subject),
                data = dat_neutral)
  }else{
    # Fit the model for each emotion
    fit <- lmer(normalized_mean ~ video_set * group + (1|subject),
                data = CircularMean %>%
                  filter(emotion == emo[i]))
  }
  
  # Generate table summary of the model
  table <- tab_model(fit, show.df = TRUE, string.p = "p adjusted", p.adjust = "bonferroni")
  
  # Perform ANOVA
  chiquadro <- car::Anova(fit, type = 3)
  
  # Generate model plot
  plot <- flexplot::visualize(fit, plot = "model", sample = 20) +
    theme(legend.position = "none") +
    ylab("Bais normalized angle error") +
    xlab(paste("Video", emo[i]))
  
  # Create ANOVA table from lmer
  
  chi_table <- chiquadro %>%
    drop_na(`Pr(>Chisq)`) %>%
    mutate(`Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>%
    kbl(caption = "Anova(model, type = 3)") %>%
    column_spec(4, color = ifelse(chiquadro$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")

  # Save the results as RData files
  save(fit, table, chiquadro, plot, chi_table, file = file.path("models", "theta", paste0("bais_", emo[i], ".RData")))
}

#################################################
# 
# END
#
#################################################
