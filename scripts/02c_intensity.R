###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS
#  Date:        19/05/2023
#  Description: Computes dataset, plot, table, fit model for Intensity measure  
#  Experiment CPO_moebius_AMIM1
#
#  Update:      19/05/2023
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

# Load the neutral dataset
dat <- readRDS(file.path("data",paste0(datasetname,"_valid.rds")))

# Calculate mean intensity
intensity_mean <-  dat_fit %>%
  mutate(correct = ifelse(emotion == resp_emotion_label, 1, 0),
         correct = as.factor(correct)) %>%
  dplyr::select(id, magnitude, video_set, emotion, Pt.group, correct) %>%
  'colnames<-'(c("subject", "intensity" ,"video_set", "emotion", "group", "correct")) %>%
  group_by(subject, group, emotion, video_set, correct) %>%
  summarise(int_mean = mean(intensity)) %>%
  drop_na(int_mean)

# Calculate intensity mean for neutral dataset
dat_neutral <- dat %>% 
  filter(Wheel.task == "task", Wheel.name == "GW1" ,emotion == "neutrality") %>% 
  mutate(video_set = Video.intensity,
  video_set = ifelse(video_set == "full","ADFES" , "JeFFE" ),
  emotion = "neutral")%>%
  dplyr::select(id,Pt.group,emotion,video_set,magnitude)%>%
  'colnames<-'(c("subject" ,"group","emotion","video_set","rho"))%>%
  drop_na(rho)%>%
  group_by(subject,group,emotion,video_set)%>%
  summarise(int_mean = mean(rho))

# Plot Angle ADFES vs JeFFE ----------------------------------------------

# Generate the first plot
plot_intensity_a <- intensity_mean %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>%
  ggplot(aes(x = int_mean, y = emotion, fill = video_set, shape = group )) +
  w_stat_halfeye() +
  facet_grid(group ~ correct ) +
  theme_paper() +
  xlab("Perceived Intensity") +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.01, 0.10))+
  labs(fill = "video_set",
       shape = "Group") 

# Generate the second plot
plot_intensity_b <- intensity_mean %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>%
  group_by(subject,emotion,group,correct) %>%
  summarise(int_mean = mean(int_mean)) %>% 
  ggplot(aes(x = int_mean, y = emotion, shape = group )) +
  w_stat_halfeye() +
  facet_grid(group ~ correct ) +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none") +
  xlab(latex2exp::TeX("$\\Delta_{intensity}\\; Perceived \\;Intensity$"))

# Create a combined plot
plot_intensity <- cowplot::plot_grid(plot_intensity_a, plot_intensity_b, 
                            labels = "AUTO", rel_widths = c(3, 2), align = "hv")

# Create a list of plots
plot_list <- make_named_list(plot_intensity_a, 
                             plot_intensity_b,
                             plot_intensity)

# Save the list of plots as an RDS file
saveRDS(plot_list, file = "objects/plots_intensity.rds")

# Save the combined plot as a PNG file
ggsave_plot(plot_intensity,
            name = file.path("figures", "png", "plots_intensity"),
            device = "png", width = 16, height = 9)

# Table intensity ADFES vs JeFFE ----------------------------------------------

# Generate a table summarizing the intensities
tab_intensity <- intensity_mean%>%
  drop_na(int_mean)%>%
  group_by(emotion,group,video_set)%>%
  reframe(int_mean = mean(int_mean))%>%
  spread(video_set,int_mean)%>%
  mutate(set_diff = ADFES- JeFFE)%>%
  'colnames<-'(c("Emotion" ,"Group","Video~ADEFS~", "Video~JeFFE~","Intensity~Bias~" ))%>%
  flextable_with_param() %>% 
  align(part = "header", align = "center") %>% 
  align(j = 2, part = "body", align = "center") %>% 
  merge_v(1)

# Save the table as an RDS file
saveRDS(tab_intensity, file = here("objects", "intensity_tables.rds"))

# Fit linear mixed-effects model for each emotion
emo <- c("anger", "disgust", "fear", "happiness", "sadness", "surprise","neutral")

for(i in 1:length(emo)){
  
  # Fit linear mixed-effects model
  
  if(emo[i]== "neutral"){
    # Fit the model for neutral data
    fit <- lmer(int_mean ~ video_set * group + (1|subject) ,
                data = dat_neutral)
  }else{
    # Fit the model for each emotion
    fit <- lmer(int_mean ~ video_set * group * correct + (1|subject),
                data = intensity_mean %>%
                  filter(emotion == emo[i]))
  }
  
  
  
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
