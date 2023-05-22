###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#  Description: Computes dataset, plot, table, fit model for Uncertainty measure  
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

# Load the fitted dataset
dat_fit <- readRDS(file = file.path("data",paste0(datasetname,"_fit.rds")))
# Load the neutral dataset
dat_neutral  <- readRDS(file = file.path("data",paste0(datasetname,"_neutral.rds")))

# Calculate resultant mean for fitted dataset
kappa_mean <- dat_fit%>%
  dplyr::select(id,degree,video_set,emotion,Pt.group)%>%
  'colnames<-'(c("subject","degree","video_set", "emotion","group"))%>%
  drop_na(degree)%>%
  group_by(subject,group,emotion,video_set)%>%
  summarise(kp_mean = rho.circular(degree),
            log_rho = log(kp_mean))

# Calculate resultant mean for neutral dataset
dat_neutral <- dat_neutral%>%
  mutate(theta = circular(theta, units = "degrees" ))%>%
  dplyr::select(subject,group,video_set,theta)%>%
  drop_na(theta)%>%
  group_by(subject,group,video_set)%>%
  summarise(kp_mean = mean.circular(theta),
            kp_mean = rho.circular(kp_mean),
            log_rho = log(kp_mean))

# Plot Rho subtle vs full ----------------------------------------------

# Generate the first plot
plot_Rho_a<- kappa_mean %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>%
  group_by(subject,emotion,group,video_set) %>%
  reframe(variance = 1-kp_mean) %>% 
  ggplot(aes(x = variance, y = emotion, fill = video_set, shape = group )) +
  geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 0.5) +
  stat_halfeye( size = 1) +
  facet_grid(group ~ . ) +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.05, 0.15)) +
  xlab("Uncertainty") +
  labs(fill = "Group",
       shape = "video_set") 

# Generate the second plot
plot_Rho_b <- kappa_mean %>% 
  clean_emotion_names(emotion) %>%
  group_by(subject,emotion, group) %>%
  reframe(variance = 1-kp_mean) %>% 
  ggplot(aes(x = variance, y = emotion, shape = group)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", size = 0.5) +
  stat_halfeye(size = 1) +
  facet_grid(group ~ . ) +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none") +
  xlab(latex2exp::TeX("$\\Delta_{video_set}$ Uncertainty$"))

# Create a combined plot
plot_uncertainty <- cowplot::plot_grid(plot_Rho_a, plot_Rho_b )

# Create a list of plots
plot_list <- make_named_list(plot_Rho_a, 
                             plot_Rho_b,
                             plot_uncertainty)

# Save the list of plots as an RDS file
saveRDS(plot_list, file = "objects/plots_uncertainty.rds")

# Save the combined plot as a PNG file
ggsave_plot(plot_uncertainty,
            name = file.path("figures", "png", "plots_bais"),
            device = "png", width = 16, height = 9)

# Table uncertainty ADFES vs JeFFE ----------------------------------------------

# Generate a table summarizing the angle biases
tab_kappa <- kappa_mean%>%
  drop_na(kp_mean)%>%
  group_by(emotion,group,video_set)%>%
  reframe(kp_mean = mean(kp_mean))%>%
  spread(video_set,kp_mean)%>%
  mutate(set_diff = ADFES- JeFFE)%>%
  'colnames<-'(c("Emotion" ,"Group","Video~ADEFS~", "Video~JeFFE~","Uncertainty~Bias~" ))%>%
  flextable_with_param() %>% 
  align(part = "header", align = "center") %>% 
  align(j = 2, part = "body", align = "center") %>% 
  merge_v(1)

# Save the table as an RDS file
saveRDS(tab_kappa, file = here("objects", "Uncertainty_tables.rds"))


# Fit linear mixed-effects model for each emotion
emo <- c("anger", "disgust", "fear", "happiness", "sadness", "surprise","neutral")

for(i in 1:length(emo)){
  # Fit linear mixed-effects model
  
  if(emo[i]== "neutral"){
    # Fit the model for neutral data
    fit <- lmer(log_rho ~ video_set * group + (1|subject) ,
                data = dat_neutral)
  }else{
    # Fit the model for each emotion
  fit <- lmer(log_rho ~ video_set * group + (1|subject),
              data = kappa_mean %>%
                filter(emotion == emo[i]))
  }
  
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

file_paths <- list.files(path = here::here("models/theta/"), pattern = "\\.Rdata", full.names = TRUE)
#################################################
# 
# END
#
#################################################