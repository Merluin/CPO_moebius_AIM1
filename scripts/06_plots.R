###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS from previous GAMBAROTA scripts (https://github.com/shared-research/face-mask-gew.git) 
#  Date:        0382022
#  Description: Generate the dataset from Gorilla (https://app.gorilla.sc/) 
#  Experiment CPO_moebius_AMIM1
#
#  Update:      13/08/2022
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(brms)
library(tidybayes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(purrr)
library(magick)

# Functions ---------------------------------------------------------------

devtools::load_all()

ggsave_plot <- function(plot, name, device = c("png", "tiff", "eps", "pdf", "svg"),
                        width = width, height = height){
  device <- match.arg(device)
  name <- paste0(name, ".", device)
  ggsave(plot, filename = name, device = device, width = width, height = height)
}

theme_paper <- function(font_size = 12){
  cowplot::theme_minimal_grid(font_size = font_size)
}

put_note <- function(x, y, text, size = 3, ...){
    annotate("label", x, y, label = text, 
             fontface = 2, 
             fill = "white", 
             label.size = NA,
             size = size,
             ...)
}

w_stat_halfeye <- function(alpha = 0.8){
  stat_halfeye(alpha = alpha, 
               size = 2.5, 
               .width = 0.95)
}
  
# Importing Data --------------------------------------------------------

datasetname<-"dataset"

intensity_objects <- readRDS(file.path("objects", "intensity_objects.rds"))
circular_objects <- readRDS(file.path("objects", "circular_objects.rds"))
emo_coords <- readRDS(file.path("objects", "emo_coords.rds"))
dat <- readRDS(file.path("data",paste0(datasetname,"_valid.rds")))
dat_fit <- readRDS(file = file.path("data",paste0(datasetname,"_fit.rds")))
# EDA Plots ---------------------------------------------------------------

bg <- magick::image_read("files/gew_low_res.png")
bg <- magick::image_modulate(bg, brightness = 80)

gew_legend <- emo_coords %>%   
  mutate(mask = "Legend",
         flip = ifelse(x_emo < 0, degree_emo + 180, degree_emo),
         emotion = stringr::str_to_title(emotion)) %>% 
  ggplot() +
  ggpubr::background_image(bg) +
  geom_text(aes(x = x_emo*0.75, y = y_emo*0.75, 
                label = emotion, 
                angle = flip),
            size = 6, fontface = "bold",
            check_overlap = TRUE) +
  facet_grid(. ~ mask) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)) +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300))

# dat_plot <- dat %>% 
#   filter(Wheel.name == "GW1")%>%
#   dplyr::select(Pt.code,Pt.group,Wheel.task,emotion, Video.intensity, x_cen, y_cen) %>% 
#   mutate( intensity = stringr::str_to_title(Video.intensity),
#          emotion = stringr::str_to_title(emotion),
#          emotion = ifelse(emotion == "Neutrality", "Neutral", emotion),
#          emotion = factor(emotion),
#          emotion = forcats::fct_relevel(emotion, "Neutral"))
dat_plot <- dat %>% 
  dplyr::select(Pt.group,Wheel.task,emotion,Wheel.name, Video.intensity, x_cen, y_cen) %>% 
  mutate( intensity = stringr::str_to_title(Video.intensity),
          emotion = stringr::str_to_title(emotion),
          emotion = ifelse(emotion == "Neutrality", "Neutral", emotion),
          emotion = factor(emotion),
          emotion = forcats::fct_relevel(emotion, "Neutral"))

neutral_plot <- dat_plot %>% 
  filter(emotion == "Neutral") %>% 
  ggplot(aes(x = x_cen, y = y_cen)) +
  ggpubr::background_image(bg) +
  geom_point(alpha = 0.5, show.legend = FALSE, size = 3) +
  ggh4x::facet_nested(Video.intensity ~ emotion, switch="y") +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA))+
  facet_grid(Pt.group ~ Wheel.name)

plot_gew_legend_neutral <- plot_grid(neutral_plot, gew_legend, labels = "AUTO")

plot_gew_emotions <- dat_plot %>% 
  filter(emotion != "Neutral", Wheel.task == "task") %>% 
  ggplot(aes(x = x_cen, y = y_cen, color = intensity)) +
  ggpubr::background_image(bg) +
  geom_point(alpha = 0.5, size = 1) +
  ggh4x::facet_nested( Pt.group + Wheel.name  ~ emotion) +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = c("NA", "white"))

#-------------------------------------------------------------------------------------------
# plot_gew_full <- dat_plot %>% 
#   drop_na(emotion)%>%
#   filter( Wheel.task == "task") %>% 
#   ggplot(aes(x = x_cen, y = y_cen)) +
#   ggpubr::background_image(bg) +
#   geom_point(alpha = 0.5, size = 2) +
#   ggh4x::facet_nested(Wheel.name + intensity ~ emotion) +
#   coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300)) +
#   theme_minimal() +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.title.x = element_blank(),
#         strip.text.x = element_text(size = 14, face = "bold"),
#         strip.text.y = element_text(size = 14, face = "bold"),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill = "white", color = NA)) +
#   scale_fill_manual(values = c("NA", "white"))

# Plot Angle subtle vs full ----------------------------------------------
CircularMean <- dat_fit%>%
  mutate(circulardiff = circular(diff, units = "degrees" ))%>%
  dplyr::select(id,circulardiff,video_set,emotion,Pt.group)%>%
  'colnames<-'(c("subject","degree" ,"video_set", "emotion","group"))%>%
  drop_na(degree)

plot_angle_a<- CircularMean %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>%
  group_by(subject,group,emotion,video_set) %>%
  summarise(circular_mean = mean.circular(degree)) %>% 
  ggplot(aes(x = circular_mean, y = emotion, fill = group, color = video_set, shape = video_set )) +
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
  summarise(circular_mean = mean.circular(degree)) %>% 
  ggplot(aes(x = circular_mean, y = emotion, shape = video_set)) +
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

# Plot Rho subtle vs full ----------------------------------------------
kappa_mean <- dat_fit%>%
  dplyr::select(id,degree,video_set,emotion,Pt.group)%>%
  'colnames<-'(c("subject","degree","video_set", "emotion","group"))%>%
  drop_na(degree)%>%
  group_by(subject,group,emotion,video_set)%>%
  summarise(kappa_mean = rho.circular(degree),
            log_rho = log(kappa_mean))

plot_Rho_a<- kappa_mean %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>%
  group_by(subject,emotion,group,video_set) %>%
  summarise(variance = 1-kappa_mean) %>% 
  ggplot(aes(x = variance, y = emotion, fill = video_set, shape = group )) +
  geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 0.5) +
  stat_halfeye( size = 1) +
  facet_grid(group ~ . ) +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.8, 0.15)) +
  xlab("Uncertainty") +
  labs(fill = "Group",
       shape = "video_set") 


plot_Rho_b <- kappa_mean %>% 
  clean_emotion_names(emotion) %>%
  group_by(subject,emotion, group) %>%
  summarise(variance = 1-kappa_mean) %>% 
  ggplot(aes(x = variance, y = emotion, shape = group)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", size = 0.5) +
  stat_halfeye(size = 1) +
  facet_grid(group ~ . ) +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none") +
  xlab(latex2exp::TeX("$\\Delta_{video_set}$ Uncertainty$"))

plot_uncertainty <- plot_grid(plot_Rho_a, plot_Rho_b, 
                                  labels = "AUTO", rel_widths = c(3, 2), align = "hv")


# Plot Intensity subtle vs full ------------------------------------------
intensity_mean <-  dat_fit %>%
  mutate(correct = ifelse(emotion == resp_emotion_label, "correct", "error"),
         correct = as.factor(correct)) %>%
  dplyr::select(id, magnitude, video_set, emotion, Pt.group, correct) %>%
  'colnames<-'(c("subject", "intensity" ,"video_set", "emotion", "group", "correct")) %>%
  group_by(subject, group, emotion, video_set, correct) %>%
  summarise(int_mean = mean(intensity)) %>%
  drop_na(int_mean)

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

plot_intensity <- plot_grid(plot_intensity_a, plot_intensity_b, 
                           labels = "AUTO", rel_widths = c(3, 2), align = "hv")

# Categorical Responses ---------------------------------------------------

# order as the wheel
dat$resp_emotion_label <- factor(dat$resp_emotion_label, levels = emo_coords$emo_order)

dat_summ <- dat %>% 
  drop_na(emotion)%>%
  mutate(video_set = Video.intensity)%>%
  mutate(video_set = ifelse(video_set == "full","ADFES" , "JeFFE" ))%>%
  filter(emotion != "neutrality") %>% 
  group_by(emotion,video_set, resp_emotion_label,Pt.group) %>% 
  summarise(n = n())

plot_gew_discrete <- dat_summ %>% 
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

# Saving ------------------------------------------------------------------

plot_list <- make_named_list(plot_gew_legend_neutral, 
                             plot_gew_emotions,
                             plot_gew_discrete,
                             plot_mean, 
                             plot_uncertainty,
                             plot_intensity )

saveRDS(plot_list, file = "objects/paper_plots.rds")

# Good size for posterior plots

mkdir_if("figures/png")
mkdir_if("figures/pdf")
mkdir_if("figures/tiff")

width <- 16
height = 10

for(i in 1:length(plot_list)){
  plot_name <- names(plot_list)[i]
  if(grepl("int", names(plot_list[i]))){
    ggsave_plot(plot_list[[i]], 
                name = file.path("figures", "pdf", plot_name), 
                device = "pdf",
                width = width, 
                height = height)
    ggsave_plot(plot_list[[i]],
                device = "png",
                name = file.path("figures", "png", plot_name), 
                width = width, 
                height = height)
  }
}

# GEW Plots

ggsave_plot(plot_list$plot_gew_legend_neutral,
            name = file.path("figures", "png", "plot_gew_legend_neutral"),
            device = "png", width = 16, height = 9)

ggsave_plot(plot_list$plot_gew_legend_neutral,
            name = file.path("figures", "pdf", "plot_gew_legend_neutral"),
            device = "pdf", width = 16, height = 9)

ggsave_plot(plot_list$plot_gew_discrete,
            name = file.path("figures", "png", "plot_gew_discrete"),
            device = "png", width = 15, height = 10)

ggsave_plot(plot_list$plot_gew_discrete,
            name = file.path("figures", "pdf", "plot_gew_discrete"),
            device = "pdf", width = 15, height = 10)

ggsave_plot(plot_list$plot_gew_emotions,
            name = file.path("figures", "png", "plot_gew_emotions"),
            device = "png", width = 15, height = 10)

ggsave_plot(plot_list$plot_gew_emotions,
            name = file.path("figures", "pdf", "plot_gew_emotions"),
            device = "pdf", width = 15, height = 10)

#################################################
# 
# END
#
#################################################