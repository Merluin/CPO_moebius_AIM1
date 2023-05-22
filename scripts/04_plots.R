###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS from previous GAMBAROTA scripts (https://github.com/shared-research/face-mask-gew.git) 
#  Date:        0382022
#  Description: Generate plots
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

# Importing Data --------------------------------------------------------

datasetname<-"dataset"

emo_coords <- readRDS(file.path("objects", "emo_coords.rds"))
dat <- readRDS(file.path("data",paste0(datasetname,"_valid.rds")))
dat_fit <- readRDS(file = file.path("data",paste0(datasetname,"_fit.rds")))

# neutral resp & GEW Plots ---------------------------------------------------------------

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
            size = 5.5, fontface = "bold",
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

dat_plot <- dat %>% 
  dplyr::select(Pt.group,Wheel.task,emotion,Wheel.name, Video.intensity, x_cen, y_cen) %>% 
  mutate( video_set = stringr::str_to_title(Video.intensity),
          video_set = ifelse(video_set == "Full","ADFES" , "JeFFE" ),
          emotion = stringr::str_to_title(emotion),
          emotion = ifelse(emotion == "Neutrality", "Neutral", emotion),
          emotion = factor(emotion),
          emotion = forcats::fct_relevel(emotion, "Neutral"))

neutral_plot <- dat_plot %>% 
  filter(emotion == "Neutral" & Wheel.name == "GW1") %>% 
  ggplot(aes(x = x_cen, y = y_cen)) +
  ggpubr::background_image(bg) +
  geom_point(alpha = 0.5, show.legend = FALSE, size = 3) +
  ggh4x::facet_nested(video_set ~ emotion, switch="y") +
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
  facet_grid(Pt.group ~ video_set)

plot_gew_legend_neutral <- cowplot::plot_grid(neutral_plot, gew_legend, labels = "AUTO")


# Emotions resps Plots ---------------------------------------------------------------

plot_gew_emotions <- dat_plot %>% 
  filter(emotion != "Neutral", Wheel.task == "task") %>% 
  ggplot(aes(x = x_cen, y = y_cen, color = video_set)) +
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


# Categorical Responses ---------------------------------------------------

# order as the wheel 1
dat$resp_emotion_label <- factor(dat$resp_emotion_label, levels = emo_coords$emo_order)

dat_summ <- dat %>% 
  drop_na(emotion)%>%
  mutate(video_set = Video.intensity)%>%
  mutate(video_set = ifelse(video_set == "full","ADFES" , "JeFFE" ))%>%
  filter(emotion != "neutrality" & Wheel.name == "GW1") %>% 
  group_by(emotion,video_set, resp_emotion_label,Pt.group) %>% 
  summarise(n = n())

plot_gew1_discrete <- dat_summ %>% 
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

# order as the wheel 2

dat_summ <- dat %>% 
  drop_na(emotion)%>%
  mutate(video_set = Video.intensity)%>%
  mutate(video_set = ifelse(video_set == "full","ADFES" , "JeFFE" ))%>%
  filter(emotion != "neutrality" & Wheel.name == "GW2") %>% 
  group_by(emotion,video_set, resp_emotion_label,Pt.group) %>% 
  summarise(n = n())

plot_gew2_discrete <- dat_summ %>% 
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

plot_list <- make_named_list(gew_legend,
                             neutral_plot,
                             plot_gew_legend_neutral, 
                             plot_gew_emotions,
                             plot_gew1_discrete,
                             plot_gew2_discrete )

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
                name = file.path("figures", "png", plot_name))
  }
}

# GEW Plots

ggsave_plot(plot_list$plot_gew_legend_neutral,
            name = file.path("figures", "png", "plot_gew_legend_neutral"),
            device = "png", width = 16, height = 9)

ggsave_plot(plot_list$plot_gew_legend_neutral,
            name = file.path("figures", "pdf", "plot_gew_legend_neutral"),
            device = "pdf", width = 16, height = 9)

ggsave_plot(plot_list$plot_gew1_discrete,
            name = file.path("figures", "png", "plot_gew_discrete"),
            device = "png", width = 15, height = 10)

ggsave_plot(plot_list$plot_gew1_discrete,
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