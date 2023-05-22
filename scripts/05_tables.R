###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS from previous GAMBAROTA scripts (https://github.com/shared-research/face-mask-gew.git) 
#  Date:        0382022
#  Description: Generate the dataset from Gorilla (https://app.gorilla.sc/) 
#  Experiment CPO_moebius_AMIM1
#
#  Update:      23/08/2022
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(dplyr)
library(flextable)
library(forcats)
library(officer)
library(ftExtra)
library(tidybayes)
library(tidyr)
library(here)
library(purrr)
library(magrittr)

# Functions ---------------------------------------------------------------

devtools::load_all()

save_table <- function(table, path){
  save_as_docx(table, path = path)
}

flextable_with_param <- function(data){
  data %>% 
    flextable() %>% 
    autofit() %>% 
    theme_vanilla() %>% 
    colformat_md(part = "all") %>% 
    fontsize(part = "all", size = 9)
}

get_post_summary <- function(data, group, sign = FALSE, null = 0){
  group <- rlang::enexpr(group)
  out <- data %>% 
    group_by(!!group) %>% 
    median_hdci(value) %>% 
    select(emotion, value, .lower, .upper)
  if(sign){
    out %>% 
      mutate(across(where(is.numeric), round, 3),
             value_chr = sprintf("**%s** [%s, %s]", value, .lower, .upper),
             value_chr = ifelse(.lower <= null & null <= .upper,
                                value_chr,
                                paste(value_chr, "*")))
  }else{
    out
  }
}

set_emotion_order <- function(data, col, levels, dpar = TRUE){
  col <- rlang::enexpr(col)
  data %>% 
    mutate(!!col := factor(!!col, levels = levels)) %>% {
      if(dpar){
        arrange(., main_param, !!col)
      }else{
        arrange(., !!col)
      }
    }
}

# Loading Data ------------------------------------------------------------

datasetname <- "dataset"

dat <- readRDS(file.path("data", paste0(datasetname,"_valid.rds")))
dat <- dat%>% 
  na.omit()%>%
  mutate(intensity = Video.intensity)

emo_coords <- readRDS(file.path("objects", "emo_coords.rds"))
intensity_objects <- readRDS(file.path("objects", "intensity_objects.rds"))
circular_objects <- readRDS(file.path("objects", "circular_objects.rds"))

emo_order = c("Surprise", "Sadness", "Happiness", "Fear", "Disgust", "Anger")

dat_fit <- readRDS(file = file.path("data",paste0(datasetname,"_fit.rds")))

# Demographic Table ---------------------------------------------------------------
demo<- dat_fit%>%
  group_by(Pt.code) %>% 
  mutate(Exp.trial = 1:n()) %>% 
  ungroup()%>%
  filter(Exp.trial==1)%>%
  dplyr::select(contains("Pt"))%>%
  mutate(Pt.gender = ifelse(Pt.gender == "Uomo","Male","Female"))%>%
  'colnames<-'(c("Subject","Gender","Education" ,"age", "Sunnybrook","Group"))%>%
  dplyr::select(Subject,Group,Gender,age,Education,Sunnybrook)%>%
  flextable_with_param() %>% 
  align(part = "header", align = "center") %>% 
  align( part = "body", align = "center")

# EDA Table ---------------------------------------------------------------

# the mean is the angular mean in radians. the computation is the same as
# using:
# ang <- circular::circular(dat$angle, units = "degrees", modulo = "2pi")
# circular::mean.circular(dat$angle)
# with less computation
# test: rad_to_deg(CircStats::circ.mean(dat$theta)) %% 360

tab_eda <- dat %>% 
  mutate(video_set = Video.intensity,
         group = Pt.group,
         video_set = ifelse(video_set == "full","ADFES" , "JeFFE" ),
         group = ifelse(group == "moebius","Moebius" , "Control" ))%>%
  group_by(group, emotion,  video_set) %>%
  summarise(m_angle = rad_to_deg(CircStats::circ.mean(theta)) %% 360,
            var_angle = 1 - CircStats::circ.disp(theta)$var,
            m_int = mean(magnitude),
            sd_int = sd(magnitude)) %>% 
  left_join(., emo_coords %>% dplyr::select(emotion, degree_emo), by = "emotion") %>% 
  dplyr::select(emotion, degree_emo, group, everything()) %>% 
  clean_emotion_names(emotion) %>% 
  mutate(emotion = factor(emotion)) %>% 
  arrange(emotion) %>% 
  flextable_with_param() %>% 
  colformat_double(digits = 2) %>% 
  colformat_double(j = 2, digits = 0) %>% 
  theme_vanilla() %>% 
  set_header_labels(values = list(
    emotion = "Emotion",
    degree_emo = "Wheel Angle°",
    group = "Group",
    video_set = "Video set",
    m_angle = "Mean°",
    var_angle = "Variance",
    m_int = "Mean",
    sd_int = "SD"
  )) %>% 
  add_header_row(values = c( "","", "", "",
                            rep(c("Angle", "Perceived Intensity"), each = 2))) %>% 
  merge_v(j = c(1:3)) %>% 
  merge_h(part = "header") %>% 
  align(align = "center", part = "all")

tab_eda%>%
  save_as_image(path = file.path("tables", "tab_eda.png"))

# Angle/kappa Full vs Subtle ----------------------------------------------




tab_kappa_angle_intensity_effect <- dat_fit%>%
  mutate(circulardiff = circular(diff, units = "degrees" ))%>%
  dplyr::select(id,Pt.group,emotion,video_set,circulardiff)%>%
  'colnames<-'(c("subject","group","emotion" ,"video_set", "err_mean"))%>%
  drop_na(err_mean)%>%
  group_by(subject,group,emotion,video_set)%>%
  summarise(err_mean = mean.circular(err_mean))%>%
  spread(video_set,err_mean)%>%
  mutate(set_diff = ang_diff(ADFES,JeFFE))
  
  summarise(angle_full = mean(full, na.rm = TRUE),
            angle_subtle = mean(subtle, na.rm = TRUE),
            angle_diff = mean(angle_diff, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(emotion, angle_full, angle_subtle, angle_diff, .draw) %>%
  na.omit()%>%
  pivot_longer(2:(ncol(.)-1), names_to = "param", values_to = "value") %>% 
  group_by(param) %>% 
  nest() %>% 
  mutate(null = ifelse(startsWith(param, "kappa"), 1, 0)) %>% 
  mutate(summary = map2(data, null, function(x, y) get_post_summary(x, emotion, sign = TRUE, null = y))) %>% 
  unnest(summary) %>% 
  select(param, emotion, value_chr) %>% 
  mutate(main_param = ifelse(startsWith(param, "kappa"), "Uncertainty", "Bias"),
         contr_param = case_when(grepl("full", param) ~ "Intensity~full~",
                                 grepl("subtle", param) ~ "Intensity~subtle~",
                                 TRUE ~ "Contrast"),
         emotion = as.character(emotion)) %>% 
  ungroup() %>% 
  select(-param) %>%
  pivot_wider(names_from = contr_param, values_from = value_chr) %>%
  clean_emotion_names(emotion) %>% 
  set_emotion_order(emotion, levels = emo_order) %>% 
  flextable_with_param() %>% 
  align(part = "header", align = "center") %>% 
  align(j = 2, part = "body", align = "center") %>% 
  merge_v(2) %>% 
  set_header_labels(values = list(
    emotion = "Emotion",
    main_param = "Parameter"))

# Int full vs subtle ------------------------------------------------------
CircularMean <- dat_fit%>%
  mutate(correct = ifelse(emotion == resp_emotion_label, 1,0),
         correct = as.factor(correct),
         circulardiff = circular(diff, units = "degrees" ))%>%
  dplyr::select(id,circulardiff,magnitude,video_set,emotion,Pt.group,correct)%>%
  'colnames<-'(c("subject","degree","magnitude" ,"video_set", "emotion","group","correct"))%>%
  drop_na(degree)%>%
  group_by(subject,group,emotion,video_set,correct)%>%
  summarise(circular_mean = mean.circular(degree),
            rho_mean = rho.circular(degree),
            mag_mean = mean(magnitude))

tab_int_intensity_effect <-  b

# Accuracy GEW ------------------------------------------------------------

tab_acc_gew <- dat %>% 
    filter(emotion != "neutrality") %>% 
    mutate(acc = ifelse(emotion == resp_emotion_label, 1, 0)) %>%
    group_by(Pt.group,emotion, intensity) %>% 
    summarise(acc = mean(acc)) %>% 
    clean_emotion_names(emotion) %>% 
    set_emotion_order(emotion, emo_order, FALSE) %>% 
    pivot_wider(names_from = emotion, values_from = acc) %>% 
    flextable() %>% 
    colformat_double(digits = 2) %>% 
    autofit() %>% 
    merge_v(j = 1:2) %>% 
    theme_vanilla() %>% 
    align(align = "center") %>% 
    set_header_labels(values = list(
      "intensity" = "Intensity"
    ))
  
# Saving ------------------------------------------------------------------

tab_list <- make_named_list(tab_eda, tab_kappa_angle_intensity_effect, tab_int_intensity_effect,
                            tab_acc_gew, demo)

tab_files <- paste0(names(tab_list), ".docx")

saveRDS(tab_list, file = here("objects", "paper_tables.rds"))
purrr::walk2(tab_list, file.path("tables", tab_files), save_table)

#################################################
# 
# END
#
#################################################