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

# Loading Data ------------------------------------------------------------

datasetname <- "dataset"

dat <- readRDS(file.path("data", paste0(datasetname,"_valid.rds")))

emo_coords <- readRDS(file.path("objects", "emo_coords.rds"))

emo_order = c("Surprise", "Sadness", "Happiness", "Fear", "Disgust", "Anger")

dat_fit <- readRDS(file = file.path("data",paste0(datasetname,"_fit.rds")))

# Demographic Table ---------------------------------------------------------------

demo<- dat_fit%>%
  group_by(Pt.code) %>% 
  mutate(Exp.trial = 1:n()) %>% 
  ungroup()%>%
  filter(Exp.trial==1)%>%
  dplyr::select(Pt.group,Pt.code,Pt.match, Pt.gender, Pt.age, Pt.study,Pt.paralisi ,Pt.sunnybrookb,
                FDI.fisica,FDI.sociale,TAS.20,AQ,OFMT,LPOST.TOT)%>%
  'colnames<-'(c("Group","Subject","Match","Gender","Age" ,"Education","Paralisi" ,"Sunnybrook","FDI fisica",
                 "FDI sociale", "TAS 20", "AQ", "OFMT", "L POST TOT"))

  
demo_tab <- demo%>%  
  flextable_with_param%>%
  merge_v(j = 1)%>%
  align(part = "header", align = "center") %>% 
  align( part = "body", align = "center")

# Participants table
pt <- demo%>%
  mutate(n = 1)%>%
  dplyr::select(Group,Subject,Gender,n,Age,Education)%>%
  group_by(Group,Gender) %>% 
  summarise(n = sum(n),
            Age_mean = mean(Age),
            Age_Sd = sd(Age),
            Education_mean = mean(Education),
            Education_Sd = sd(Education))
  

# add total
pt_tab <- rbind(pt, data.frame(Group = NA, Gender ='Total',t(colSums(pt[,3])), t(colMeans(pt[, -c(1,2,3)]))))%>% 
  'colnames<-'(c("Group","Gender","n","Age~mean~", "Age~sd~","Education~mean~","Education~sd~" ))%>%
  flextable_with_param%>%
  merge_v(j = 1)%>%
  colformat_double(digits = 2) %>% 
  align(part = "header", align = "center") %>% 
  align( part = "body", align = "center")%>%
  bold(i = 5, bold = TRUE)




# EDA Table ---------------------------------------------------------------

# the mean is the angular mean in radians. the computation is the same as
# using:
# ang <- circular::circular(dat$angle, units = "degrees", modulo = "2pi")
# circular::mean.circular(dat$angle)
# with less computation
# test: rad_to_deg(CircStats::circ.mean(dat$theta)) %% 360

# Calculate circular mean for neutral dataset

dat_neutral <- dat %>% 
  filter(Wheel.task == "task", Wheel.name == "GW1" ,emotion == "neutrality") %>% 
  mutate(video_set = Video.intensity,
         video_set = ifelse(video_set == "full","ADFES" , "JeFFE" ),
         emotion = "neutral",
         degree_emo = "no angle")%>%
  dplyr::select(id,emotion,degree_emo,Pt.group,video_set,theta,magnitude)%>%
  na.omit()%>%
  group_by(emotion,degree_emo,Pt.group,video_set)%>%
  summarise(theta = rad_to_deg(CircStats::circ.mean(theta)) %% 360,
            var_angle = 1 - CircStats::circ.disp(theta)$var,
            mean_int = mean(magnitude, na.rm=TRUE),
            int_sd = sd(magnitude, na.rm=TRUE))%>%
  'colnames<-'(c("emotion","degree_emo","group" ,"video_set","m_angle","var_angle", "m_int","sd_int"))
  


tab_eda <- dat_fit %>% 
  mutate(group = ifelse(group == "moebius","Moebius" , "Control" ))%>%
  group_by(group, emotion,  video_set) %>%
  na.omit()%>%
  summarise(m_angle = rad_to_deg(CircStats::circ.mean(theta)) %% 360,
            var_angle = 1 - CircStats::circ.disp(theta)$var,
            m_int = mean(magnitude),
            sd_int = sd(magnitude)) %>% 
  left_join(., emo_coords %>% dplyr::select(emotion, degree_emo), by = "emotion") %>% 
  dplyr::select(emotion, degree_emo, group, everything()) %>% 
  clean_emotion_names(emotion) %>% 
  mutate(emotion = factor(emotion),
         degree_emo = as.character(as.factor(degree_emo))) %>% 
  arrange(emotion) %>% 
  rbind(.,dat_neutral)%>%
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


  
# Saving ------------------------------------------------------------------

tab_list <- make_named_list(tab_eda,demo_tab, pt_tab,tab_eda)

tab_files <- paste0(names(tab_list), ".docx")

saveRDS(tab_list, file = here("objects", "paper_tables.rds"))
purrr::walk2(tab_list, file.path("tables", tab_files), save_table)

#################################################
# 
# END
#
#################################################