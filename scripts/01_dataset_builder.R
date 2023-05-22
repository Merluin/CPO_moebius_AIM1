###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#  Description: Generate the dataset from Gorilla (https://app.gorilla.sc/) 
#  Experiment CPO_moebius_AMIM1
#
#  Update:      23/08/2022
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)

# Functions ---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------

datasetname<-"dataset"
dataset_moebius(datasetname)
load(file.path("data", paste0(datasetname,".rds")))

# Coordinates -------------------------------------------------------------

dat <- data %>% 
  mutate(x_cen = Wheel.x,
         y_cen = Wheel.y,
         magnitude = calc_dist(point_x = x_cen, point_y = y_cen),
         x_cen = ifelse(x_cen == 0, x_cen + 0.0001, x_cen),
         y_cen = ifelse(y_cen == 0, y_cen + 0.0001, y_cen),
         theta = atan(y_cen/x_cen),
         theta = correct_angle(theta, x_cen, y_cen), # correcting for quadrant
         degree = rad_to_deg(theta)) # convert to degrees

# Here we create a dataframe with the "correct" value for each emotion. In this way
# we can calculate the difference between the pressed angle and the correct one

coords <- tibble(
  emotion = c("satisfaction", "happiness", "elation", "pride", "anger", "contempt",
          "disgust", "envy", "guilt", "shame", "fear", "sadness", "surprise", 
          "interest", "hope", "relief")
)

coords <- coords %>% 
  mutate(theta_emo = (2 * 0:(nrow(.) - 1) * pi)/nrow(.), # see https://math.stackexchange.com/a/206662
         theta_emo = theta_emo + deg_to_rad((360/nrow(.))/2), # adding the shift for centering emotions
         x_emo = 300 * cos(theta_emo),
         y_emo = 300 * sin(theta_emo),
         degree_emo = rad_to_deg(theta_emo))

# Here we calculate the angular difference between the correct and the pressed angle

names(coords$degree_emo) <- coords$emotion # renaming for expanding

dat <- dat %>% 
  mutate(emotion_angle = coords$degree_emo[.$emotion], # expanding the coords
         diff = ang_diff(emotion_angle, degree))

# combining the coords with the dat

dat <- left_join(dat, coords, by = "emotion")

# assigning order to coord

coords$order <- c(4, 3, 2, 1, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5)
coords$emo_order <- coords$emotion[coords$order]

# Adding label to emotion/intensity ---------------------------------------

dat<- dat%>% 
  mutate(resp_emotion_label = seg_position(round(degree, 1)),
         resp_intensity_label = level_int_position(magnitude),
         resp_intensity_ord = parse_number(resp_intensity_label),
         id = as.numeric(Pt.code))

dat_fit <- dat %>% 
  filter(Wheel.task == "task", Wheel.name == "GW1" ,emotion != "neutrality") %>% 
  mutate(diff_theta = unname(deg_to_rad(diff)),
         emotion = factor(emotion),
         video_set = Video.intensity)%>%
  mutate(video_set = ifelse(video_set == "full","ADFES" , "JeFFE" ))

# Saving ------------------------------------------------------------------

saveRDS(dat,file.path("data", paste0(datasetname,"_valid.rds")))
saveRDS(coords, file = file.path("objects", "emo_coords.rds"))
saveRDS(dat_fit, file = file.path("data",paste0(datasetname,"_fit.rds")))

#################################################
# 
# END
#
#################################################a