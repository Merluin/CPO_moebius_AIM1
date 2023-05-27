#################################################
# 
# Name:           Psychopy dataset
# Programmer:     Thomas Quettier
# Date:           18/11/2021
# Description:    Put together several csv files
#
#################################################
dataset_moebius <- function(dataset_name)
{
  
  # packages
  library(tidyverse)
  library(circular)
  devtools::load_all()
  
  # find nb of file
  folder_dir<-file.path("original_data")
  
  
  # Concatenate all file from Psychopy
  # Files are .csv placed in "original_data" folder
  dataset<-list.files(path=folder_dir, full.names = TRUE,pattern='csv') %>%
    lapply(.,function(x) read.csv(x, sep=",", header=TRUE, stringsAsFactors = FALSE ))%>%
    lapply(clean_practice)%>%
    bind_rows()
  
  dataset<- dataset%>%
    mutate(subject = ifelse(tolower(group) == "moebius",participant,participant+length(unique(participant))))%>%
    group_by(subject) %>% 
    mutate(trial = 1:n()) %>% 
    ungroup()
  
  # Demographic Dataset from google sheet
  # Files are .csv placed in "original_data/demography" folder
  filedemo<-list.files("original_data/demography",pattern= 'Partecipanti') 
  demo<- read.csv(file.path("original_data/demography",filedemo), sep=",", header=TRUE,stringsAsFactors = FALSE,na.strings= "aa")%>%
    filter(group != "bell" & group != "")%>%
    dplyr::select(-c(Nome,Cognome))
  
  # combine Psychopy and Demographic datasets 
  # ID.subject is used to combine datasets
  id <- parse_number(demo$ID.subject)
  demo$ID.subject <- sprintf("%s_%s", id, demo$group)
  dataset2<-left_join(dataset,demo%>%dplyr::select(ID.subject,Sunnybrook), by = "ID.subject")
  
  
Pct<-dataset2%>%
  filter(loop_practice.thisRepN>=0)%>%
  dplyr::select("ID.subject","date","trial","subject","sex","education", "age","Sunnybrook","group",
           "practice","primary.time","primary.x","primary.y",
           "file_duration", "file", "file_emotion_level", "file_gender",
           "file_emotion", "file_id")%>%
    'colnames<-'(c("ID.subject","Exp.date","Exp.trial","Pt.code" ,"Pt.gender","Pt.study","Pt.age","Pt.sb","Pt.group",
                   "Wheel.name", "Wheel.rt", "Wheel.x", "Wheel.y", "Wheel.task", 
                   "Video.name", "Video.intensity", "Video.gender", "Video.emotion", "Video.id"))%>%
    mutate(Wheel.name = "GW1" ,
           Wheel.task = "practice")

  
Gw1<-dataset2%>%
  filter(exp_blocks.thisRepN >= 0)%>%
  dplyr::select("ID.subject","date","trial","subject","sex","education", "age","Sunnybrook","group",
         "practice","primary.time","primary.x","primary.y",
         "file_duration", "file", "file_emotion_level", "file_gender",
         "file_emotion", "file_id")%>%
    'colnames<-'(c("ID.subject","Exp.date","Exp.trial","Pt.code" ,"Pt.gender","Pt.study","Pt.age","Pt.sb","Pt.group",
                   "Wheel.name", "Wheel.rt", "Wheel.x", "Wheel.y", "Wheel.task", 
                   "Video.name", "Video.intensity", "Video.gender", "Video.emotion", "Video.id"))%>%
    mutate(Wheel.name = "GW1",
           Wheel.task = "task")%>%
    drop_na(Video.id)

Gw2<-dataset2%>%
  filter(exp_blocks.thisRepN >= 0)%>%
  dplyr::select("ID.subject","date","trial","subject","sex","education", "age","Sunnybrook","group",
         "practice","secondary.time","secondary.x","secondary.y",
         "file_duration", "file", "file_emotion_level", "file_gender",
         "file_emotion", "file_id")%>%
  'colnames<-'(c("ID.subject","Exp.date","Exp.trial","Pt.code" ,"Pt.gender","Pt.study","Pt.age","Pt.sb","Pt.group",
                 "Wheel.name", "Wheel.rt", "Wheel.x", "Wheel.y", "Wheel.task", 
                 "Video.name", "Video.intensity", "Video.gender", "Video.emotion", "Video.id"))%>%
  mutate(Wheel.name = "GW2",
         Wheel.task = "task")%>%
  drop_na(Video.id)


data <- rbind(Pct,Gw1, Gw2)%>%
    mutate(
           Exp.trial = as.numeric(Exp.trial),
           Pt.code = as.factor(Pt.code),
           Pt.gender = ifelse(Pt.gender == "f","Donna","Uomo"),
           Pt.gender = as.factor(Pt.gender),
           Pt.group = as.factor(Pt.group),
           Pt.study = as.numeric(Pt.study),
           Pt.age = as.numeric(Pt.age),
           Wheel.name = as.factor(Wheel.name),
           Wheel.task = as.factor(Wheel.task),
           Wheel.x = str_remove_all(Wheel.x, "[\\[|\\] ']"),
           Wheel.y = str_remove_all(Wheel.y, "[\\[|\\] ']"),
           Wheel.rt = str_remove_all(Wheel.rt, "[\\[|\\] ']"),
           Wheel.x = as.numeric(Wheel.x),
           Wheel.y = as.numeric(Wheel.y),
           Wheel.rt = as.numeric(Wheel.rt),
           Wheel.rt = round(Wheel.rt*1000,1),
           Wheel.rt = replace_na(Wheel.rt,19999),
           Video.intensity = as.factor(Video.intensity),
           Video.gender = as.factor(Video.gender),
           emotion = as.factor(Video.emotion),
           Video.id = as.factor(Video.id))%>%
  group_by(Pt.code) %>% 
  mutate(Exp.trial = 1:n()) %>% 
  ungroup()%>%
  arrange(Pt.code)%>%
  mutate(emotion = case_when(emotion == "angry"~"anger",
                      emotion == "disgusted"~"disgust",  
                      emotion == "fear"~"fear",
                      emotion == "happy"~"happiness",
                      emotion == "neutral"~"neutrality",
                      emotion == "sad"~"sadness",
                      emotion == "surprised"~"surprise"))

  

  
  
  write.csv2(data, file= file.path("data",paste0(dataset_name,".csv")))
  save(data,file= file.path("data",paste0(dataset_name,".rds")))
  
  
} #end function  

#################################################
# 
# END
#
#################################################