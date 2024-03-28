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
  filedemo<-list.files("original_data/demography",pattern= '.csv') 
  demo<- read.csv(file.path("original_data/demography",filedemo), sep=";", header=TRUE,stringsAsFactors = FALSE,na.strings= "aa")%>%
    filter(GRUPPO != "bell" & GRUPPO != "")
  
  # combine Psychopy and Demographic datasets 
  # ID.subject is used to combine datasets
  demo$ID <- ifelse(demo$ID >10,demo$ID-10,demo$ID)
  demo$ID.subject <- sprintf("%s_%s", demo$ID, demo$GRUPPO)
  dataset2<-left_join(dataset,demo%>%dplyr::select(-c(ID,ETA,SCOLARITA)), by = "ID.subject")
  

cols<-c("ID.subject",
        "Exp.date",
        "Exp.trial",
        "Pt.code" ,
        "Pt.gender",
        "Pt.study",
        "Pt.age",
        "Pt.paralisi",
        "Pt.sunnybrookb",
        "Pt.group",
        "Pt.match",
        "FDI.fisica",
        "FDI.sociale",
        "TAS.20",
        "AQ",
        "OFMT",
        "LPOST.TOT",
        "LPOST.1",
        "LPOST.2",
        "LPOST.3",
        "LPOST.4",
        "LPOST.5",
        "LPOST.6",
        "LPOST.7",
        "LPOST.8",
        "LPOST.9",
        "LPOST.10",
        "LPOST.11",
        "LPOST.12",
        "LPOST.13",
        "LPOST.14",
        "LPOST.15",
        "Wheel.name", 
        "Wheel.rt", 
        "Wheel.x", 
        "Wheel.y", 
        "Wheel.task", 
        "Video.name", 
        "Video.intensity", 
        "Video.gender", 
        "Video.emotion", 
        "Video.id")

Pct<-dataset2%>%
  filter(loop_practice.thisRepN>=0)%>%
  dplyr::select("ID.subject","date","trial","subject","GENERE","education","age","PARALISI","SUNNYBROOK","group",
                "MATCH","FDI_fisica","FDI_sociale","TAS_20","AQ","OFMT","L_POST_TOT","L_POST_1","L_POST_2",
                "L_POST_3","L_POST_4","L_POST_5","L_POST_6","L_POST_7","L_POST_8","L_POST_9","L_POST_10",
                "L_POST_11","L_POST_12","L_POST_13","L_POST_14","L_POST_15", "practice","primary.time","primary.x",
                "primary.y","file_duration", "file", "file_emotion_level","file_gender","file_emotion", "file_id")%>%
    'colnames<-'(cols)%>%
    mutate(Wheel.name = "GW1" ,
           Wheel.task = "practice")

  
Gw1<-dataset2%>%
  filter(exp_blocks.thisRepN >= 0)%>%
  dplyr::select("ID.subject","date","trial","subject","GENERE","education","age","PARALISI","SUNNYBROOK","group",
                "MATCH","FDI_fisica","FDI_sociale","TAS_20","AQ","OFMT","L_POST_TOT","L_POST_1","L_POST_2",
                "L_POST_3","L_POST_4","L_POST_5","L_POST_6","L_POST_7","L_POST_8","L_POST_9","L_POST_10",
                "L_POST_11","L_POST_12","L_POST_13","L_POST_14","L_POST_15", "practice","primary.time","primary.x",
                "primary.y","file_duration", "file", "file_emotion_level","file_gender","file_emotion", "file_id")%>%
    'colnames<-'(cols)%>%
    mutate(Wheel.name = "GW1",
           Wheel.task = "task")%>%
    drop_na(Video.id)

Gw2<-dataset2%>%
  filter(exp_blocks.thisRepN >= 0)%>%
  dplyr::select("ID.subject","date","trial","subject","GENERE","education","age","PARALISI","SUNNYBROOK","group",
                "MATCH","FDI_fisica","FDI_sociale","TAS_20","AQ","OFMT","L_POST_TOT","L_POST_1","L_POST_2",
                "L_POST_3","L_POST_4","L_POST_5","L_POST_6","L_POST_7","L_POST_8","L_POST_9","L_POST_10",
                "L_POST_11","L_POST_12","L_POST_13","L_POST_14","L_POST_15", "practice","secondary.time","secondary.x","secondary.y",
         "file_duration", "file", "file_emotion_level", "file_gender",
         "file_emotion", "file_id")%>%
  'colnames<-'(cols)%>%
  mutate(Wheel.name = "GW2",
         Wheel.task = "task")%>%
  drop_na(Video.id)


data <- rbind(Pct,Gw1, Gw2)%>%
  drop_na(Pt.gender)%>%
    mutate(
           Exp.trial = as.numeric(Exp.trial),
           Pt.group = as.factor(Pt.group),
           Pt.code = as.factor(Pt.code),
           Pt.gender = ifelse(Pt.gender == "M","Uomo","Donna"),
           Pt.gender = as.factor(Pt.gender),
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
           Wheel.rt = round(Wheel.rt*1000,6),
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

  

  
  

  save(data,file= file.path("objects",paste0(dataset_name,".RData")))
  
  
} #end function  

#################################################
# 
# END
#
#################################################