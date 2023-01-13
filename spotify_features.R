library(rvest)
library(tidyverse)
library(spotifyr)

# The idea for this project has been inspired by paper
# Sciandra, Mariangela; Spera, Irene; 2020/01/01; 
# "A Model Based Approach to Spotify Data Analysis: A Beta GLMM"; 
# 10.2139/ssrn.3557124; SSRN Electronic Journal

# The aim of this paper is to investigate if audio features from Spotify 
# can be considered as determinants of the stream popularity of songs.

# In here the extracted data will consider different artist and
# the explained variable will be bounded popularity which can have values from 0 to 100
# however the values are extracted the way the boundary values are not really possible
# so the explained variable can be considered as continuous.

Sys.setenv(SPOTIFY_CLIENT_ID = "7279343fb60242fe8ee1194a4f7fe5df")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "10b0750b71d34615bf5e3396097b9b23")
access_token <- get_spotify_access_token()

get_audio_features <- function(artist_name){
  
  artist <- search_spotify(artist_name, type = "artist", market = "US", limit = 1)
  top <- get_artist_top_tracks(artist$id, 
                               market = "US") %>% 
    select(name, id, popularity)
  
  top.features <- data.frame()
  for (id in top$id){
    top.features <- get_track_audio_features(id) %>% 
      bind_rows(top.features)
  } 
  
  top <- top %>% 
    left_join(top.features) %>% 
    select(-c(15:18, 20)) %>% 
    mutate(followers.total = artist$followers.total) %>% 
    mutate(genres = artist$genres)
  
  rm(artist, top.features)
  
  return(top)
}

artist.data <- bind_rows(
               get_audio_features("LadyGaga"),
               get_audio_features("Rihanna"),
               get_audio_features("ArianaGrande")
)