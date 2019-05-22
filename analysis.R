rm(list = ls())
gc()
set.seed(0)

#Loading packages
library(tidyverse)
library(magrittr)
library(lubridate)

#Loading functions

#Loading data
df_cab <- read.csv(file = 'data/cab_rides_utf8.csv', sep = ';', encoding = 'UTF-8', stringsAsFactors = F)
df_weather <- read.csv(file = 'data/weather_utf8.csv', sep = ';', encoding = 'UTF-8', stringsAsFactors = F)

head(df_cab, n = 5)
head(df_weather, n = 5)

#Data preprocessing
df_cab %<>%
    mutate(date_time = as_datetime(df_cab$time_stamp/1000, tz = 'UTC'))

#TODO: time features; join weather data


#Peek at the dataset 
glimpse(df_cab)
glimpse(df_weather)



