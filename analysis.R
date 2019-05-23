rm(list = ls()); gc()
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
df_weather %<>%
    rename(temp = X.U.FEFF.temp) %>%
    mutate(date_time = as_datetime(df_weather$time_stamp, tz = 'UTC'),
           date_time_rounded = round_date(date_time, unit = 'hour'),
           index = str_c(location, ' - ', date(date_time_rounded), ' - ', hour(date_time_rounded))) %>%
    select(-time_stamp, -date_time, -location, -date_time, -date_time_rounded) %>%
    group_by(index) %>%
    slice(1) %>%
    ungroup()

df <- df_cab %>%
    rename(distance = X.U.FEFF.distance) %>%
    mutate(date_time = as_datetime(df_cab$time_stamp/1000, tz = 'UTC'),
           date_time_rounded = round_date(date_time, unit = 'hour'),
           index = str_c(source, ' - ', date(date_time_rounded), ' - ', hour(date_time_rounded))) %>%
    select(-time_stamp) %>%
    left_join(df_weather, by = 'index') %>%
    filter(!is.na(temp)) %>%
    select(-date_time_rounded, -index)

df %<>% mutate(month = month(date_time),
               day = day(date_time),
               hour = hour(date_time),
               minute = minute(date_time),
               day_of_week = wday(date_time),
               week_of_year = week(date_time),
               is_weekend = ifelse(day_of_week %in% c(6,7), 1, 0))

rm(df_cab, df_weather); invisible(gc())

