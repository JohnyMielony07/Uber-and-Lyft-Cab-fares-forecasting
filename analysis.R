rm(list = ls()); gc()
set.seed(0)

# Loading packages
library(tidyverse)
library(magrittr)
library(lubridate)
library(gridExtra)
library(scales)

# Loading functions

source("R/utilities.R")

# Loading data
df_cab <- read.csv(file = 'data/cab_rides_utf8.csv', sep = ';', encoding = 'UTF-8', stringsAsFactors = F)
df_weather <- read.csv(file = 'data/weather_utf8.csv', sep = ';', encoding = 'UTF-8', stringsAsFactors = F)

# Data preprocessing
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

write.table(x = df, file = "data/Merged.csv", dec = ".", sep = ";", row.names = F)
rm(df_cab, df_weather); invisible(gc())

# Peek at the dataset
## General info
cat("Dataset dimensions:", dim(df))
glimpse(df)

## Number of unique values per variable
var_names <- names(df)
cnt_per_var <- data.frame(var = var_names, cnt = apply(df, 2, get_uniqe_counts)) %>% arrange(cnt)
cnt_per_var %>%
    filter(var != 'id') %>%
    ggplot(aes(x = reorder(var, cnt), y = cnt)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = cnt), vjust = -1, color = "black", size=3.5) +
    labs(x = 'Number of unique values per variable', y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Number of missing data per variable
NA_count <- apply(is.na(df), 2, sum)
NA_DF <- data.frame(variable = names(df), missing = NA_count)
NA_DF$share <- round(((NA_DF$missing/nrow(df))*100),1)
NA_DF %>%
    ggplot(aes(x = variable, y = share)) +
    geom_bar(stat='identity') + coord_flip(y=c(0,110)) +
    labs(x = "", y = "Percentage of missing data") +
    geom_text(aes(label=paste0(NA_DF$share, "%"), hjust=-0.1))
### Rain NA - no rain
### Prce NA - ?

# EDA
# Hypothesis: Fare(district) ~ Current Demand(district) + Trip length + Standard of service + Number of passangers
# Hypothesis: Current Demand(district) ~ weather + day of week + time + district type

## Price
summary(df$price)

h1 <- df %>%
    filter(!is.na(price)) %>%
    ggplot(aes(price)) + 
    geom_histogram(binwidth = 2.5, color="black", fill="white") +
    geom_vline(aes(xintercept = mean(price)), color = "blue", linetype = "dashed", size = 1) +
    xlab("Price") +
    ylab("Count")

h2 <- df %>%
    filter(!is.na(price)) %>%
    ggplot(aes("var", price)) +
    geom_boxplot(outlier.alpha = .25) +
    scale_y_log10(labels = dollar, breaks = quantile(df$price ,na.rm = T)) +
    ylab("log(Price")

grid.arrange(h1, h2, ncol = 2)
### Right-side skewness, log can possibly improve prediction 
### One potential outlier - max value 
