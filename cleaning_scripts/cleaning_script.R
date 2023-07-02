


# Cleaning script

library(tidyverse)
library(janitor)
library(hms)
library(lubridate)


flights <- read_csv(here::here("data/flights.csv"))
weather_alt <- read_csv(here::here("data/export.csv"))
planes <-  read_csv(here::here("data/planes.csv"))
airlines <- read_csv(here::here("data/airlines.csv"))
airports <- read_csv(here::here("data/airports.csv"))


# flights cleaning

flights_alt <- flights %>% 
  rename(flight_duration = air_time) %>% 
  mutate(across(contains("_time"), ~ sprintf("%04d", .)),
         across(contains("_time"), ~ format(strptime(., format="%H%M"), format = "%H:%M")),
         across(contains("_time"), ~ as.POSIXct(., format = "%H:%M", tz = "UTC")),
         across(contains("_time"), ~ as_hms(.)),
         cancelled_flight = if_else(is.na(dep_time), TRUE, FALSE),
         delayed_flight = if_else(dep_delay > 0 | is.na(dep_delay), TRUE, FALSE),
         delayed_flight_over30 = if_else(dep_delay > 30 | is.na(dep_delay), TRUE, FALSE)) %>% 
  mutate(date = lubridate::make_date(year = year, month = month, day = day))


# weather cleaning

weather_alt_clean <- weather_alt %>% 
  select(-wpgt, -tsun) %>% 
  drop_na() %>% 
  rename(wind_dir = wdir) %>% 
  mutate(wind_dir2 = case_when(
    wind_dir <= 22.5 | wind_dir >= 337.5 ~ "North",
    wind_dir > 22.5 & wind_dir < 67.5 ~ "North East",
    wind_dir >= 67.5 & wind_dir <= 112.5 ~ "East",
    wind_dir > 112.5 & wind_dir < 157.5 ~ "South East",
    wind_dir >= 157.5 & wind_dir <= 202.5 ~ "South",
    wind_dir > 202.5 & wind_dir < 247.5 ~ "South West",
    wind_dir >= 247.5 & wind_dir <= 292.5 ~ "West",
    wind_dir > 292.5 & wind_dir < 337.5 ~ "North West",
    TRUE ~ "Other"
  )) 

# planes cleaning

planes_2 <- planes %>% 
  select(-speed) %>% 
  mutate(year = na_if(year, 0),
         engines = as.character(engines)) %>% 
  mutate(seats = case_when(
    seats < 11 ~ "10 or less",
    seats < 101 ~ "11 to 100",
    seats < 251 ~ "101 to 250",
    TRUE ~ "Over 250"
  )) %>% 
  rename(year_manufactured = year)


# joining datasets

flights_joined_alt <- flights_alt %>% 
  left_join(airlines, by = "carrier") %>% 
  left_join(weather_alt_clean, by = c("date")) %>% 
  left_join(planes_2, by = c("tailnum")) %>%
  mutate(across(type:engine, ~ case_when(
    is.na(.) ~ "Unknown",
    TRUE ~ .
  ))) %>% 
  mutate(manufacturer = case_when(
    manufacturer == "AIRBUS INDUSTRIE" ~ "AIRBUS",
    manufacturer == "EMBRAER S A" ~ "EMBRAER",
    manufacturer %in% c("AIRBUS", "BOEING", "BOMBARDIER INC", "EMBRAER", "Unknown") ~ manufacturer,
    TRUE ~ "Other"
  )) 


write_csv(flights_joined_alt, here::here("clean_data/flights_clean_joined.csv"))


flights_joined_alt_airports <- flights_joined_alt %>% 
  left_join(airports, by = c("origin" = "faa")) %>% 
  left_join(airports, by = c("dest" = "faa"))
  

write_csv(flights_joined_alt_airports, here::here("clean_data/flights_clean_joined_airports.csv"))
  