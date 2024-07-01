library(tidyverse)
library(lubridate)
library(janitor)

data_import <- read_csv("unravelling_numbers/week_9/IDCJAC0001_066006_Data1.csv") |>
  clean_names()

# setwd("/Users/kellytall/Documents/github/unraveling_numbers/unravelling_numbers/week_9")

View(data_import)

data1 <- data_import |>
  mutate(year = as.character(year)) |>
  unite(date,year, month, sep="-", remove=FALSE) |>
  mutate(date=ym(date)) |>
  rename(rain=monthly_precipitation_total_millimetres) |>
  mutate(year=as.numeric(year)) |>
  group_by(year) |>
  summarise(total_rain =sum(rain)) |>
  ungroup() |>
  mutate(average = median(total_rain)) |>
  mutate(year_result = case_when(average >=total_rain ~ "below",
                                 average < total_rain ~ "above"))
