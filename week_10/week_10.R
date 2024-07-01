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

expand_variable <- data_import |>
  expand(year, month)

data1 <- data_import |>
  mutate(year = as.character(year)) |>
  unite(date,year, month, sep="-", remove=FALSE) |>
  mutate(date=ym(date)) |>
  rename(rain=monthly_precipitation_total_millimetres) |>
  mutate(year=as.numeric(year)) |>
  group_by(month) |>
  mutate(av_rain_month =mean(rain)) |>
  ungroup() |>
  mutate(av_rain_diff = (((rain - av_rain_month)/av_rain_month))*100) |>
  mutate(rain_diff = rain - av_rain_month) |>
  # mutate(av_rain_diff = rain - av_rain_month) |>
  right_join(expand_variable) |>
  mutate(month_label = case_when(month == "01" ~ "JAN",
                                 month == "02" ~ "FEB",
                                 month == "03" ~ "MAR",
                                 month == "04" ~ "APR",
                                 month == "05" ~ "MAY",
                                 month == "06" ~ "JUN",
                                 month == "07" ~ "JUL",
                                 month == "08" ~ "AUG",
                                 month == "09" ~ "SEP",
                                 month == "10" ~ "OCT",
                                 month == "11" ~ "NOV",
                                 month == "12" ~ "DEC")) |>
  mutate(month_label = as_factor(month_label)) |>
  mutate(month_label = fct_relevel(month_label, c("JAN", "FEB", "MAR","APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")))

rain2 <- theme(
  plot.background = element_rect(fill="#012030"),
  panel.background = element_rect(fill="#012030"),
  legend.background = element_rect(fill="#012030"),
  panel.grid = element_line(colour="#13678A", linewidth = .25),
  text = element_text(colour="#DAFDBA"),
  axis.text = element_text(colour="#DAFDBA"),
  axis.title = element_blank(),
  legend.position = "bottom"

)


week_10 <- ggplot(data1, aes(month_label,year, fill=rain_diff))+
  geom_raster()+
  scale_fill_gradient2(low="#BF491F", mid="#012030",high="#45C4B0", na.value = "slategrey")+
  theme_minimal()+
  rain2

week_10

ggsave("week_10.svg", week_10, width=39.5, height = 19.5, units=c('cm'))
