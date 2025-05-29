library(tidyverse)
library(lubridate)
library(janitor)

data_import <- read_csv("unravelling_numbers/week_9/IDCJAC0001_066006_Data1.csv") |>
  clean_names()

setwd("/Users/kellytall/Documents/github/unraveling_numbers/unravelling_numbers/week_9")

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


data2 <- data_import |>
  mutate(year = as.character(year)) |>
  unite(date,year, month, sep="-", remove=FALSE) |>
  mutate(date=ym(date)) |>
  rename(rain=monthly_precipitation_total_millimetres) |>
  mutate(year=as.numeric(year)) |>
  # filter(year >=2000) |>
  group_by(year) |>
  add_tally(name = "total_rain", sum(rain)) |>
  mutate(average = median(rain))


data3 <- data_import |>
  mutate(year = as.character(year)) |>
  unite(date,year, month, sep="-", remove=FALSE) |>
  mutate(date=ym(date)) |>
  rename(rain=monthly_precipitation_total_millimetres) |>
  mutate(year=as.numeric(year)) |>
  # mutate(year=as.numeric(month)) |>
  # filter(year >=1970) |>
  group_by(month) |>
  add_tally(name = "total_rain", sum(rain)) |>
  mutate(average = median(rain)) |>
  mutate(month_result = case_when(average >=rain ~ "below",
                                 average < rain ~ "above")) |>
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




View(data3)

ggplot(data1, aes(year, total_rain)) +
  geom_col(aes(fill=year_result))+
  geom_smooth()+
  scale_x_continuous()



ggplot(data3, aes(year, rain))+
  geom_col(aes(fill=month_result))+
  geom_hline(aes(yintercept=average))+
  facet_wrap(~month)

ggplot(data3, aes(rain))+
  geom_density()+
  facet_wrap(~month)

rain <- theme(
  plot.background = element_rect(fill="#012030"),
  panel.background = element_rect(fill="#012030"),
  legend.background = element_rect(fill="#012030"),
  panel.grid = element_line(colour="#13678A", linewidth = .25),
  text = element_text(colour="#DAFDBA"),
  axis.text = element_text(colour="#DAFDBA")

)

week_9 <- ggplot(data3, aes(month_label, rain))+
  geom_violin(fill="#45C4B0", aes(alpha=average), colour="#45C4B0", linewidth=.25)+
  geom_jitter(height = 0, width = 0.1, alpha=.2, colour="#DAFDBA")+
  rain


ggsave("week_9.svg", week_9, width=39.5, height = 19.5, units=c('cm'))



max <- data3 |>
  group_by(month) |>
  slice_max(rain)

min <- data3 |>
  group_by(month) |>
  slice_min(rain)

average <- data3 |>
  group_by(month_label, average) |>
  tally()

year <- data3 |>
  group_by(year) |>
  summarise(total = sum(rain)) |>
  arrange(desc(total)) |>
  mutate()
View(year)
