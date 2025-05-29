library(tidyverse)
library(janitor)
library(caret)

data1 <- readxl::read_xlsx("sofia_coppola.xlsx") |>
  clean_names() |>
  filter(film != "On the Rocks") |>


View(data1)


run_time_norm_scale <- data1$running_time

process <- preProcess(as.data.frame(run_time_norm_scale), method=c("range"))

norm_scale_run_time <- predict(process, as.data.frame(run_time_norm_scale))


budget_norm_scale <- data1$budget

process <- preProcess(as.data.frame(budget_norm_scale), method=c("range"))

norm_scale_budget <- predict(process, as.data.frame(budget_norm_scale))


box_office_norm_scale <- data1$box_office

process <- preProcess(as.data.frame(box_office_norm_scale), method=c("range"))

norm_scale_box_office <- predict(process, as.data.frame(box_office_norm_scale))


rotten_toms_score_norm_scale <- data1$rotten_toms_score

process <- preProcess(as.data.frame(rotten_toms_score_norm_scale), method=c("range"))

norm_scale_rotten_toms_score <- predict(process, as.data.frame(rotten_toms_score_norm_scale))


profit_norm_scale <- data1$profit

process <- preProcess(as.data.frame(profit_norm_scale), method=c("range"))

norm_scale_profit <- predict(process, as.data.frame(profit_norm_scale))

data2 <- data1 |>
  cbind(norm_scale_run_time, norm_scale_budget, norm_scale_box_office, norm_scale_rotten_toms_score, norm_scale_profit) |>
  as_tibble()

data3 <- data2 |>
  select(-comments) |>
  select(1:2, 9:13) |>
  pivot_longer(cols=3:7, names_to = "names", values_to = "values")

View(data4)

data4 <- data3 |>
  mutate(values = values +1) |>
  group_by(names) |>
  mutate(av_score=mean(values)) |>
    ungroup() |>
    pivot_wider(names_from = names, values_from = c(values, av_score))


write_csv(data4, "all_data_sofia_copolla.csv")

data_average_set <- data4 |>
  select(names, av_score) |>
  unique()


write_csv(data_average_set, "data_average_set.csv")



