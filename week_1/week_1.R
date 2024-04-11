library(tidyverse)
library(janitor)

data_import <- read_csv("20240105spcs.csv")|>
  clean_names()


data1 <- data_import |>
  select(scientific_name, common_name, kingdom, class, threatened_status, genus, species)

View(data1)

kingdom_summary <- data1 |>
  group_by(kingdom) |>
  tally()


kingdom_status <- data1 |>
  group_by(kingdom, threatened_status) |>
  tally()


kingdom_genus <- data1 |>
  group_by(kingdom, genus) |>
  tally() |>
  arrange(desc(n))


kingdom_genus_threat <- data1 |>
  group_by(kingdom, genus) |>
  add_tally(name = "total") |>
  group_by(kingdom, genus, threatened_status, total) |>
  tally(name="total_status") |>
  arrange(desc(total),genus, threatened_status)
396
View(kingdom_genus_threat)


kingdom_class_threat <- data1 |>
  group_by(kingdom, class) |>
  add_tally(name = "total") |>
  group_by(kingdom, class, threatened_status, total) |>
  tally(name="total_status") |>
  arrange(desc(total),class, threatened_status)

View(kingdom_class_threat)

write_csv(kingdom_class_threat,"kingdom_class_threat.csv")


kingdom_class <- data1 |>
  group_by(kingdom, class) |>
  tally(name = "total")

write_csv(kingdom_class,"kingdom_class.csv")

threat <- data1 |>
  group_by(threatened_status) |>
  tally(name = "total")


