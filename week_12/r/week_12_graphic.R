library(tidyverse)
library(ggpol)

member_data <- readRDS("week_12/input/members_data_with_gender.rds")


View(member_data)

party_main <- c( "mediumblue", "green4",  "yellow", "green", "goldenrod","cyan4","red")

member_data_totals <- member_data |>
  group_by(party) |>
  tally(name="seats") |>
  mutate(party = as_factor(party),
         party = fct_rev(fct_relevel(party, c("Labor", "Independent", "Centre Alliance", "Greens",
                               "Katter's Australian", "National", "Liberal"))) )|>
  arrange(party,seats)


ggplot(member_data_totals) +
  geom_parliament(aes(seats = seats, fill = party), color = "black") +
  scale_fill_manual(values=party_main, labels = member_data_totals$party) +
  coord_fixed() +
  theme_void()

gender_col <- c("slateblue", "seagreen")

member_data_gender_totals <- member_data |>
  group_by(gender_test) |>
  tally(name="seats")

ggplot(member_data_gender_totals) +
  geom_parliament(aes(seats = seats, fill = gender_test), color = "black") +
  scale_fill_manual(values=gender_col, labels = member_data_totals$gender_test) +
  coord_fixed() +
  theme_void()

gender_by_party <- member_data |>
  mutate(party = case_when(party == "Liberal" | party == "National" ~ "Coaltion",
                           party %in% c("Greens", "Centre Alliance", "Independent",
                                        "Katter's Australian") ~ "Other",
                           TRUE ~ as.character(party))) |>
  group_by(party, gender_test) |>
  tally(name="seats") |>
  mutate(party = as_factor(party),
         party = fct_relevel(party, c("Coaltion", "Other", "Labor"))) |>
  mutate(gender_party_label = paste(party, gender_test)) |>
  arrange(party)

gender_party_col <- c("blue4", "blue1" , "aquamarine4", "aquamarine1", "red4","red1")

ggplot(gender_by_party) +
  geom_parliament(aes(seats = seats, fill = gender_party_label), color = "black") +
  scale_fill_manual(values=gender_party_col, labels = member_data_totals$gender_party_label) +
  coord_fixed() +
  theme_void()

