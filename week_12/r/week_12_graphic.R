library(tidyverse)
library(ggpol)

member_data <- readRDS("week_12/input/members_data_with_gender.rds") |>
  mutate(gender_test = case_when(gender_test =="female" ~ "Women",
                                 gender_test =="male" ~ "Men"))


View(member_data)

theme_parlo <- theme(
  legend.title = element_blank(),
  legend.justification =  "left",
  legend.margin = margin(0,0,0,0)
)

party_main <- c( "mediumblue", "#2f6e4e",  "firebrick1", "springgreen2", "goldenrod","#0E788F","#e43942")

member_data_totals <- member_data |>
  group_by(party) |>
  tally(name="seats") |>
  mutate(party = as_factor(party),
         party = fct_rev(fct_relevel(party, c("Labor", "Independent", "Centre Alliance", "Greens",
                               "Katter's Australian", "National", "Liberal"))) )|>
  arrange(party,seats) |>
  mutate(party_labels = paste0(party, " (",seats,")"))




members_all <- ggplot(member_data_totals) +
  geom_parliament(aes(seats = seats, fill = party), colour="white") +
  scale_fill_manual(values=party_main, labels = member_data_totals$party_labels,guide = guide_legend(reverse = TRUE)) +
  coord_fixed() +
  theme_void()+
  theme_parlo

members_all

ggsave("week_12/output/members_all.png", members_all, width = 5, height = 3, dpi = 300)


gender_col <- c("slateblue", "seagreen")

member_data_gender_totals <- member_data |>
  group_by(gender_test) |>
  tally(name="seats") |>
  ungroup() |>
  mutate(gender_test = as_factor(gender_test)) |>
  mutate(party_labels = paste0(gender_test, " (",seats,")"))

gender_split_overall <- ggplot(member_data_gender_totals) +
  geom_parliament(aes(seats = seats, fill = gender_test), color = "white") +
  scale_fill_manual(values=gender_col, labels = member_data_gender_totals$party_labels) +
  coord_fixed() +
  theme_void()+
  theme_parlo

gender_split_overall

ggsave("week_12/output/gender_split_overall.png", gender_split_overall, width = 5, height = 3, dpi = 300)


gender_by_party <- member_data |>
  mutate(party = case_when(party == "Liberal" | party == "National" ~ "Coaltion",
                           party %in% c("Greens", "Centre Alliance", "Independent",
                                        "Katter's Australian") ~ "Independent",
                           TRUE ~ as.character(party))) |>
  group_by(party, gender_test) |>
  tally(name="seats") |>
  mutate(party = as_factor(party),
         party = fct_relevel(party, c("Coaltion", "Independent", "Labor"))) |>
  mutate(gender_party_label = paste(party, gender_test)) |>
  arrange(party) |>
  mutate(gender_party_label = as_factor(gender_party_label)) |>
  mutate(party_labels = paste0(gender_party_label, " (",seats,")"))



gender_party_col <- c("steelblue4", "steelblue1" , "aquamarine1","aquamarine4",  "red4","red1")

party_gender_split <- ggplot(gender_by_party) +
  geom_parliament(aes(seats = seats, fill = gender_party_label), color = "white") +
  scale_fill_manual(values=gender_party_col, labels = gender_by_party$party_labels,  guide = guide_legend(reverse = TRUE)) +
  coord_fixed() +
  theme_void()+
  theme_parlo

party_gender_split

ggsave("week_12/output/party_gender_split.png", party_gender_split, width = 5, height = 3, dpi = 300)
