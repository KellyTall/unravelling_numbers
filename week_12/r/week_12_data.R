# Load necessary libraries
library(rvest)
library(dplyr)
library(readr)
library(janitor)
library(stringr)
library(tidyr)
library(gender)

# install.packages("gender")
# install.packages("remotes")
# remotes::install_github("lmullen/genderdata")


# Define the URL
url <- "https://en.wikipedia.org/wiki/Members_of_the_Australian_House_of_Representatives,_2025%E2%80%932028"

# Read the webpage
page <- read_html(url)

# Extract all tables
tables <- page %>% html_elements("table")



# Get the "Members" table (usually first or second depending on page layout)
members_table <- tables[[6]] %>% html_table(fill = TRUE) |>
  clean_names()


# View(members_data)

members_data <- members_table |>
  select(-party, -party_3, -member, -ref) |>
  rename(member = member_2,
         party=party_2) |>
  rename(name_raw = member) %>%
  mutate(
    year_of_birth = str_extract(name_raw, "\\d{4}|\\?"),
    full_name = str_remove(name_raw, "\\s*\\(born\\s*(\\d{4}|\\?)\\)")
  ) |>
  mutate(year_of_birth = case_when(year_of_birth == "?" ~ NA,
                                   TRUE ~ as.numeric(year_of_birth))) |>
  separate(full_name, into = c("first_last_name"), sep = "\n", extra = "merge") %>%
  separate(first_last_name, into = c("first_name", "last_name"), sep = " ", extra = "merge") %>%
  relocate(first_name, last_name, year_of_birth, .before = name_raw) |>
  mutate(approx_age = 2025-year_of_birth) |>
  mutate(first_name = str_trim(first_name)) |>
  mutate(party = str_remove(party, "\\[.*?\\]"))

members_gender <- gender(members_data$first_name, method = "ssa") |>
  rename(gender_test=gender) |>
  group_by(name, gender_test) |>
  summarise() |>
  rename(first_name = name)

members_data <- members_data %>%
  mutate(name_raw = str_replace_all(name_raw, "\n", " "),
         name_raw = str_squish(name_raw))

members_data_with_gender <- members_data |>
  left_join(members_gender) |>
  mutate(gender_test = case_when(name_raw == "Ali France (born 1973)" ~ "female",
                                name_raw == "Julie-Ann Campbell (born ?)" ~ "female",
                                name_raw == "Ged Kearney (born 1963)" ~ "female",
                                name_raw == "Llew O'Brien (born 1972)" ~ "male",
                            name_raw == "Pat Conroy (born 1979)" ~ "male",
                            TRUE ~ gender_test))


View(members_data_with_gender)



saveRDS(members_data_with_gender, file="week_12/input/members_data_with_gender.rds")
