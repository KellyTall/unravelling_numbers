library(rvest)
library(tidyverse)
library(lubridate)



html_1 <-  read_html("https://www.artgallery.nsw.gov.au/collection/works/?artist_id=cossington-smith-grace")

artist_name <- html_1%>%
  html_elements("span.card-artwork-artist") %>%
  html_text2() %>%
  as_tibble() %>%
  rename(artist_name=value)

name_of_work <- html_1%>%
  html_elements("span.card-artwork-title") %>%
  html_text2() %>%
  as_tibble() %>%
  rename(work_name=value)


date_of_work <- html_1%>%
  html_elements("span.card-artwork-date") %>%
  html_text2() %>%
  as_tibble() %>%
  rename(work_date=value)

data <- cbind(artist_name, name_of_work, date_of_work) %>%
  as_tibble()

filenames <- list.files(pattern="*.csv")

data2 <- purrr::map_df(filenames,
                                     ~read.csv(.x, stringsAsFactors = FALSE) %>% mutate(filename = .x))
View(name_of_work2)

data3 <- data2 |>
  group_by(file_name) |>
  mutate(order=row_number())

name_of_work2 <- data |>
  mutate(filename = case_when(work_name=="The curve of the bridge" ~ "curve_of_bridge.csv",
                               work_name=="Arums growing" ~ "arum_growing.csv",
                               work_name=="Centre of a city" ~ "centre_of_city.csv",
                               work_name=="Bonfire in the bush" ~ "bonfire_in_bush.csv",
                               work_name=="Bush at evening" ~ "bush_at_evening.csv",
                               work_name=="Drapery and wattle" ~ "drapery_and_wattle.csv",
                               work_name=="Extravaganza" ~ "extravaganza.csv",
                               work_name=="Golden morning" ~ "golden_morning.csv",
                               work_name=="Gum blossoms" ~ "gum_blossoms.csv",
                               work_name=="Interior with wardrobe mirror" ~ "interior.csv",
                               work_name=="Landscape at Pentecost" ~ "landscape_at_pentacost.csv",
                               work_name=="Reinforcements: troops marching" ~ "troops.csv",
                               work_name=="Rushing" ~ "rushing.csv",
                               work_name=="Signing" ~ "signing.csv",
                               work_name=="The curve of the bridge" ~ "curve_of_bridge.csv",
                               work_name=="The Lacquer Room" ~ "lacquer_room.csv",
                               work_name=="The prince" ~ "the_prince.csv",
                               work_name=="The reader" ~ "the_reader.csv",
                               work_name=="The sock knitter" ~ "sock_knitter.csv",
                               work_name=="The window" ~ "the_window.csv",
                               work_name=="Things on an iron tray on the floor" ~ "iron_tray.csv",
                               work_name=="Wildflowers" ~ "wildflowers.csv",
                               work_name=="Wonga Wonga Street, Turramurra" ~ "wonga_wonga.csv")) |>
  filter(!is.na(filename))


View(data4)

data4 <- data3 |>
  right_join(name_of_work2) |>
  select(-col1, -col2, -col4, -col5) |>
  mutate(col3=str_remove(col3, "^.*(?=(#))")) |>
  mutate(col3 = str_split_i(col3, " ", 1)) |>
  mutate(work_date =str_remove(work_date , "circa ")) |>
  mutate(work_date = case_when(work_date == "1928-1929" ~ "1929",
                               TRUE ~ as.character(work_date))) |>
  mutate(work_date = ymd(work_date, truncated = 2L)) |>
  group_by (work_name)|>
  mutate(number = cur_group_id()) |>
  ungroup() |>
  mutate(work_date = case_when(filename == "signing.csv" ~ "1945-06-01",
                               TRUE ~ as.character(work_date)))  |>
  mutate(work_date = ymd(work_date)) |>
  group_by(work_date,work_name) |>
  mutate(row_num =row_number())




list1 <- data4 |>
  ungroup() |>
  filter(number ==1) |>
  select(col3) |>
  as.list()

arum_growing <- c(list1)


list2 <- data4 |>
  ungroup() |>
  filter(number ==2) |>
  select(col3) |>
  as.list()

bonfire_in_bush <- c(list2)


list3 <- data4 |>
  ungroup() |>
  filter(number ==3) |>
  select(col3) |>
  as.list()

bush_at_evening <- c(list3)


list4 <- data4 |>
  ungroup() |>
  filter(number ==4) |>
  select(col3) |>
  as.list()

centre_of_city <- c(list4)


list5 <- data4 |>
  ungroup() |>
  filter(number ==5) |>
  select(col3) |>
  as.list()

drapery_and_wattle <- c(list5)


list6 <- data4 |>
  ungroup() |>
  filter(number ==6) |>
  select(col3) |>
  as.list()

extravaganza <- c(list6)

View(data4)
list7 <- data4 |>
  ungroup() |>
  filter(number ==7) |>
  select(col3) |>
  as.list()

golden_morning <- c(list7)

list8 <- data4 |>
  ungroup() |>
  filter(number ==8) |>
  select(col3) |>
  as.list()

gum_blossoms <- c(list8)


View(time_line)

time_line <- data4 |>
  group_by(work_name, work_date) |>
  tally()

week_5_chart <-  ggplot (data4, aes(work_date)) +
  geom_dotplot(binwidth = 150)+
  coord_flip()


ggsave(file="week_5.svg", plot=week_5_chart, width=120, height=520, units = c("px"), dpi=300)

write_csv(data4, "week_5.csv")
