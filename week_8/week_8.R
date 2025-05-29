library(rvest)
library(polite)
library(chromote)
library(tidyverse)
library(lubridate)
library(stringr)

# setwd("/Users/kellytall/Documents/github/unraveling_numbers/unravelling_numbers/week_8")



html_1 <-  read_html_live("https://www.sff.org.au/program/a-z/?type=Film&type=Event")

html_1$view()


html_1$scroll_to(top = 20000, left = 0)

title_film <- html_1 |>
  html_elements("h4.title") |>
  html_text() |>
  as_tibble() |>
  mutate(type = "all films") |>
  rename(title=value)

View(genre2)

genre <- html_1 |>
  html_elements(".-accordion-group, li , .-title") |>
  html_text() |>
  as_tibble()

genre2 <- genre |>
  mutate(value = str_trim(value, side=c("both"))) |>
  mutate(filter_var = case_when(value == "Genre" ~ "header",
                                value == "Country" ~ "header",
                                value == "Language" ~ "header",
                                value == "Classification" ~ "header",
                                value == "Program Strand" ~ "header",
                                TRUE ~ "filter" ))

View(genre3)

genre3 <- genre2 |>
  mutate(row_num = row_number()) |>
  filter(row_num >115) |>
  filter(row_num < 161) |>
  filter(value != "Genre") |>
  select(value) |>
  mutate(value = str_replace_all(value, " ", "%20"),
         value = str_replace_all(value, "&", "%26"),
         value = str_replace_all(value, "\\+", "%2B"))



## genre loop

##create url

genre_url_read <- genre3 |>
  mutate(url = paste("https://www.sff.org.au/program/a-z/?type=Film&type=Event&genre=", value, sep="")) |>
  select(-value)


genre_url_read_1 <- genre_url_read |>
  slice(1:10)

genre_url_read_2 <- genre_url_read |>
  slice(11:20)

genre_url_read_3 <- genre_url_read |>
  slice(21:30)

genre_url_read_4 <- genre_url_read |>
  slice(31:44)


View(genre_url_read)


sf_genre_query <- lapply(genre_url_read_1$url, function(i){

  genre_url <-  read_html_live(i)

  Sys.sleep(5)

  # genre_url$view()

  genre_url$scroll_to(top = 20000, left = 0)

  genre_film <- genre_url |>
    html_elements("h4.title") |>
    html_text() |>
    as_tibble() |>
    rename(title=value) |>
    mutate(genre = i)

  })

sf_genre_query_1 <- sf_genre_query

sf_genre_query_2 <- lapply(genre_url_read_2$url, function(i){

  genre_url <-  read_html_live(i)

  Sys.sleep(5)

  # genre_url$view()

  genre_url$scroll_to(top = 20000, left = 0)

  genre_film <- genre_url |>
    html_elements("h4.title") |>
    html_text() |>
    as_tibble() |>
    rename(title=value) |>
    mutate(genre = i)

})


sf_genre_query_3 <- lapply(genre_url_read_3$url, function(i){

  genre_url <-  read_html_live(i)

  Sys.sleep(5)

  # genre_url$view()

  genre_url$scroll_to(top = 20000, left = 0)

  genre_film <- genre_url |>
    html_elements("h4.title") |>
    html_text() |>
    as_tibble() |>
    rename(title=value) |>
    mutate(genre = i)

})

sf_genre_query_4 <- lapply(genre_url_read_4$url, function(i){

  genre_url <-  read_html_live(i)

  Sys.sleep(5)

  # genre_url$view()

  genre_url$scroll_to(top = 20000, left = 0)

  genre_film <- genre_url |>
    html_elements("h4.title") |>
    html_text() |>
    as_tibble() |>
    rename(title=value) |>
    mutate(genre = i)

})

View(combined_genre)

combined_genre <- bind_rows(sf_genre_query_1, sf_genre_query_2, sf_genre_query_3, sf_genre_query_4) |>
  mutate(genre_short = str_replace_all(genre, "%20", " ")) |>
  mutate(genre_short = str_replace_all(genre_short,  "%26", "\\&"),
         genre_short = str_replace_all(genre_short, "%2B", "\\+" )) |>
  mutate(genre_short = str_remove_all(genre_short, "https://www.sff.org.au")) |>
  mutate(genre_short = str_split_i(genre_short, "\\=", 4)) |>
  group_by(title) |>
  add_tally(name="number_for_film") |>
  group_by(genre_short) |>
  add_tally(name="number_for_genre") |>
  mutate(genre_range = row_number()) |>
  mutate(min = min(genre_range)) |>
  mutate(max = max(genre_range)) |>
  mutate(mid = max/2)

genre_focus <- combined_genre|>
  group_by(genre_short, number_for_genre,min, max, mid) |>
  summarise() |>
  mutate(genre_short = as_factor(genre_short)) |>
  mutate(genre_short = fct_reorder(genre_short, number_for_genre))



week_8 <- ggplot(genre_focus, aes(genre_short)) +
  geom_segment(aes(x=0, y=mid, xend=5, yend=max), colour="red")+
  geom_segment(aes(x=0, y=mid, xend=5, yend=min),colour="blue")+
  geom_text(aes(label=max, x=5, y=mid))+
  facet_wrap(~fct_reorder(genre_short, -number_for_genre))+
  theme_minimal()

ggsave("week_8.svg", week_8)


ggplot(combined_genre, aes(reorder(genre_short, -number_for_genre), reorder(title,number_for_film)))+
  geom_tile()+
  scale_x_discrete(position = "top")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, hjust=0))



