
##imports data from wesadersoncaston github (not on cran at this stage)
remotes::install_github("KellyTall/wesandersoncast")


library(wesandersoncast)
library(tidyverse)


View(wes_actors)

# ?wes_actors

#creates sorting variable for factor
film_order <- wes_actors |>
  ungroup() |>
  distinct(film, release_date)  |>
  arrange(release_date) |>
  pull(film)

#wraps text
film_order_wrapped <- str_wrap(film_order, width = 10)

arranged_data <- wes_actors |>
  mutate(
    film_wrapped = factor(str_wrap(film, width = 10), levels = film_order_wrapped)
  ) |>
  group_by(actor) |>
  add_count() |>
  filter(n > 1) |>
  ungroup() |>
  mutate(actor = fct_reorder(actor, release_date))

saveRDS(arranged_data, file="week_11/input/wes_data_2plus.rds")

