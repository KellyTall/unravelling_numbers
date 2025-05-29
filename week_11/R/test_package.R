
remotes::install_github("KellyTall/wesandersoncast")


library(wesandersoncast)
library(tidyverse)
library(wesanderson)

# ?wesanderson

View(wes_actors)

film_order <- wes_actors |>
  ungroup() |>
  distinct(film, release_date)  |>
  arrange(release_date) |>
  pull(film)


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

# View(arranged_data)
pal <- wes_palette("GrandBudapest2", 100, type="continuous")

wes_theme <- theme(
    legend.position = c(0, -0.15),  # bottom-left, just under the plot
    legend.justification = c(0, 1), # align legend's top-left corner
    legend.direction = "horizontal",
    legend.box.margin = margin(t = 10),
    plot.margin = margin(b = 40)   # extra space at bottom for legend
  )


ggplot(arranged_data, aes(film_wrapped, actor, fill=n))+
  geom_tile(alpha=.8)+
  theme_minimal()+
  scale_fill_gradientn(colours = pal)+
  wes_theme


