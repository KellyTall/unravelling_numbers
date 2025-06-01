

library(tidyverse)
library(wesanderson)

wes_data_3plus <- read_rds("week_11/input/wes_data.rds")
wes_data_2plus <- read_rds("week_11/input/wes_data_2plus.rds")

#to view all HEX codes in palette
wes_palettes$AsteroidCity1

wes_reorder_pal <- c("#0A9F9D","#CEB175","#C18748","#E54E21")

# View(arranged_data)

wes_theme <- theme(
  text = element_text(family = "Futura", size = 8, colour = "black"),
  axis.text = element_text(colour = "black"),
  axis.title = element_blank(),
  legend.position = "bottom",
  legend.justification = "right",
  legend.direction = "horizontal",
  legend.box.margin = margin(t = 2),
  legend.spacing = unit(2, "pt"),
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
  legend.title = element_text(size = 7),
  legend.text = element_text(size = 5),
  plot.background = element_rect(fill = "#D9C5B4", color = NA),
  panel.background = element_rect(fill = "#D9C5B4", color = NA),
  panel.grid = element_line(linewidth = 0.2)
)



wes_tile_3plus <- ggplot(wes_data_3plus, aes(film_wrapped, actor, fill=n))+
  geom_tile(alpha=.95, width = 0.95, height = 0.95)+
  theme_minimal()+
  scale_fill_gradientn(colours = wes_reorder_pal)+
  wes_theme+
  labs(fill="Number of films")+
  guides(
    fill = guide_colorbar(
      barheight = 0.5,
      barwidth = 5,
      title.position = "top",
      title.hjust = 0.5
    ))



wes_tile_3plus


ggsave("week_11/output/wes_tile_3plus.png", wes_tile_3plus, width = 7, height = 8, dpi = 300)

# wes_tile_2plus <- ggplot(wes_data_2plus, aes(film_wrapped, actor, fill=n))+
#   geom_tile(alpha=.8, width = 0.95, height = 0.95)+
#   theme_minimal()+
#   scale_fill_gradientn(colours = wes_reorder_pal)+
#   wes_theme+
#   labs(fill="Number of films")
#
# wes_tile_2plus
