library(tidyverse)
library(patchwork)



arum <- read_csv("arum_growing.csv", col_names = FALSE)

arum1 <- arum |>
  select(X1,X3) |>
  rename(value = X1,
         colour=X3) |>
  mutate(name = "Arum") |>
  mutate(value = str_split_i(value, " ", 1)) |>
  mutate(colour = str_split_i(colour, " ", 2)) |>
  mutate(value = str_remove_all(value, ":")) |>
  mutate(value = as.numeric(value)) |>
  group_by(name) |>
  arrange(desc(value)) |>
  mutate(prop = value/sum(value))

arum_colour <- c(arum1$colour)

sock_knitter <- read_csv("sock_knitter.csv", col_names = FALSE)

sock_knitter1 <- sock_knitter |>
  select(X1,X3) |>
  rename(value = X1,
         colour=X3) |>
  mutate(name = "Sock Knitter") |>
  mutate(value = str_split_i(value, " ", 1)) |>
  mutate(colour = str_split_i(colour, " ", 2)) |>
  mutate(value = str_remove_all(value, ":")) |>
  mutate(value = as.numeric(value)) |>
  group_by(name) |>
  arrange(desc(value)) |>
  mutate(prop = value/sum(value))

sock_knitter_colour <- c(sock_knitter1$colour)


window <- read_csv("the_window.csv", col_names = FALSE)

window1 <- window |>
  select(X1,X3) |>
  rename(value = X1,
         colour=X3) |>
  mutate(name = "The Window") |>
  mutate(value = str_split_i(value, " ", 1)) |>
  mutate(colour = str_split_i(colour, " ", 2)) |>
  mutate(value = str_remove_all(value, ":")) |>
  mutate(value = as.numeric(value)) |>
  group_by(name) |>
  arrange(desc(value)) |>
  mutate(prop = value/sum(value))

window_colour <- c(window1$colour)


bush <- read_csv("bush_at_evening.csv", col_names = FALSE)

bush1 <- bush |>
  select(X1,X3) |>
  rename(value = X1,
         colour=X3) |>
  mutate(name = "Bush at evening") |>
  mutate(value = str_split_i(value, " ", 1)) |>
  mutate(colour = str_split_i(colour, " ", 2)) |>
  mutate(value = str_remove_all(value, ":")) |>
  mutate(value = as.numeric(value)) |>
  group_by(name) |>
  arrange(desc(value)) |>
  mutate(prop = value/sum(value))

bush_colour <- c(bush1$colour)





interior <- read_csv("interior.csv", col_names = FALSE)

interior1 <- interior |>
  select(X1,X3) |>
  rename(value = X1,
         colour=X3) |>
  mutate(name = "interior") |>
  mutate(value = str_split_i(value, " ", 1)) |>
  mutate(colour = str_split_i(colour, " ", 2)) |>
  mutate(value = str_remove_all(value, ":")) |>
  mutate(value = as.numeric(value)) |>
  group_by(name) |>
  arrange(desc(value)) |>
  mutate(prop = value/sum(value))

interior_colour <- c(interior1$colour)




lacquer_room <- read_csv("lacquer_room.csv", col_names = FALSE)

lacquer_room1 <- lacquer_room |>
  select(X1,X3) |>
  rename(value = X1,
         colour=X3) |>
  mutate(name = "Lacquer Room") |>
  mutate(value = str_split_i(value, " ", 1)) |>
  mutate(colour = str_split_i(colour, " ", 2)) |>
  mutate(value = str_remove_all(value, ":")) |>
  mutate(value = as.numeric(value)) |>
  group_by(name) |>
  arrange(desc(value)) |>
  mutate(prop = value/sum(value))

lacquer_room_colour <- c(lacquer_room1$colour)

theme_gcs <- theme(
  axis.title = element_blank(),
  legend.position = "none",
  axis.line = element_blank(),
  panel.grid = element_blank(),
  axis.text = element_blank()
  )

sock_knit_bar <- ggplot(sock_knitter1, aes(name, reorder(desc(value),prop), fill=colour))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values=sock_knitter_colour)+
  theme_minimal()+
  theme_gcs+
  ggtitle("The Sock Knitter (1917)")

sock_knit_bar



lacquer_bar <- ggplot(interior1, aes(name, reorder(desc(value),prop), fill=colour))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values=lacquer_room_colour)+
  theme_minimal()+
  theme_gcs+
  ggtitle("The Lacquer Room (1936)")

lacquer_bar

interior_bar <- ggplot(interior1, aes(name, reorder(desc(value),prop), fill=colour))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values=interior_colour)+
  theme_minimal()+
  theme_gcs+
  ggtitle("Interior with Wardrobe Mirror (1955)")

interior_bar

bush_bar <- ggplot(bush1,aes(name, reorder(desc(value),prop), fill=colour))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values=bush_colour)+
  theme_minimal()+
  theme_gcs+
  ggtitle("Bush at evening (1947)")

bush_bar

window_bar <- ggplot(window1,aes(name, reorder(desc(value),prop), fill=colour))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values=window_colour)+
  theme_minimal()+
  theme_gcs+
  ggtitle("The Window (1956)")

window_bar

arum_bar <- ggplot(arum1,aes(name, reorder(desc(value),prop), fill=colour))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values=arum_colour)+
  theme_minimal()+
  theme_gcs+
  ggtitle("Arums growing (1927)")

arum_bar



 test <- sock_knit_bar/arum_bar/lacquer_bar/bush_bar/interior_bar/window_bar

 interior_bar/sock_knit_bar/lacquer_bar/bush_bar/window_bar/arum_bar

 ggsave("week7.svg",test)
