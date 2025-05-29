library(tidyverse)
library(clock)
library(janitor)
library(svglite)

# setwd("/Users/kellytall/Documents/github/unraveling_numbers/unravelling_numbers/week_4")

data_import <- read_csv("week_4.csv") |>
  clean_names()


View(data_format)


data_format <- data_import |>
  mutate(details = dmy(details)) |>
  select(1:7)

data_num <- data_format |>
  group_by(type) |>
  tally()

data_studio <- data_format |>
  filter(type=="Studio Albums")

data_single <- data_format |>
  filter(type=="Single")

data_comp <- data_format |>
  filter(type=="Compilation albums")


data_live <- data_format |>
  filter(type=="Live Albums")


data_remix <- data_format |>
  filter(type=="Remix albums")


data_soundtrack <- data_format |>
  filter(type=="Soundtrack albums")


data_ep <- data_format |>
  filter(type=="EP")




area_theme_polar <- theme(panel.background= element_rect(fill="black"),
                          axis.line = element_blank(),
                          panel.grid = element_line(colour="#EFEDE4", linewidth = .1),
                          axis.text.x = element_text(vjust = -2, hjust=1.25,size = 8, colour= "#C4C4C4"),
                          plot.background = element_rect(fill="black"),
                          panel.grid.minor = element_blank(),
                          axis.text.y = element_blank()
)

psb <- ggplot(data_format, aes(details))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(ymd("1983-01-01"),ymd("2024-12-28")))+
  scale_y_time()+

  geom_point(data=data_single, aes(x=details, y=length, alpha = .5), colour="orange")+
  geom_segment(data=data_single, aes(x=details, xend=details, y=0, yend=length, alpha = .5),  linewidth=.25, colour="orange")+

  geom_point(data=data_comp, aes(x=details, y=length, alpha = .5),colour="red")+
  geom_segment(data=data_comp, aes(x=details, xend=details, y=0, yend=length, alpha = .5), linewidth=.5, colour="red")+


  geom_point(data=data_live, aes(x=details, y=length, alpha = .5),colour="blue")+
  geom_segment(data=data_live, aes(x=details, xend=details, y=0, yend=length, alpha = .5),  linewidth=.25, colour="blue")+

  geom_point(data=data_remix, aes(x=details, y=length, alpha = .5),colour="purple")+
  geom_segment(data=data_remix, aes(x=details, xend=details, y=0, yend=length, alpha = .5),linewidth=.25, colour="purple")+

  geom_point(data=data_soundtrack, aes(x=details, y=length, alpha = .5),colour="darkgreen")+
  geom_segment(data=data_soundtrack, aes(x=details, xend=details, y=0, yend=length, alpha = .5),  linewidth=.25, colour="darkgreen")+

  geom_point(data=data_ep, aes(x=details, y=length, alpha = .5),colour="yellow")+
  geom_segment(data=data_ep, aes(x=details, xend=details, y=0, yend=length, alpha = .5),  linewidth=.25, colour="yellow")+

  geom_segment(data=data_studio, aes( x=details, xend=details, y=0, yend=length, linewidth=tracks), colour="pink", lineend = "round", alpha=.8)+

    coord_polar()+
  theme_minimal()+
  area_theme_polar


ggsave(file="psb.svg", plot=psb, width=24.6, height=14.8, units = c("cm"), dpi=300)

