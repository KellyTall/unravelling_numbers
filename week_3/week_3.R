# devtools::install_github("fkeck/subtools")

library(subtools)
library(tidyverse)
library(tidytext)
library(janitor)
library(stringr)
library(svglite)

# setwd("/Users/kellytall/Documents/github/unraveling_numbers/unravelling_numbers/week_3")

succession_ep1_s4_meta <- data.frame(Name = "Succession", Season = 4, Episode = 1)

succession_ep1_s4_import <- read_subtitles("Succession.S04E01.WEBRip.x264-ION10.srt", metadata = succession_ep1_s4_meta)

succession_ep1_s4 <- clean_captions(succession_ep1_s4_import) |>
  clean_names()

View(succession_ep1_s4)

succession_ep1_s4_tokens <- unnest_tokens(succession_ep1_s4, word, text_content)


View(succession_ep1_s4_bigram)


succession_ep1_s4_bigram <- succession_ep1_s4 |>
  unnest_tokens(trigram, text_content, token = "ngrams", n=2) |>
  filter(!is.na(trigram))


succession_ep1_s4_tokens <- succession_ep1_s4 |>
  unnest_tokens(word, text_content) |>
  filter(!is.na(word))

View(succession_ep1_s4_tokens)


fuck_filter <- succession_ep1_s4_tokens |>
  filter(str_detect(word, "fuck")) |>
  mutate(word_pos = factor(word)) |>
  mutate(word_pos = as.numeric(word_pos))


jitter <- position_jitter(width = 0, height = 0.5)

theme_fuck <- theme(
  panel.background = element_rect(fill="black"),
  plot.background = element_rect(fill="black"),
  panel.grid = element_line(colour="grey", linewidth = .10),
  panel.grid.major.x =  element_line(colour="darkgrey", linewidth = .10),
  axis.title = element_blank(),
  axis.text.x = element_text(colour="white", family="Helvetica"),
  axis.text.y = element_blank()
  )




fuck_chart <- ggplot(fuck_filter, aes(timecode_in, word))+
  geom_blank()+
  geom_segment(aes(x=timecode_in, xend=timecode_in, y=word_pos-.25, yend=word_pos+.25), alpha=.5, colour="yellow", size=2)+
  theme_minimal()+
  theme_fuck

ggsave(file="fuck.svg", plot=fuck_chart, width=24.6, height=14.8, units = c("cm"), dpi=300)


View(fuck_filter)


fuck_totals <- fuck_filter |>
  group_by(word) |>
  tally() |>
  mutate(word=as_factor(word))

fuck_chart1 <- ggplot(fuck_totals, aes(word,n))+
  geom_col()+
  coord_flip()

