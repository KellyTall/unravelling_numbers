library(tidyverse)
library(janitor)



data_import <- read_csv("20240105spcs.csv")|>
  clean_names()


View(plants)

plants <- data_import |>
  filter(kingdom=="Plantae") |>
  filter(class=="Magnoliopsida") |>
  select(threatened_status, class, family) |>
  group_by(family) |>
  add_tally(name="fam_total") |>
  group_by(family, threatened_status) |>
  add_tally(name="threat_fam_total") |>
  distinct() |>
  mutate(comb = paste(family, threatened_status, sep=", " )) |>
  mutate(threatened_status = as_factor(threatened_status))


sum <- plants |>
  group_by(threatened_status) |>
  tally()

plant_fam <- plants |>
  ungroup() |>
  select(family,fam_total) |>
  distinct()



flowers <- ggplot(plants, aes(comb, fam_total))+
  geom_segment(aes(x=comb, y=0, xend=comb,yend=fam_total))+
  geom_point(aes(size=threat_fam_total, colour=threatened_status), alpha=.5)+
  scale_size_continuous(range=c(5,15))+
  facet_wrap(~threatened_status, scales = "free" )+
  geom_text(aes(label=family), angle=90,hjust=-0.05, size=2)+
  theme_minimal()+
  theme(
    axis.line = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_blank()
  )


ggsave("flowers.svg", plot=flowers)

write.csv(plants, "plants.csv")
