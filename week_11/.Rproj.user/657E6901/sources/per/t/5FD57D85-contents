install.packages(c("WikidataQueryServiceR", "tidyverse", "jsonlite"))
library(WikidataQueryServiceR)
library(tidyverse)
library(janitor)




query <- '
SELECT ?filmLabel ?actorLabel ?releaseDate ?duration ?boxOffice WHERE {
  ?film wdt:P57 wd:Q223687.         # Directed by Wes Anderson
  ?film wdt:P161 ?actor.            # Actor in film

  OPTIONAL { ?film wdt:P577 ?releaseDate. }  # Release date
  OPTIONAL { ?film wdt:P2047 ?duration. }     # Duration (in seconds)

  OPTIONAL {
    ?film p:P2142 ?boxOfficeStatement.
    ?boxOfficeStatement ps:P2142 ?boxOffice;
                        pq:P3005 wd:Q13780930.  # Valid in place: worldwide
  }

  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}'

results <- query_wikidata(query) 

View(results)

clean_results <- results |> 
  clean_names() |> 
  filter(!film_label %in% c("Castello Cavalcanti", "Cousin Ben Troop Screening with Jason Schwartzman", "Hotel Chevalier")) |> ##removing shorts / promos
  filter(duration != "13") |> # filters out short film versions of bottle rocket
  mutate(release_date = ymd(release_date)) |> 
  mutate(actor_label = case_when(actor_label == "Q1333118" ~ "Richard Ayoade",
                                TRUE ~ actor_label)) |> 
  group_by(film_label, actor_label) |> 
  arrange(release_date) |> 
  slice_min(release_date) |> 
  mutate(duration_seconds = as.numeric(duration) * 60,
         duration_period = seconds_to_period(duration_seconds)
  ) |> 
  select(-duration, -duration_seconds)
  

View(clean_results)
