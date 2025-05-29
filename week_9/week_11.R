install.packages(c("WikidataQueryServiceR", "tidyverse", "jsonlite"))
library(WikidataQueryServiceR)
library(tidyverse)



query <- '
SELECT ?filmLabel ?actorLabel ?characterLabel WHERE {
  ?film wdt:P57 wd:Q223687.  # Wes Anderson as director
  ?film wdt:P161 ?actor.   # Actor in film
  OPTIONAL { ?film p:P161 ?castStatement.
             ?castStatement ps:P161 ?actor;
                             pq:P453 ?character. }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}'

results <- query_wikidata(query) |> 
  mutate(actorLabel = case_when(actorLabel == "Q1333118" ~ "Richard Ayoade",
                                ))


View(results)
