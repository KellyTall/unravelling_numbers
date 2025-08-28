# Load required libraries

library(tidyverse)
library(lubridate)
library(stringr)

library(chromote)
library(rvest)



# Start Chromote session
b <- ChromoteSession$new()

# Navigate to the JS-heavy page
b$Page$navigate("https://www.sff.org.au/program/a-z/")

# Wait a few seconds to allow JS content to load
Sys.sleep(8)

# Evaluate JS to return full HTML after rendering
html_content <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value

# Parse HTML
page <- read_html(html_content)

# Extract film titles
film_titles <- page %>%
  html_elements("div.type-label-m") %>%
  html_text2()



films_df <- tibble(title = film_titles)

# Preview results
film_titles <- write_csv(films_df, "week_13/output/films_df.csv")
