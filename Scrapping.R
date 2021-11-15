# Scraping

library(tidyverse)
library(rvest)
library(purrr)

html <- read_html("https://es.wikipedia.org/wiki/%C3%81reas_metropolitanas_de_M%C3%A9xico")

html %>% 
  html_element(".wikitable") %>% 
  html_table() -> zm

View(zm)

#colnames(zm) <- zm[1,]

glimpse(zm)

#zm <- zm[2:51,]

#zm <- zm[,3:8] 

#write_csv(x = zm, file = "Data Science/Datasets/zm_2020.csv")
