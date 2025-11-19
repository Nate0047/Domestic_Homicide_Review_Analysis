# animal extract

# script to identify animal words in text and extract all to seperate df

# import packages ------------------------------------------------------------------------
library(tidyverse)

# for animal names
library(wakefield)
data("animal_list")
# turn into single tokens
single_animal_list <- tolower(animal_list[!str_detect(animal_list, " ")])

# identify all animals
animal_detected <-
  dhr_fulltokens %>% 
  filter(word %in% single_animal_list)

animal_detected %>%
  filter(word != "human") %>% # excludes term human and only returns animal tokens
  write_excel_csv(., "specific animal name search.csv")