# text cleaning

# script to pre-process and clean text data from the dhr in preparation for text analysis

# import packages ------------------------------------------------------------------------
library(tidyverse)
library(tidytext)
  data("stop_words")
library(SnowballC)
library(fs)

# ingest raw data ------------------------------------------------------------------------

# read in dhr_data from csv file
dhr_text <- read.csv("dhr_data_csv/dhr_dataframe.csv", encoding = "UTF-8")

# aggregate all text into report_id
dhr_grouped <- aggregate(text ~ report_id, dhr_text, function(x) paste(x, collapse = " "))

# cleaning body of text ------------------------------------------------------------------

# to help with memory, process each df separately and then combine again at end

# list report ids
report_ids <- unique(dhr_grouped$report_id)

# create empty list to store results
df_list <- list()

# loop cleaning process over each df
for(id in report_ids) {
  # subset the report from the df
  df_subset <- dhr_grouped[dhr_grouped$report_id == id, ]
  
  # clean the text and store the result in a list
  df_list[[as.character(id)]] <- df_subset %>%
  mutate(clean_text = gsub("\\d+", "", # Remove numbers
                      gsub("[[:punct:]]+", "", # Remove punctuation
                      gsub("\\s+", " ", # Replace multiple spaces with a single space
                      tolower(text) # Convert to lowercase
                      ))))
}

# combine the results back into a df
dhr_cleantext <- bind_rows(df_list)

# clear environment
rm(df_list, df_subset, dhr_grouped, id, report_ids)

# cleaning tokens ------------------------------------------------------------------------

# tokenise cleaned df
dhr_tokens <- dhr_cleantext %>%
  select(report_id, clean_text) %>%
  unnest_tokens(word, clean_text)

# processing steps on single tokens
dhr_tokens <- dhr_tokens %>%
  anti_join(stop_words) %>% # remove stop words
  mutate(stem_word = wordStem(word)) # generate word stems

# build cleaned tokens back into full reports
dhr_cleantext <- dhr_tokens %>% 
  group_by(report_id) %>%
  summarise(words = paste(word, collapse = " "), 
            stem_words = paste(stem_word, collapse = " ")) 

# write out cleaned text -----------------------------------------------------------------

# create subfolder to save json
fs::dir_create("dhr_data_json")

# write out to json
jsonlite::write_json(dhr_cleantext, "dhr_data_json/cleaned_and_stemmed_dhrs.json")

# clear environment ----------------------------------------------------------------------
rm(dhr_cleantext, dhr_text, dhr_tokens, stop_words)








