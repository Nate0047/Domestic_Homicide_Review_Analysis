# text analysis 

# this script reads in the cleaned text data and then runs basic text analysis

# import packages ------------------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(forcats)
library(igraph)
library(ggraph)
library(widyr)

# data ingestion and exploration ---------------------------------------------------------

# read in data from json file
dhr_cleantext <- jsonlite::fromJSON("dhr_data_json/cleaned_and_stemmed_dhrs.json")

# extract tokens -------------------------------------------------------------------------

# extract tokens from full words 
dhr_fulltokens <- dhr_cleantext %>%
  select(report_id, words) %>%
  unnest_tokens(word, words)

# extract tokens from stemmed words
dhr_stemtokens <- dhr_cleantext %>%
  select(report_id, stem_words) %>%
  unnest_tokens(word, stem_words)

dhr_stemtokens %>%
  filter(word == "animal") # could not find
# NOTE - animal stems to anim, better to work with the full tokens

# exploring report tokens ----------------------------------------------------------------

# calcualte total count of unique tokens across reports
dhr_fulltokens %>%
  summarise(unique_words = n_distinct(word))

# calculate counts of tokens across reports
dhr_fulltokens %>%
  count(report_id, word, sort = TRUE)

# calculate total number of tokens across reports
dhr_fulltokens %>%
  group_by(report_id) %>%
  summarise(n = n())

# calculate presence of animal across all reports (plot)
dhr_fulltokens %>% 
  filter(word %in% "animal") %>%
  group_by(report_id) %>%
  summarise(count = n()) %>%
ggplot(., aes(x = reorder(report_id, -count), y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# exported plot to plot folder

# bigrams --------------------------------------------------------------------------------

# extract bigrams across dhr reports
dhr_fullbigram <- dhr_cleantext %>%
  select(report_id, words) %>%
  unnest_tokens(bigram, words, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# examine bigram counts across sample
dhr_fullbigram %>%
  count(bigram, sort = TRUE)

# get counts of bigrams as seprate cols
dhr_fullbigram_count <- dhr_fullbigram %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)

# bigram network analysis on full reports ------------------------------------------------

# use igraph package to convert counts into graph input
dhr_bigram_igraph <- dhr_fullbigram_count %>%
  filter(word1 %in% "animal" | word2 %in% "animal") %>%
  igraph::graph_from_data_frame()

# use ggraph package to visualise
set.seed(47)

ggraph(dhr_bigram_igraph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(size = 0.5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# correlation of all words to animal words ---------------------------------------------

# using stemmed tokens, which words correlate with animal

# word correlations
word_cors <- dhr_fulltokens %>%
  group_by(word) %>%
  pairwise_cor(word, report_id, sort = TRUE)

word_cors %>%
  filter(str_detect(item1, "^animal"))

# calculate correlation of stemmed words to animal (take top 20)
word_cors %>%
  filter(item1 %in% c("animal")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 20) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# correlation network analysis
set.seed(47)

a <- grid::arrow(type = "closed", length = unit(.07, "inches"))

word_cors %>%
  filter(item1 %in% c("animal")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 40) %>%
  ungroup() %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE, arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(size = 0.5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# clear environment ----------------------------------------------------------------------
rm(a, dhr_bigram_igraph, dhr_cleantext, dhr_fullbigram, dhr_fullbigram_count, dhr_fulltokens,
   dhr_stemtokens, word_cors)

