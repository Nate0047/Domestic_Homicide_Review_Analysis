# Text Analysis

# this script conducts text analysis on dhr reports to gain an overview of the language used
# across the 111 reports. 

# establish library and packages ---------------------------------------------------------
source("packages.R")

library(tidyverse)
library(tidytext)
  data("stop_words")
library(forcats)
library(igraph)
library(ggraph)
library(widyr)

# data ingestion and exploration ---------------------------------------------------------

# read in data from csv file
dhr_text <- read.csv("dhr_data_csv/dhr_dataframe.csv", encoding = "UTF-8")

# convert to tidytext
dhr_tidytext <- dhr_text %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) # remove stop words

# exploring report tokens ----------------------------------------------------------------

# calculate counts of tokens across reports
report_words <- dhr_tidytext %>%
  count(report_id, word, sort = TRUE)

# calculate total of words 
total_words <- report_words %>%
  group_by(report_id) %>%
  summarise(total = sum(n))

# join the total to the report counts
report_words <- left_join(report_words, total_words)

# tf-idf ---------------------------------------------------------------------------------

# attach tf-idf
report_tf_idf <- report_words %>%
  bind_tf_idf(word, report_id, n)

# visualise top tf-idf words per report
report_tf_idf %>%
  group_by(report_id) %>%
  slice_max(tf-idf, n = 20) %>%
  ungroup %>%
ggplot(aes(tf-idf, fct_reorder(word, tf_idf), fill = report_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~report_id, ncol = 11, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# note: save plot manually for review. If looking at token frequency, personal pronouns appear
# often. When using tf-idf indicates a fairly standard use of language pertainin to dhrs. 

# clear environment
rm(report_tf_idf, report_words, total_words)

# bigrams --------------------------------------------------------------------------------

# extract bigrams across dhr reports
dhr_bigrams <- dhr_text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# examine bigrams - mostly stop words paired
dhr_bigrams %>%
  count(bigram, sort = TRUE)

# remove numbers, punctuation and stop words from each side of bigram (form filtered bigrams)
dhr_bigrams_filtered <- dhr_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  mutate(word1 = gsub("\\d+", "", word1),
         word2 = gsub("\\d+", "", word2)) %>%
  mutate(word1 = gsub("[[:punct:]]", "", word1),
         word2 = gsub("[[:punct:]]", "", word2)) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# get counts of seperated bigrams
dhr_bigram_counts <- dhr_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# unite filtered bigrams
dhr_bigrams_reunited <- dhr_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# bigram network analysis on full reports ------------------------------------------------

# use igraph package to convert counts into graph input
dhr_bigram_igraph <- dhr_bigram_counts %>%
  filter(n > 150) %>%
  igraph::graph_from_data_frame()

# use ggraph package to visualise
set.seed(47)

ggraph(dhr_bigram_igraph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(size = 0.5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# bigram network analysis on stalking words ----------------------------------------------

# establish counts of stalking / harassment bigrams
dhr_stalking1 <- dhr_bigrams_filtered %>%
  filter(word1 %in% c("stalking", "harassment")) %>% # stalking as first word
  count(word1, word2, sort = TRUE)

dhr_stalking2 <- dhr_bigrams_filtered %>%
  filter(word2 %in% c("stalking", "harassment")) %>% # stalking as second word
  count(word2, word1, sort = TRUE)

# combine the dfs
dhr_stalking_combined <- rbind(dhr_stalking1, dhr_stalking2)

# use igraph package to convert counts into graph input
dhr_bigram_igraph <- dhr_stalking_combined %>%
  filter(n > 2) %>%
  igraph::graph_from_data_frame()

# use ggraph package to visualise stalking / harassment specific network
set.seed(47)

a <- grid::arrow(type = "closed", length = unit(.07, "inches"))

ggraph(dhr_bigram_igraph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(size = 0.5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# note: visual shows couple of things in overview - 
# 'cyber / digital' stalking; 
# 'stalking behaviour'; 
# 'continual' harassment;
# harassment 'notice / warnings'; 
# sexual / racial / discrimination' harassment.

# clear environment
rm(dhr_bigram_counts, dhr_bigram_igraph, dhr_bigrams, dhr_bigrams_filtered, dhr_bigrams_reunited,
   dhr_stalking_combined, dhr_stalking1, dhr_stalking2)

# correlation of words to stalking words -------------------------------------------------

# build df of only report id and words
report_words <- dhr_tidytext %>%
  select(report_id, word)

# calculate correlation of words to stalking (take top 25)
word_cor %>%
  filter(item1 %in% c("stalking", "harassment")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 25) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# correlation network analysis
set.seed(47)

word_cor %>%
  filter(item1 %in% c("stalking", "harassment")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 25) %>%
  ungroup() %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(size = 0.5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# to do ----------------------------------------------------------------------------------

# try now converting to a DTM and running LDA on only pages of the reports that contain the
# terms stalking &| harassment. 

