# word embedding

# this script takes the json file and conducts word embedding on dhr tokens

# import packages ------------------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(h2o)

# ISSUE: keras package for R installed, but this relies on Python and keras to be running
# to fix get a anaconda env running with python and keras so that R package keras can use this.

# data ingestion and exploration ---------------------------------------------------------

# read in data from json file
dhr_cleantext <- jsonlite::fromJSON("dhr_data_json/cleaned_and_stemmed_dhrs.json")

# begin h2o ------------------------------------------------------------------------------

# connect to h2o cluster
h2o.init()

# analyse text data via h2o --------------------------------------------------------------

# turn df into h20 object
h2o_object = as.h2o(dhr_cleantext)

# tokenise words in h2o
words <- h2o.tokenize(h2o_object$words, "\\\\W+")

# apply word2vec model
word2vec_model <- h2o.word2vec(words, min_word_freq = 5, epochs = 10)

# find synonyms using embeddings
stalk_syns <- as.data.frame(h2o.findSynonyms(word2vec_model, "stalk"))
stalking_syns <- as.data.frame(h2o.findSynonyms(word2vec_model, "stalking"))
stalked_syns <- as.data.frame(h2o.findSynonyms(word2vec_model, "stalked"))
stalker_syns <- as.data.frame(h2o.findSynonyms(word2vec_model, "stalker"))

harass_syns <-  as.data.frame(h2o.findSynonyms(word2vec_model, "harass"))
harassing_syns <-  as.data.frame(h2o.findSynonyms(word2vec_model, "harassing"))
harassed_syns <-  as.data.frame(h2o.findSynonyms(word2vec_model, "harassed"))
harassment_syns <-  as.data.frame(h2o.findSynonyms(word2vec_model, "harassment"))

# close down h2o -------------------------------------------------------------------------

# formally shutdown h2o cluster
h2o.shutdown()

# Process synonyms into a df
stalk_syns <- stalk_syns %>%
  mutate(item1 = c("stalk")) %>%
  rename(item2 = synonym) %>%
  select(item1, item2, score)

stalking_syns <- stalking_syns %>%
  mutate(item1 = c("stalking")) %>%
  rename(item2 = synonym) %>%
  select(item1, item2, score)

stalked_syns <- stalked_syns %>%
  mutate(item1 = c("stalked")) %>%
  rename(item2 = synonym) %>%
  select(item1, item2, score)

stalker_syns <- stalker_syns %>%
  mutate(item1 = c("stalker")) %>%
  rename(item2 = synonym) %>%
  select(item1, item2, score)

harass_syns <- harass_syns %>%
  mutate(item1 = c("harass")) %>%
  rename(item2 = synonym) %>%
  select(item1, item2, score)

harassing_syns <- harassing_syns %>%
  mutate(item1 = c("harassing")) %>%
  rename(item2 = synonym) %>%
  select(item1, item2, score)

harassed_syns <- harassed_syns %>%
  mutate(item1 = c("harassed")) %>%
  rename(item2 = synonym) %>%
  select(item1, item2, score)

harassment_syns <- harassment_syns %>%
  mutate(item1 = c("harassment")) %>%
  rename(item2 = synonym) %>%
  select(item1, item2, score)

stalkharass_syns <- rbind(stalk_syns, stalking_syns, stalked_syns, stalker_syns, 
                          harass_syns, harassing_syns, harassed_syns, harassment_syns)

# process into network graph
set.seed(47)

a <- grid::arrow(type = "closed", length = unit(.07, "inches"))

stalkharass_syns %>%
graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = score), show.legend = FALSE, arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(size = 0.5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


