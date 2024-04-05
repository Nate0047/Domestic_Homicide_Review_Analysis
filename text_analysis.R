# Text Analysis

# this script conducts text analysis on dhr reports to gain an overview of the language used
# across the 111 reports. 

# establish library and packages ---------------------------------------------------------
source("packages.R")

library(tidyverse)
library(tidytext)
  data("stop_words")
library(forcats)

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

# to do
# bigram analysis - bigram network graph
# words that correlate with 'stalking' and 'harassment'
# build network graph of correlation to 'stalking' and 'harassment'?
# 



