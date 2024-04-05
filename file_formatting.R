# data formatting

# takes pdf files and formats them into one csv where each row is a line of text from ther report
# and the report_id, and page are stored as seperate variables. 

# establish library and packages ---------------------------------------------------------
source("packages.R")

library(tidyverse)
library(pdftools)
library(tm)
library(stringr)
library(fs)

# format pdfs into single df -------------------------------------------------------------

# get list of pdf files from folder
pdf_files <- list.files(path = "dhr_data_pdf/", pattern = "pdf$", full.names = TRUE)

# establish empty df for for loop
pdf_data <- data.frame()

# for loop to put contents of pdf files into df
for (i in seq_along(pdf_files)) {
  
  # read in the pdf and clean rogue " and trailing spaces
  pdf_text <- pdftools::pdf_text(pdf_files[i]) %>%
  str_split("\n") %>%
    lapply(., function(x) str_replace_all(x, "\"", "")) %>%
    lapply(., function(x) str_trim(x)) %>%
    lapply(., function(x) str_replace_all(x, " {2,}", " "))
  
  # add pdf text to df - each row = line of text (preserve page numbers as seperate var)
  for(j in seq_along(pdf_text)) {
    pdf_data <- rbind(pdf_data, data.frame(filename = pdf_files[i], page = j, text = unlist(pdf_text[j]), stringsAsFactors = FALSE))
  }
}

# remove rows with empty character vector at line level
pdf_data <- pdf_data %>% filter(text != "")

# add document index to second row of df and then order by this
pdf_data <- pdf_data %>%
  mutate(report_id = as.numeric(str_extract(filename, "\\d+"))) %>%
  select(report_id, everything()) %>%
  arrange(report_id)

# store file in subfolder ----------------------------------------------------------------

# create subfolder to save csv
fs::dir_create("dhr_data_csv")
  
# write out df to csv
write.csv(pdf_data, "dhr_data_csv/dhr_dataframe.csv")

# clear environment ----------------------------------------------------------------------
rm(pdf_data, pdf_text, i, j, pdf_files)
