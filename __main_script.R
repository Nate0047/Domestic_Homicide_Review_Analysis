# main script for project

# renv library
source("R/packages.R")

# deploy webscraper to scrape dhrs from gov website
source("R/scraper.R") # uncomment this module if scraper is to run.

# convert downloaded pdfs into single .csv
source("R/pdf_to_csv.R")

# conduct pre-processing and cleaning on the dhr reports
source("R/text_cleaning.R")

# conduct general text analysis on the dhr reports
source("R/text_analysis.R")

# extracts all animal terms from DHR tokens
source("R/animal_extract.R")

# broke down the 654 DHRs as of 30 June 2026
# total words: 13.2 million words
# total unique tokens: 6 million unique tokens
# animal list checked against this, finding 1758 hits
# many of these will be false positives
