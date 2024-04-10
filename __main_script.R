# main script for project

# establish library and packages
source("R/packages.R")

# deploy webscraper to scrape dhrs from gov website
#source("R/scraper.R")

# convert downloaded pdfs into single .csv
source("R/pdf_to_csv.R")

# conduct pre-processing and cleaning on the dhr reports
source("R/text_cleaning.R")

# in progress - re-write this module following new cleaning
# conduct general text analysis on the dhr reports
source("R/text_analysis.R")