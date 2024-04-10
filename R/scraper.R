# web scraper for dhr project

# this script downloads the .pdfs from gov website and stores them within project file

# establish library and packages ---------------------------------------------------------
source("packages.R")

# scraper --------------------------------------------------------------------------------

library(rvest)
library(fs)

# set URL for DHR gov website - filtered to stalking - set pagination to 1000 to have all DHR on one webpage
url <- "https://homicide-review.homeoffice.gov.uk/?Keywords%5B%5D=&Victim-Specific+Information.Victims.Nationality%5B%5D=&Perpetrator%2FSuspect+Information.Perpetrators.Nationality%5B%5D=&Aggravating+Factors%5B%5D=Stalking+factors.Stalking&Basic+Case+Details.Name+of+Community+Safety+Partnership%5B%5D=&pagination%5BsortField%5D=uploadDate&pagination%5BsortDirection%5D=desc&pagination%5BpageSize%5D=1000"

# read html from site
webpage <- rvest::read_html(url)

# identify all of the links in webpage
pdf_links <- webpage %>%
  html_nodes("a") %>%
  html_attr("href")

# filter list of links relating to /download/ 
pdf_links <- pdf_links[str_detect(pdf_links, "^/download/")]

# de-duplicate (replication caused by view & download function on webpage)
pdf_links <- unique(pdf_links)

# for loop over urls to d/l each pdf
for(i in seq_along(pdf_links)) {
  
  # create filename for each pdf
  filename <- paste0("pdf", i, ".pdf")
  
  # download the pdf
  download.file(paste0("https://homicide-review.homeoffice.gov.uk", pdf_links[i]), filename, mode = "wb")
}

# move files to a subfolder --------------------------------------------------------------

# get list of pdf files
pdf_files <- list.files(path = ".", pattern = "pdf$", full.names = TRUE)

# create subfolder for storing the pdfs
fs::dir_create("dhr_data_pdf")

# move pdfs to this subfolder
fs::file_move(pdf_files, file.path("dhr_data_pdf", basename(pdf_files)))

# clear environment ----------------------------------------------------------------------
rm(webpage, filename, i, pdf_links, url, pdf_files)




