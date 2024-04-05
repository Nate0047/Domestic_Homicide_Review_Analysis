# Text Analysis

# this script conducts text analysis on dhr reports to gain an overview of the language used
# across the 111 reports. 

# establish library and packages ---------------------------------------------------------
source("packages.R")

library(tidyverse)

# Read

# read in data from csv file
dhr_text <- read.csv("dhr_data_csv/dhr_dataframe.csv", encoding = "UTF-8")
