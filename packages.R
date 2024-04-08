# script containing packages used in the project

.libPaths("C:/R library")

if(!require("tidyverse")) {install.packages("tidyverse")}
library(tidyverse)

if(!require("pdftools")) {install.packages("pdftools")}
library(pdftools)

if(!require("tm")) {install.packages("tm")}
library(tm)

if(!require("tidytext")) {install.packages("tidytext")}
library(tidytext)

if(!require("igraph")) {install.packages("igraph")}
library(igraph)

if(!require("ggraph")) {install.packages("ggraph")}
library(ggraph)

if(!require("widyr")) {install.packages("widyr")}
library(widyr)
