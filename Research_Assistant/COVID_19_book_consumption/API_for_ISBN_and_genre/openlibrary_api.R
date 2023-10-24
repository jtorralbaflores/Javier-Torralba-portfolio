library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(httr)
library(jsonlite)
library(purrr)

# disable scientific notation
options(scipen = 999)

# Loading the sample
isbn_sample_test <- read_csv("isbn_sample_test.csv")

# Convert ISBN column to character
isbn_sample_test$isbn_sample_test <- as.character(isbn_sample_test$isbn_sample_test)

# Running the API
get_json_and_save <- function(ISBN) {
  url1 <- paste0("https://openlibrary.org/ISBN/", ISBN, ".json")
  step1 <- GET(url1)
  step2 <- fromJSON(rawToChar(step1$content))
  url2 <- paste0("https://openlibrary.org", step2$works$key, ".json")
  step3 <- GET(url2)
  step4 <- fromJSON(rawToChar(step3$content))
  filename <- paste0("out/data/", ISBN, ".json")
  writeLines(toJSON(step4), filename)
  Sys.sleep(1)
}

# Trying out the loop
walk(isbn_sample_test$isbn_sample_test, safely(get_json_and_save))
