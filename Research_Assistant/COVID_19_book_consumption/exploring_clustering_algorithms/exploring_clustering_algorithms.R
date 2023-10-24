# load the required packages
library(tm)
library(text2vec)
library(stats)
library(cluster)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(httr)
library(jsonlite)
library(quanteda)
library(lsa)
library(data.table)

# disable scientific notation
options(scipen = 999)

# Load data
books <- read_csv("D:/Research assistant/Thesis/Data wrangling for Thesis/Preparation of datasets/all_data_jt1_date_read_and_isbn.csv")

# Convert ISBN column to character type
books$isbn13 <- as.character(books$isbn13)

# Grouping books by title and isbn13, how many isbn13 per title
book_name_author_isbn <- books %>%
  count(book_url, book_title,author_name, isbn13) %>%
  group_by(book_title) %>%
  top_n(1, wt = n)


# Exploring options, removing capitalization, special characters and space
books_try <- book_name_author_isbn %>%
  mutate(book_title = tolower(book_title)) %>%
  mutate(book_title = gsub("[^[:alnum:]]", "", book_title))

# create a new column with the most common isbn13 for each book_title and author_name combination
book_try_isbn <- books_try %>%
  group_by(book_title, author_name) %>%
  filter(!is.na(isbn13)) %>%
  summarise(most_common_isbn = isbn13[which.max(table(isbn13))])

# merge most_common_isbn with book_try
book_try_merged <- books_try %>%
  left_join(book_try_isbn, by = c("book_title", "author_name")) %>%
  mutate(isbn13 = if_else(is.na(isbn13), most_common_isbn, isbn13))



# Exploring the results to see the change
nas_dirty <- sum(is.na(book_name_author_isbn$isbn13))
nas_cleaned <- sum(is.na(book_try_merged$isbn13))

# Exploring unique number of ISBNs
dirty_unique_isbn <- book_name_author_isbn %>%
  distinct(isbn13) %>%
  nrow()

cleaned_unique_isbn <- book_try_merged %>%
  distinct(isbn13) %>%
  nrow()

#Number of titles
dirty_titles <- book_name_author_isbn %>%
  distinct(book_title) %>%
  nrow()

cleaned_titles <- book_try_merged %>%
  distinct(book_title) %>%
  nrow()
