library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(httr)
library(jsonlite)
library(data.table)
# disable scientific notation
options(scipen = 999)

# Load data
all_data_jt1_top_10_countries_and_isbn <- read_csv("D:/Research assistant/Thesis/Data wrangling for Thesis/Preparation of datasets/all_data_jt1_top_10_countries_and_isbn.csv")

#Unique number of ISBN13
number_of_ISBN <- all_data_jt1_top_10_countries_and_isbn %>%
  distinct(isbn13) %>%
  nrow()

# Creating a dataframe only for ISBNs
unique_ISBN <- unique(all_data_jt1_top_10_countries_and_isbn$isbn13, incomparables = FALSE)
ISBN_dataset <- data.frame(isbn13 = unique_ISBN)
ISBN_dataset <- na.omit(ISBN_dataset)

# Creating all_isbn.csv
write.csv(ISBN_dataset, "all_isbn.csv", row.names = FALSE)


#The total number of unique ISBNs in the dataset I will use for my thesis is 601,683. 

isbn_sample_test <- ISBN_dataset[1:100,]
isbn_sample_test <- data.frame(isbn_sample_test)
write.csv(isbn_sample_test, "isbn_sample_test2.csv", row.names = FALSE)



