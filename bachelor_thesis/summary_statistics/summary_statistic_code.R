library(kableExtra)
library(stargazer)
library(gt)
library(readr)
library(dplyr)
library(psych)

# Load data
all_data_jt1_date_read_and_isbn <- read_csv("D:/Research assistant/Thesis/Data wrangling for Thesis/Preparation of datasets/all_data_jt1_date_read_and_isbn.csv")

# Fixing age of book
all_data_jt1_date_read_and_isbn$year_read <- format(as.Date(all_data_jt1_date_read_and_isbn$date_read, format = "%d/%m/%Y"), "%Y")
all_data_jt1_date_read_and_isbn$year_read <- as.numeric(as.character(all_data_jt1_date_read_and_isbn$year_read))
all_data_jt1_date_read_and_isbn$year_pub <- as.numeric(as.character(all_data_jt1_date_read_and_isbn$year_pub))
all_data_jt1_date_read_and_isbn$age_of_book <- all_data_jt1_date_read_and_isbn$year_read - all_data_jt1_date_read_and_isbn$year_pub
  

# Preparing original dataset
tryout <- all_data_jt1_date_read_and_isbn

# Create a new variable for time that captures biweekly intervals
tryout$biweek <- as.integer(format(tryout$date_read, "%U")) %/% 2

# Make sure the biweek variable starts at 0
tryout$biweek <- tryout$biweek - min(tryout$biweek)

# Create a new variable for the year
tryout$year <- as.integer(format(tryout$date_read, "%Y"))

# Divide stringency index by 10
tryout$stringency_index_divided_by_10 <- tryout$stringency_index/10

# Use the cut() function to create a new column with the appropriate level of restriction for each stringency index value
# Create a new column with the appropriate level of restriction for each stringency index value
cutoffs <- c(0/10, 0.01/10, 41.66/10, 83.33/10, 100/10)
labels <- c("no restrictions", "recommended restrictions", 
            "required closing of some areas", "required closing of most of society")
tryout$level_of_restriction <- cut(tryout$stringency_index, breaks = cutoffs, labels = labels, include.lowest = TRUE)

# Selecting relevant variables
relevant_variables <- tryout %>%
  select(user_id, date_read, user_rating, read_time_days, age_of_book, stringency_index, stringency_index_divided_by_10, biweek, year)

# Adding sum pages data
sum_pages <- tryout %>%
  group_by(user_id, biweek, year, Country, nr_ratings_by_user, avg_rating_given_by_user, nr_reviews_by_user, nr_books_read_by_user, Nr.Friends, Age, Male, Joined, Last, days_active, books_per_day, totalbooks_left) %>%
  summarise(stringency_index = mean(stringency_index, na.rm = TRUE),
            total_pages_read = sum(num_pages)) %>%
  distinct(user_id, biweek, year, .keep_all = TRUE)

# Select relevant sumpages data
sum_pages_relevant <- sum_pages %>%
  ungroup() %>%
  select(user_id, biweek, year, total_pages_read) 

# merge with relevant data
relevant_variables_1 <- merge(relevant_variables, sum_pages_relevant, by = c("user_id", "biweek", "year"))

# Remove irrelevant data
rm(sum_pages, sum_pages_relevant, relevant_variables, all_data_jt1_date_read_and_isbn)

# Adding the number of books data
sum_books <- tryout %>%
  group_by(user_id, biweek, year, Country, nr_ratings_by_user, avg_rating_given_by_user, nr_reviews_by_user, nr_books_read_by_user, Nr.Friends, Age, Male, Joined, Last, days_active, books_per_day, totalbooks_left) %>%
  summarise(stringency_index = mean(stringency_index, na.rm = TRUE),
            count = n()) %>%
  distinct(user_id, biweek, year, .keep_all = TRUE)

# select relevant sumbooks data
sum_books_relevant <- sum_books %>%
  ungroup() %>%
  select(user_id, biweek, year, count)

# merge with relevant data
relevant_variables_2 <- merge(relevant_variables_1, sum_books_relevant, by = c("user_id", "biweek", "year"))

relevant_variables_3 <- relevant_variables_2

# assuming your dataset is called relevant_variables_2
relevant_variables_2$dup <- duplicated(relevant_variables_2[, c("user_id", "biweek", "year")])
relevant_variables_2$total_pages_read[relevant_variables_2$dup] <- NA
relevant_variables_2$dup <- NULL

# Same but for count variable
relevant_variables_2$dup_count <- duplicated(relevant_variables_2[, c("user_id", "biweek", "year")])
relevant_variables_2$count[relevant_variables_2$dup_count] <- NA
relevant_variables_2$dup_count <- NULL

# Create a new column called "stringency_index_in_covid_era"
relevant_variables_2$stringency_index_in_covid_era <- ifelse(relevant_variables_2$date_read >= as.Date("2020-01-01"), 
                                                           relevant_variables_2$stringency_index, NA)

relevant_variables_2$stringency_index_in_covid_era_divided_by_10 <- relevant_variables_2$stringency_index_in_covid_era / 10


# Setting as dataframe for stargazer

relevant_variables_3 <- as.data.frame(relevant_variables_2)

# Filtering to only have observations cut observations above 97th percentile for Reading speed in days
relevant_variables_3 <- relevant_variables_3 %>%
  mutate(read_time_days = ifelse(read_time_days > quantile(read_time_days, 0.98, na.rm = TRUE), NA, read_time_days),
         age_of_book = ifelse(age_of_book > quantile(age_of_book, 0.98, na.rm = TRUE), NA, age_of_book),
         total_pages_read = ifelse(total_pages_read > quantile(total_pages_read, 0.98, na.rm = TRUE), NA, total_pages_read),
         count = ifelse(count > quantile(count, 0.98, na.rm = TRUE), NA, count))


# Select relevant variables for stargazer
relevant_variables_3 <- relevant_variables_3 %>%
  select(read_time_days, age_of_book, user_rating, total_pages_read, count, stringency_index_in_covid_era) %>%
  rename("Reading time per book" = read_time_days,
         "Age of book" = age_of_book,
         "User rating" = user_rating,
         "Number of total pages read" = total_pages_read,
         "Number of total books read" = count,
         "Stringency Index" = stringency_index_in_covid_era)

# Stargazer package
stargazer(relevant_variables_3, type = "html", iqr = TRUE, 
          median = TRUE, title = "Table 1: summary statistics for dependent and independent variables",
          out = "summary_statistics_bachelor_thesis.html")

# Set the file name and path
file_name <- "summary_table.tex"
file_path <- getwd()

# Run stargazer and save the output as a LaTeX file
cat(stargazer(relevant_variables_3, type = "latex"), file = file.path(file_path, file_name))


length(unique(tryout$user_id))
