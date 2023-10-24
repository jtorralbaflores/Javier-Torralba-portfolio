library(plm)
library(lmtest)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(tidyr)
library(car)
library(fixest)
library(knitr)
library(kableExtra)
library(stargazer)
library(gt)

#Data prep

# Loading and wrangling the data
all_data_jt1_date_read_and_isbn <- read_csv("Data wrangling for Thesis/Preparation of datasets/all_data_jt1_date_read_and_isbn.csv")

tryout <- all_data_jt1_date_read_and_isbn

# Create a new variable for time that captures biweekly intervals
tryout$biweek <- as.integer(format(tryout$date_read, "%U")) %/% 2

# Make sure the biweek variable starts at 0
tryout$biweek <- tryout$biweek - min(tryout$biweek)

# Create a new variable for the year
tryout$year <- as.integer(format(tryout$date_read, "%Y"))

# Divide stringency index by 10
tryout$stringency_index <- tryout$stringency_index/10

# Use the cut() function to create a new column with the appropriate level of restriction for each stringency index value
# Create a new column with the appropriate level of restriction for each stringency index value
cutoffs <- c(0/10, 0.01/10, 41.66/10, 83.33/10, 100/10)
labels <- c("no restrictions", "recommended restrictions", 
            "required closing of some areas", "required closing of most of society")
tryout$level_of_restriction <- cut(tryout$stringency_index, breaks = cutoffs, labels = labels, include.lowest = TRUE)

tryout <- tryout %>%
  mutate(read_time_days = ifelse(read_time_days > quantile(read_time_days, 0.98, na.rm = TRUE), NA, read_time_days),
         age_of_book = ifelse(age_of_book > quantile(age_of_book, 0.98, na.rm = TRUE), NA, age_of_book))
# ---REGRESSION---

# USER RATING
fe_reg_user_rating_user_id_stringency_index <- feols(log(user_rating) ~ stringency_index | factor(biweek) + factor(year) +
                              user_id + book_url, data = tryout, cluster = c("Country", "biweek"))

fe_reg_user_rating_user_id_adapted_stringency <- feols(log(user_rating) ~ level_of_restriction | factor(biweek) + factor(year) +
                                      user_id + book_url, data = tryout, cluster = c("Country", "biweek"))

gc()

#On days it takes to read a book
fe_reg_read_time_days_user_id_stringency_index <- feols(log(read_time_days) ~ stringency_index | factor(biweek) + factor(year) +
                                         user_id + book_url, data = tryout, cluster = c("Country", "biweek"))

fe_reg_read_time_days_user_id_stringency_index_no_book_FE <- feols(log(read_time_days) ~ stringency_index | factor(biweek) + factor(year) +
                                                          user_id, data = tryout, cluster = c("Country", "biweek"))

fe_reg_read_time_days_user_id_adapted_stringency <- feols(log(read_time_days) ~ level_of_restriction | factor(biweek) + factor(year) +
                                         user_id + book_url, data = tryout, cluster = c("Country", "biweek"))

fe_reg_read_time_days_user_id_adapted_stringency_no_book_FE <- feols(log(read_time_days) ~ level_of_restriction | factor(biweek) + factor(year) +
                                                            user_id, data = tryout, cluster = c("Country", "biweek"))
gc()

#On the age of a book
fe_reg_age_of_book_user_id_stringency_index <- feols(log(age_of_book) ~ stringency_index | factor(biweek) + factor(year) +
                              user_id, data = tryout, cluster = c("Country", "biweek"))

fe_reg_age_of_book_user_id_adapted_stringency <- feols(log(age_of_book) ~ level_of_restriction | factor(biweek) + factor(year) +
                                      user_id, data = tryout, cluster = c("Country", "biweek"))

gc()
#On number of books read

# ---RESULTS---
etable(fe_reg_user_rating_user_id_stringency_index, fe_reg_read_time_days_user_id_stringency_index, fe_reg_age_of_book_user_id_stringency_index)
etable(fe_reg_user_rating_user_id_adapted_stringency, fe_reg_read_time_days_user_id_adapted_stringency, fe_reg_age_of_book_user_id_adapted_stringency)


etable(fe_reg_num_pages_, fe_reg_num_pages_user_id, fe_reg_age_of_book_Country, fe_reg_age_of_book_user_id)

# TRYING FOR NUMBER OF PAGES
#Wrangling data
sum_pages <- tryout %>%
  group_by(user_id, biweek, year, Country, nr_ratings_by_user, avg_rating_given_by_user, nr_reviews_by_user, nr_books_read_by_user, Nr.Friends, Age, Male, Joined, Last, days_active, books_per_day, totalbooks_left) %>%
  summarise(stringency_index = mean(stringency_index, na.rm = TRUE),
            total_pages_read = sum(num_pages)) %>%
  distinct(user_id, biweek, year, .keep_all = TRUE)

sum_pages <- sum_pages %>%
  mutate(total_pages_read = ifelse(total_pages_read > quantile(total_pages_read, 0.98, na.rm = TRUE), NA, total_pages_read))

sum_pages$level_of_restriction <- cut(sum_pages$stringency_index, breaks = cutoffs, labels = labels, include.lowest = TRUE)

# Number of pages regression
reg_FE_sum_num_pages_stringency_index <- feols(log(total_pages_read) ~ stringency_index | factor(biweek) + factor(year) +
                                user_id, data = sum_pages, cluster = c("Country", "biweek"))

reg_FE_sum_num_pages_adapted_stringency <- feols(log(total_pages_read) ~ level_of_restriction | factor(biweek) + factor(year) +
                                user_id, data = sum_pages, cluster = c("Country", "biweek"))

gc()
#Wrnagling data
sum_books <- tryout %>%
  group_by(user_id, biweek, year, Country, nr_ratings_by_user, avg_rating_given_by_user, nr_reviews_by_user, nr_books_read_by_user, Nr.Friends, Age, Male, Joined, Last, days_active, books_per_day, totalbooks_left) %>%
  summarise(stringency_index = mean(stringency_index, na.rm = TRUE),
            count = n()) %>%
  distinct(user_id, biweek, year, .keep_all = TRUE)

sum_books <- sum_books %>%
  mutate(count = ifelse(count > quantile(count, 0.98, na.rm = TRUE), NA, count))

sum_books$level_of_restriction <- cut(sum_books$stringency_index, breaks = cutoffs, labels = labels, include.lowest = TRUE)

# Number of books regression
reg_FE_num_books_stringency_index <- feols(log(count) ~ stringency_index | factor(biweek) + factor(year) +
                                user_id, data = sum_books, cluster = c("Country", "biweek"))

reg_FE_num_books_adapted_stringency <- feols(log(count) ~ level_of_restriction | factor(biweek) + factor(year) +
                            user_id, data = sum_books, cluster = c("Country", "biweek"))

gc()
# RESULTS FOR BOTH NUMBER OF PAGES READ AND BOOKS READ BY USERS
etable(fe_reg_user_rating_user_id_stringency_index, fe_reg_read_time_days_user_id_stringency_index, fe_reg_age_of_book_user_id_stringency_index, reg_FE_num_books_stringency_index, reg_FE_sum_num_pages_stringency_index)

etable(fe_reg_user_rating_user_id_adapted_stringency, fe_reg_read_time_days_user_id_adapted_stringency, fe_reg_age_of_book_user_id_adapted_stringency, reg_FE_num_books_adapted_stringency, reg_FE_sum_num_pages_adapted_stringency)

# --- WORKING WITH MODEL SUMMARY ---
library(modelsummary)
library(kableExtra)
library(webshot2)

book_level_results_regular_stringency <- list("Log (Reading time per book)" = fe_reg_read_time_days_user_id_stringency_index,
                                              "Log (Reading time per book)" = fe_reg_read_time_days_user_id_stringency_index_no_book_FE,
               "Log (Age of book)" = fe_reg_age_of_book_user_id_stringency_index,
               "Log (User rating)" = fe_reg_user_rating_user_id_stringency_index)

user_level_resutls_regular_stringency <- list("Log (Number of pages read by a user)" = reg_FE_sum_num_pages_stringency_index,
                            "Log (Number of books read by a user)" = reg_FE_num_books_stringency_index)

book_level_results_regular_stringency_FINAL <- modelsummary(book_level_results_regular_stringency,  
                                         stars = TRUE, 
                                         fmt = fmt_sprintf("%.3f"),
                                         coef_rename = c("stringency_index" = "COVID stringency",
                                                         "FE: factor(biweek)" = "FE: Biweek"),
                                         gof_omit = "AIC|BIC|RMSE|Adj|Within",
                                         title = "Table 2: linear model book level resutls",
                                         output = "gt")

user_level_results_regular_stringency_FINAL <- modelsummary(user_level_resutls_regular_stringency,  
                                                            stars = TRUE, 
                                                            fmt = fmt_sprintf("%.3f"),
                                                            coef_rename = c("stringency_index" = "COVID stringency"),
                                                            gof_omit = "AIC|BIC|RMSE|Adj|Within",
                                                            title = "Table 3: linear model user level resutls",
                                                            output = "gt")


gt::gtsave(book_level_results_regular_stringency_FINAL, file = "Results from R/book_level_results_regular_stringency_FINAL_NEW.png")
gt::gtsave(user_level_results_regular_stringency_FINAL, file = "Results from R/user_level_results_regular_stringency_FINAL_NEW.png")



book_level_results_adapted_stringency <- list("Log (Reading time per book)" = fe_reg_read_time_days_user_id_adapted_stringency,
                                              "Log (Reading time per book)" = fe_reg_read_time_days_user_id_adapted_stringency_no_book_FE,
                                  "Log (Age of book)" = fe_reg_age_of_book_user_id_adapted_stringency,
                                  "Log (User rating)" = fe_reg_user_rating_user_id_adapted_stringency)

user_level_results_adapted_stringency <- list("Log (Number of pages read by a user)" = reg_FE_sum_num_pages_adapted_stringency,
                                              "Log (Number of books read by a user)" = reg_FE_num_books_adapted_stringency)



book_level_results_adapted_stringency_FINAL <- modelsummary(book_level_results_adapted_stringency,  stars = TRUE, fmt = fmt_sprintf("%.3f"),
             coef_rename = c("level_of_restrictionrecommended restrictions" = "Light restrictions or recommended restrictions",
                             "level_of_restrictionrequired closing of some areas" = "Required closing of some of society",
                             "level_of_restrictionrequired closing of most of society" = "Required closing of all but essential parts of society"),
             gof_omit = "AIC|BIC|RMSE|Adj|Within",
             title = "Table 4: Differential effect of COVID stringencies at book level resutls",
             output = "gt")

user_level_results_adapted_stringency_FINAL <- modelsummary(user_level_results_adapted_stringency,  stars = TRUE, fmt = fmt_sprintf("%.3f"),
                                                            coef_rename = c("level_of_restrictionrecommended restrictions" = "Light restrictions or recommended restrictions",
                                                                            "level_of_restrictionrequired closing of some areas" = "Required closing of some of society",
                                                                            "level_of_restrictionrequired closing of most of society" = "Required closing of all but essential parts of society"),
                                                            gof_omit = "AIC|BIC|RMSE|Adj|Within",
                                                            title = "Table 5: Differential effect of COVID stringencies at user level results",
                                                            output = "gt")

gt::gtsave(user_level_results_adapted_stringency_FINAL, file = "Results from R/user_level_results_adapted_stringency_FINAL_NEW.png")
gt::gtsave(book_level_results_adapted_stringency_FINAL, file = "Results from R/book_level_results_adapted_stringency_FINAL_NEW.png")

results_stringency_index
results_adapted_stringency

etable(fe_reg_read_time_days_user_id_adapted_stringency)

# results in latex
modelsummary(book_level_results_regular_stringency,  
             stars = TRUE, 
             fmt = fmt_sprintf("%.3f"),
             coef_rename = c("stringency_index" = "COVID stringency",
                             "FE: factor(biweek)" = "FE: Biweek"),
             gof_omit = "AIC|BIC|RMSE|Adj|Within",
             title = "Table 2: linear model book level resutls",
             output = "latex")

modelsummary(user_level_resutls_regular_stringency,  
             stars = TRUE, 
             fmt = fmt_sprintf("%.3f"),
             coef_rename = c("stringency_index" = "COVID stringency"),
             gof_omit = "AIC|BIC|RMSE|Adj|Within",
             title = "Table 3: linear model user level resutls",
             output = "latex")

modelsummary(book_level_results_adapted_stringency,  stars = TRUE, fmt = fmt_sprintf("%.3f"),
             coef_rename = c("level_of_restrictionrecommended restrictions" = "Light restrictions or recommended restrictions",
                             "level_of_restrictionrequired closing of some areas" = "Required closing of some of society",
                             "level_of_restrictionrequired closing of most of society" = "Required closing of all but essential parts of society"),
             gof_omit = "AIC|BIC|RMSE|Adj|Within",
             title = "Table 4: Differential effect of COVID stringencies at book level resutls",
             output = "latex")

modelsummary(user_level_results_adapted_stringency,  stars = TRUE, fmt = fmt_sprintf("%.3f"),
             coef_rename = c("level_of_restrictionrecommended restrictions" = "Light restrictions or recommended restrictions",
                             "level_of_restrictionrequired closing of some areas" = "Required closing of some of society",
                             "level_of_restrictionrequired closing of most of society" = "Required closing of all but essential parts of society"),
             gof_omit = "AIC|BIC|RMSE|Adj|Within",
             title = "Differential effect of COVID stringencies at user level results",
             output = "latex")

# Testing hypothesis
coef(fe_reg_read_time_days_user_id_adapted_stringency)

#Significant differences for reading speed in days
linearHypothesis(fe_reg_read_time_days_user_id_adapted_stringency, "level_of_restrictionrequired closing of some areas = level_of_restrictionrequired closing of most of society")

#Significant differences for Age of book
linearHypothesis(fe_reg_age_of_book_user_id_adapted_stringency, "level_of_restrictionrequired closing of some areas = level_of_restrictionrequired closing of most of society")

#Significant differences for number of pages read by  user
linearHypothesis(reg_FE_sum_num_pages_adapted_stringency, "level_of_restrictionrequired closing of some areas = level_of_restrictionrequired closing of most of society")

#significant differences for number of books read by a user
linearHypothesis(reg_FE_num_books_adapted_stringency, "level_of_restrictionrequired closing of some areas = level_of_restrictionrequired closing of most of society")
Thqnk