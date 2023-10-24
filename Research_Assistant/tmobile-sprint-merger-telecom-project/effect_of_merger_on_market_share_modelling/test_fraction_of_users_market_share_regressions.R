library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(ggplot2)
library(dplyr)
library(networkD3)
library(webshot)        
library(arrow)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(data.table)
library(fixest)
library(modelsummary)
library(kableExtra)
library(webshot)
library(stargazer)
library(plm)
library(gt)

setwd("~/researchdrive/telecom/Javiers_tests")

# Create a sequence of dates from January 2017 to June 2022, with monthly intervals
dates <- seq(as.Date("2017-01-01"), as.Date("2022-06-01"), by="1 month")

# Format the dates as character strings in the "YYYY-MM-01" format
dates <- format(dates, "%Y-%m-01")

# Initialize an empty data frame
combined_data <- data.frame()

# Loop through the dates and append data to the combined_data data frame,
# This also drops multihomers!
for (start_date in dates) {
  pq_path_start_date <- paste0("../user_choice_month/year_mon=", start_date)
  
  df_current_date <- 
    open_dataset(
      sources = pq_path_start_date, 
      format = "parquet"
    ) %>% 
    collect() %>%
    filter(indicator == TRUE) %>%
    filter(str_detect(companies_transacted, "only")) %>%
    mutate(date = as.Date(start_date)) 
  
  # Bind the data frame for the current date to the combined_data data frame
  combined_data <- bind_rows(combined_data, df_current_date)
}

# Wrangling data
test_my_df <- combined_data %>% 
  mutate(# Create a new column with numbers for each company
    at_t = ifelse(companies_transacted == "only_at_t", 1, 0),
    t_mobile = ifelse(companies_transacted == "only_t_mobile", 1, 0),
    sprint = ifelse(companies_transacted == "only_sprint", 1, 0),
    verizon = ifelse(companies_transacted == "only_verizon", 1, 0)
  ) %>% # Sum the number of users per cbsa and date for each company
  group_by(cbsa_name, date) %>%
  summarize(
    sum_at_t = sum(at_t),
    sum_t_mobile = sum(t_mobile),
    sum_sprint = sum(sprint),
    sum_verizon = sum(verizon)
  ) %>% 
  mutate(# compute market share for each of the companies
    market_share_per_cbsa_at_t = sum_at_t / (sum_at_t + sum_t_mobile + sum_sprint + sum_verizon),
    market_share_per_cbsa_t_mobile = sum_t_mobile / (sum_at_t + sum_t_mobile + sum_sprint + sum_verizon),
    market_share_per_cbsa_sprint = sum_sprint / (sum_at_t + sum_t_mobile + sum_sprint + sum_verizon),
    market_share_per_cbsa_verizon = sum_verizon / (sum_at_t + sum_t_mobile + sum_sprint + sum_verizon)
  ) %>% 
  mutate(#Divide dates depending on what was going on in the process 
    merger_state = case_when(
      date <= as.Date("2018-03-01") ~ "no_merger",
      between(date, as.Date("2018-04-01"), as.Date("2018-06-01")) ~ "merger_announcement",
      between(date, as.Date("2018-07-01"), as.Date("2020-01-01")) ~ "merger_bureaucracy",
      between(date, as.Date("2020-02-01"), as.Date("2020-04-01")) ~ "merger_officialization",
      between(date, as.Date("2020-05-01"), as.Date("2022-07-01")) ~ "post_merge",
      TRUE ~ NA_character_
    )) %>% 
      mutate(#Sum the size of the market in each CBSA
        size_of_cbsa = sum_at_t + sum_t_mobile + sum_sprint + sum_verizon) %>%
  mutate(year = year(date),
         month = month(date))

# Convert the merger_state variable to a factor
test_my_df$merger_state <- factor(test_my_df$merger_state)

# Reorder the levels with "no_merger" as the baseline
test_my_df$merger_state <- relevel(test_my_df$merger_state, ref = "no_merger")


# AT_T REGRESSIONS
at_t_regression_1 <- feols(market_share_per_cbsa_at_t ~ merger_state,
                         data = test_my_df) 

at_t_regression_2 <- feols(market_share_per_cbsa_at_t ~ merger_state | cbsa_name,
                        data = test_my_df)

at_t_regression_3 <- feols(market_share_per_cbsa_at_t ~ merger_state | 
                             cbsa_name + factor(month),
                           data = test_my_df, cluster = c("month", "cbsa_name"))

at_t_regression_4 <- feols(market_share_per_cbsa_at_t ~ merger_state | 
                             cbsa_name + factor(month) + factor(year),
                            data = test_my_df, cluster = c("month", "cbsa_name"))

at_t_regression_5 <- feols(market_share_per_cbsa_at_t ~ merger_state | 
                          cbsa_name + factor(month) + factor(year) + factor(size_of_cbsa),
                           data = test_my_df, cluster = c("month", "cbsa_name"))

# ATT results on modelsummary
at_t_regressions <- list(at_t_regression_1, at_t_regression_2, at_t_regression_3, 
                         at_t_regression_4, at_t_regression_5)

modelsummary(at_t_regressions,
             stars = TRUE, 
             fmt = fmt_sprintf("%.3f"),
             gof_omit = "AIC|BIC|RMSE|Adj|Within",
             title = "At&t market share at merger states",
             output = "gt")

#verizon_regression 
verizon_regression_1 <- feols(market_share_per_cbsa_verizon ~ merger_state,
                           data = test_my_df) 

verizon_regression_2 <- feols(market_share_per_cbsa_verizon ~ merger_state | cbsa_name,
                           data = test_my_df)

verizon_regression_3 <- feols(market_share_per_cbsa_verizon ~ merger_state | 
                                cbsa_name + factor(month),
                              data = test_my_df, cluster = c("month", "cbsa_name"))

verizon_regression_4 <- feols(market_share_per_cbsa_verizon ~ merger_state | 
                             cbsa_name + factor(month) + factor(year),
                           data = test_my_df, cluster = c("month", "cbsa_name"))

verizon_regression_5 <- feols(market_share_per_cbsa_verizon ~ merger_state | 
                             cbsa_name + factor(month) + factor(year) + factor(size_of_cbsa),
                           data = test_my_df, cluster = c("month", "cbsa_name"))

# Verizon results on modelsummary
verizon_regressions <- list(verizon_regression_1, verizon_regression_2, verizon_regression_3, 
                         verizon_regression_4, verizon_regression_5)

modelsummary(verizon_regressions,
             stars = TRUE, 
             fmt = fmt_sprintf("%.3f"),
             gof_omit = "AIC|BIC|RMSE|Adj|Within",
             title = "Verizon market share at merger states",
             output = "gt")


#T_Mobile_regression 
t_mobile_regression_1 <- feols(market_share_per_cbsa_t_mobile ~ merger_state,
                              data = test_my_df) 

t_mobile_regression_2 <- feols(market_share_per_cbsa_t_mobile ~ merger_state | cbsa_name,
                              data = test_my_df)

t_mobile_regression_3 <- feols(market_share_per_cbsa_t_mobile ~ merger_state | 
                              cbsa_name + factor(month),
                               data = test_my_df, cluster = c("month", "cbsa_name"))

t_mobile_regression_4 <- feols(market_share_per_cbsa_t_mobile ~ merger_state | 
                                cbsa_name + factor(month) + factor(year),
                              data = test_my_df, cluster = c("month", "cbsa_name"))

t_mobile_regression_5 <- feols(market_share_per_cbsa_t_mobile ~ merger_state | 
                                cbsa_name + factor(month) + factor(year) + factor(size_of_cbsa),
                              data = test_my_df, cluster = c("month", "cbsa_name"))

# Verizon results on modelsummary
t_mobile_regressions <- list(t_mobile_regression_1, t_mobile_regression_2, t_mobile_regression_3, 
                             t_mobile_regression_4, t_mobile_regression_5)

modelsummary(t_mobile_regressions,
             stars = TRUE, 
             fmt = fmt_sprintf("%.3f"),
             gof_omit = "AIC|BIC|RMSE|Adj|Within",
             title = "T-Mobile market share at merger states",
             output = "gt")


#Sprint_regression 
sprint_regression_1 <- feols(market_share_per_cbsa_sprint ~ merger_state,
                               data = test_my_df) 

sprint_regression_2 <- feols(market_share_per_cbsa_sprint ~ merger_state | cbsa_name,
                               data = test_my_df)

sprint_regression_3 <- feols(market_share_per_cbsa_sprint ~ merger_state | 
                                 cbsa_name + factor(month),
                               data = test_my_df, cluster = c("month", "cbsa_name"))

sprint_regression_4 <- feols(market_share_per_cbsa_sprint ~ merger_state | 
                                 cbsa_name + factor(month) + factor(year),
                               data = test_my_df, cluster = c("month", "cbsa_name"))

sprint_regression_5 <- feols(market_share_per_cbsa_sprint ~ merger_state | 
                                 cbsa_name + factor(month) + factor(year) + factor(size_of_cbsa),
                               data = test_my_df, cluster = c("month", "cbsa_name"))

# Sprint results on modelsummary
sprint_regressions <- list(sprint_regression_1, sprint_regression_2, sprint_regression_3, 
                           sprint_regression_4, sprint_regression_5)

modelsummary(sprint_regressions,
             stars = TRUE, 
             fmt = fmt_sprintf("%.3f"),
             gof_omit = "AIC|BIC|RMSE|Adj|Within",
             title = "Sprint market share at merger states",
             output = "gt")



