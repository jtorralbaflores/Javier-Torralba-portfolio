#Load libraries 
library(jsonlite)
library(purrr)
library(tidyjson)
library(data.table)
library(dplyr)

# Set working directory
setwd("D:/DSS Thesis/data_processing/metadata_pre_processing")

# Load relevant data
json_files_data <- fread("output/relevant_metadata_second_try.csv")
post_info_data <- fread("output/post_info_converted.csv")

# Use gsub to replace '.json' with '' (an empty string)
post_info_data$post_id <- gsub("\\.json$", "", post_info_data$`JSON file`)

# Let's merge the damn data!
merged <- merge(json_files_data, post_info_data, all.x = TRUE, by = "post_id")

# let's do a bit of EDA 
# Count occurrences of each value in the binary column
binary_distribution <- table(merged$`Sponsorship label`)

# Print the distribution
print(binary_distribution)

# Let's save the usernames we need
# Get the unique usernames
unique_usernames <- unique(merged$`USER name`)

# Create a new data frame with these usernames
usernames_df <- data.frame(username = unique_usernames)

# Save the data frame to a CSV file
fwrite(usernames_df, "output/relevant_usernames.csv", row.names = FALSE)

# Let's get the user data now
user_data <- fread("output/relevant_username_data.csv")

# Merge user data with post data
merged <- merged %>%
  rename(Username = `USER name`)

all_data <- merge(merged, user_data, by = 'Username')

# Save data
fwrite(all_data, "output/all_metadata.csv")
