#Load libraries 
library(jsonlite)
library(purrr)
library(tidyjson)
library(data.table)
library(dplyr)

# Set working directory
setwd("D:/DSS Thesis/data_processing/metadata_pre_processing")

# Define the path to your JSON files
json_dir <- "D:/DSS Thesis/data_processing/metadata_pre_processing/relevant_JSON"

# List all JSON files
json_files <- list.files(json_dir, pattern = "*.json", full.names = TRUE)

# Define a function to safely extract data from JSON
safe_extract <- function(data, ...) {
  keys <- list(...)
  for (key in keys) {
    if (is.null(data) || !key %in% names(data)) {
      return(NA)
    } else {
      data <- data[[key]]
    }
  }
  # Ensure the function returns a vector of length 1
  return(ifelse(length(data) == 0, NA, data))
}

# Function to process each JSON file
process_json <- function(file_path) {
  # Read JSON file
  json_data <- fromJSON(file_path)
  
  # Extract data
  data.frame(
    caption_is_edited = safe_extract(json_data, "__typename"),
    comments_disabled = safe_extract(json_data, "comments_disabled"),
    image_height = safe_extract(json_data[["dimensions"]], "height"),
    image_width = safe_extract(json_data[["dimensions"]], "width"),
    like_count = safe_extract(json_data[["edge_media_preview_like"]], "count"),
    comment_count = safe_extract(json_data[["edge_media_to_comment"]], "count"),
    post_id = safe_extract(json_data, "id"),
    is_video = safe_extract(json_data, "is_video"),
    media_preview = safe_extract(json_data, "media_preview"),
    owner_id = safe_extract(json_data[["owner"]], "id"),
    shortcode = safe_extract(json_data, "shortcode"),
    taken_at_timestamp = safe_extract(json_data, "taken_at_timestamp"),
    caption = safe_extract(json_data[["edge_media_to_caption"]][["edges"]][1][["node"]], "text"),
    is_ad = safe_extract(json_data, "is_ad"),
    stringsAsFactors = FALSE
  )
}

# Use purrr to process all JSON files and combine into a single dataframe
instagram_data_2 <- map_dfr(json_files, possibly(process_json, otherwise = data.frame()))

# Save the final dataframe to a CSV file
fwrite(instagram_data_2, "output/relevant_metadata_second_try.csv")

