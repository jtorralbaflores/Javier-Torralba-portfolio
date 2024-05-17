# Load libraries
library(dplyr)
library(ggplot2)
library(data.table)

# Set WD
setwd("D:/DSS Thesis/EDA/Actual_thesis")

# Load data 
metadata <- fread("D:/DSS Thesis/data_processing/metadata_pre_processing/output/all_metadata.csv")

# Histogram likes distribution
ggplot(metadata, aes(x = like_count)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Likes", x = "Number of Likes", y = "Frequency")

# Assuming 'is_sponsored' is a binary variable indicating whether a post is sponsored
ggplot(metadata, aes(x = `Sponsorship label`, y = like_count, fill = `Sponsorship label`)) +
  geom_boxplot() +
  labs(title = "Likes Distribution for Sponsored vs Non-Sponsored Posts", x = "Post Type", y = "Likes")

# Likes vs comments
comments_and_likes <- ggplot(metadata, aes(x = comment_count, y = like_count)) +
  geom_point(alpha = 0.5) +
  labs(title = "Relationship Between Comments and Likes", x = "Comments", y = "Likes")

ggsave("visualizations/comments_and_likes.jpg", comments_and_likes, height = 5, width = 10)
