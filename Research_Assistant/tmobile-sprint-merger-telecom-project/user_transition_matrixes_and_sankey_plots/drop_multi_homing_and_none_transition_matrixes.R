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

setwd("~/researchdrive/telecom/my_tests")

start_date <- "2019-04-01"
end_date <- "2022-04-01"


# --- START OF CHUNK 1 ---
#Loading data 
pq_path_start_date <- paste0("../user_choice_month/year_mon=", start_date)

df_start_date <- 
  open_dataset(
    sources = pq_path_start_date, 
    format = "parquet"
  ) %>% 
  collect()

pq_path_end_date <- paste0("../user_choice_month/year_mon=", end_date)

df_end_date <- 
  open_dataset(
    sources = pq_path_end_date, 
    format = "parquet"
  ) %>%
  collect()

#wrangling data and merging data
test_start_date <- df_start_date %>%
  mutate(category_used = ifelse(!is.na(companies_transacted), 1, 0))%>%
  filter(indicator == "TRUE") %>%
  pivot_wider(names_from = companies_transacted, values_from = category_used, values_fill = 0)

test_end_date <- df_end_date %>%
  mutate(category_used = ifelse(!is.na(companies_transacted), 1, 0))%>%
  filter(indicator == "TRUE") %>%
  pivot_wider(names_from = companies_transacted, values_from = category_used, values_fill = 0)

test_merged_data <- merge(test_start_date, test_end_date, by = "member_id", all = TRUE)

rm(df_end_date, df_start_date, test_end_date, test_start_date)

#Removing the rows that have NA values (removing all none values)
drop_na_test_merged_data <- drop_na(test_merged_data)

#Removing all multihoming
drop_na_and_multihoming_test_merged_data <- drop_na_test_merged_data %>%
  select(-at_t_and_sprint.x, 
         -at_t_and_verizon.x,
         -at_t_and_t_mobile.x,
         -verizon_and_t_mobile.x,
         -verizon_and_sprint.x,
         -sprint_and_t_mobile.x,
         -at_t_and_t_mobile.y,
         -at_t_and_verizon.y,
         -at_t_and_sprint.y,
         -verizon_and_t_mobile.y,
         -verizon_and_sprint.y,
         -sprint_and_t_mobile.y,
         -indicator.x, 
         -indicator.y)

rm(drop_na_test_merged_data, test_merged_data)

# --- END OF CHUNK 1 ---
# --- START OF CHUNK 2 ---

#Att client
drop_na_and_multihoming_test_merged_data$at_t_to_at_t <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_at_t.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_at_t.y == 1),
  1,
  0
)

drop_na_and_multihoming_test_merged_data$at_t_to_verizon <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_at_t.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_verizon.y == 1),
  1,
  0
)

drop_na_and_multihoming_test_merged_data$at_t_to_sprint <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_at_t.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_sprint.y == 1),
  1,
  0
)

drop_na_and_multihoming_test_merged_data$at_t_to_t_mobile <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_at_t.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_t_mobile.y == 1),
  1,
  0
)

#Verizon clients
drop_na_and_multihoming_test_merged_data$verizon_to_verizon <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_verizon.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_verizon.y == 1),
  1,
  0
)

drop_na_and_multihoming_test_merged_data$verizon_to_at_t <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_verizon.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_at_t.y == 1),
  1,
  0
)

drop_na_and_multihoming_test_merged_data$verizon_to_t_mobile <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_verizon.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_t_mobile.y == 1),
  1,
  0
)

drop_na_and_multihoming_test_merged_data$verizon_to_sprint <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_verizon.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_sprint.y == 1),
  1,
  0
)

#Sprint clients
drop_na_and_multihoming_test_merged_data$sprint_to_sprint <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_sprint.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_sprint.y == 1),
  1,
  0
)

drop_na_and_multihoming_test_merged_data$sprint_to_at_t <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_sprint.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_at_t.y == 1),
  1,
  0
)

drop_na_and_multihoming_test_merged_data$sprint_to_verizon <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_sprint.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_verizon.y == 1),
  1,
  0
)

drop_na_and_multihoming_test_merged_data$sprint_to_t_mobile <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_sprint.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_t_mobile.y == 1),
  1,
  0
)

#T-Mobile clients
drop_na_and_multihoming_test_merged_data$t_mobile_to_t_mobile <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_t_mobile.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_t_mobile.y == 1),
  1,
  0
)

drop_na_and_multihoming_test_merged_data$t_mobile_to_at_t <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_t_mobile.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_at_t.y == 1),
  1,
  0
)

drop_na_and_multihoming_test_merged_data$t_mobile_to_sprint <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_t_mobile.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_sprint.y == 1),
  1,
  0
)

drop_na_and_multihoming_test_merged_data$t_mobile_to_verizon <- ifelse(
  (drop_na_and_multihoming_test_merged_data$only_t_mobile.x == 1) &
    (drop_na_and_multihoming_test_merged_data$only_verizon.y == 1),
  1,
  0
)

# --- END OF CHUNK 2 ---
# --- START OF CHUNK 3 ---
#Let's add up the columns
total_columns <- c(
  "at_t_to_at_t",
  "at_t_to_verizon",
  "at_t_to_sprint",
  "at_t_to_t_mobile",
  "verizon_to_verizon",
  "verizon_to_at_t",
  "verizon_to_sprint",
  "verizon_to_t_mobile",
  "sprint_to_sprint",
  "sprint_to_at_t",
  "sprint_to_t_mobile",
  "sprint_to_verizon",
  "t_mobile_to_t_mobile",
  "t_mobile_to_sprint",
  "t_mobile_to_at_t",
  "t_mobile_to_verizon")

#Summing all columns
column_sums <-colSums(drop_na_and_multihoming_test_merged_data[total_columns], na.rm = TRUE)

print(column_sums)

# Define the row and column names
row_names <- c("at_t", "verizon", "t_mobile", "sprint")
column_names <- c("at_t", "verizon", "t_mobile", "sprint")

# Initialize the conversion matrix with zeros
conversion_matrix <- matrix(0, nrow = length(row_names), ncol = length(column_names), 
                            dimnames = list(row_names, column_names))
# Iterate over row and column names to assign values
for (row in row_names) {
  for (column in column_names) {
    conversion_matrix[row, column] <- column_sums[paste0(row, "_to_", column)]
  }
}

conversion_matrix <- as.data.frame(conversion_matrix)

# --- END OF CHUNK 3 ---

# --- START OF CHUNK 4 ---
# Let's try out the Sankey visualizations

data_long <- as.data.frame(conversion_matrix)
data_long$rowname <- row.names(conversion_matrix)

data_long <- data_long %>%
  gather(key = 'key', value = 'value', -rowname) %>%
  filter(value > 0)

colnames(data_long) <- c("source", "target", "value")
data_long$target <- paste(data_long$target, " ", sep="")

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
change_of_users_2019_04_to_2022_04 <- sankeyNetwork(Links = data_long, Nodes = nodes,
                      Source = "IDsource", Target = "IDtarget",
                      Value = "value", NodeID = "name", 
                      sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)

plot
# --- END OF CHUNK 4
# save the widget
library(htmlwidgets)
saveWidget(change_of_users_2019_04_to_2022_04, file=paste0(getwd(), "/transition_matrixes_august_september_2023/visualizations_and_transition_matrixes/2019_04_to_2022_04_plot.html"))

saveRDS(conversion_matrix, file = paste0(getwd(), "/transition_matrixes_august_september_2023/visualizations_and_transition_matrixes/2019_04_to_2022_04_matrix.RDS"))
