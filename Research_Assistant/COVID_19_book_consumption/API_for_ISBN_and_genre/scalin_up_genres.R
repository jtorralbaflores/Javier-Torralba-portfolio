library(rjson)
library(dplyr)
library(purrr)

# --- CODE BY LACHLAN ---

# find all json files in folder
list.files("out/data")


# for each book file
in_file <- "out/data/9780062870674.json"
isbn <- tools::file_path_sans_ext(basename(in_file))

test <- fromJSON(file = in_file)

test2 <- tibble::tibble(subject = test$subjects,
                        isbn = isbn)
# --- END OF CODE BY LACHLAN ---

# --- Start of code  by me ---
# Preaparing file path
file_paths <- list.files("out/data/")
file_paths <- paste0("out/data/", file_paths)

# Creating a function
my_function <- function(file_path) {
  data <- fromJSON(file = file_path)
  if ("subjects" %in% names(data)){
    subjects <- data$subjects
  } else {
    subjects <- NA_character_
  }
  isbn <- tools::file_path_sans_ext(basename(file_path))
  df <- tibble::tibble(isbn = isbn, subjects = subjects)
  return(as.data.frame(df))
}

#looping over function to create dataframe
isbn_and_subjects_sample <- map_df(file_paths, my_function)

# Saving the sample as a CSV
library(data.table)
fwrite(isbn_and_subjects_sample, "isbn_and_subjects_sample.csv")
 