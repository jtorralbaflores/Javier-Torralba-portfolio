post_ids <- instagram_data$post_id
expected_json_files <- paste0(post_ids, ".json")

json_files <- list.files(json_dir, pattern = "\\.json$", full.names = FALSE)

missing_files <- setdiff(json_files, expected_json_files)

head(missing_files)

json_file <- "D:/DSS Thesis/data_processing/metadata_pre_processing/relevant_JSON/1000147795992383306.json"

# Read the JSON file
json_data <- fromJSON(json_file)

# Let's get the damn data!

# Make a function to extract from the JSON file
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

# Get the data that you want
caption_is_edited <- safe_extract(json_data[["__typename"]]) #c
comments_disabled <- safe_extract(json_data[["comments_disabled"]]) #c
image_height <- safe_extract(json_data[["dimensions"]][["height"]]) #c
image_width <- safe_extract(json_data[["dimensions"]][["width"]]) #c
like_count <- safe_extract(json_data[["edge_media_preview_like"]][["count"]]) #c
comment_count <- safe_extract(json_data[["edge_media_to_comment"]][["count"]]) #c
post_id <- safe_extract(json_data[["id"]]) #c
is_video <- safe_extract(json_data[["is_video"]]) #c
media_preview <- safe_extract(json_data[["media_preview"]]) #c
owner_id <- safe_extract(json_data[["owner"]][["id"]]) #c
shortcode <- safe_extract(json_data[["shortcode"]]) #c
taken_at_timestamp <- safe_extract(json_data[["taken_at_timestamp"]]) #c
caption <- safe_extract(json_data[["edge_media_to_caption"]][["edges"]][["node"]][["text"]]) #c
is_ad <- safe_extract(json_data[["is_ad"]])

print(caption_is_edited)
print(comments_disabled)
print(image_height)
print(image_width)
print(like_count)
print(comment_count)
print(post_id)
print(is_video)
print(media_preview)
print(owner_id)
print(shortcode)
print(taken_at_timestamp)
print(caption)
print(sponsor_user)
print(is_ad)


test_2 <- data.frame(
  caption_is_edited = caption_is_edited,
  comments_disabled = comments_disabled,
  image_height = image_height,
  image_width = image_width,
  like_count = like_count,
  comment_count = comment_count,
  post_id = post_id,
  is_video = is_video,
  media_preview = media_preview,
  owner_id = owner_id,
  shortcode = shortcode,
  taken_at_timestamp = taken_at_timestamp,
  caption = caption,
  stringsAsFactors = FALSE,
  is_ad = is_ad
)
)