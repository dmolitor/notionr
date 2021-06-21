# Constructor for notion database class
new_database <- function(x) {
  stopifnot(is.list(x))
  if (
    !all(
      c("object", 
        "id", 
        "created_time",
        "last_edited_time",
        "title",
        "properties") %in% names(x)
    )
  ) {
    stop("Object is missing essential database fields", call. = FALSE)
  }
  # Unnest database title
  x$title <- x$title[[1]]
  # Add database class
  class(x) <- "notionr_db"
  return(x)
}

# Method for summarizing database
summary.notionr_db <- function(x, ..., start.with = "\r") {
  formatted_db <- unlist(
    lapply(names(x), function(i) {
      if (!is.list(x[[i]])) {
        return(paste0(start.with, "", i, ": ", x[[i]], "\n"))
      } else {
        c(paste0(start.with, "", i, ":\n"),
          unlist(format.notion.database(x[[i]], start.with = paste0(start.with, "    "))))
      }
    })
  )
  cat(formatted_db)
}

# Function that lists every database object
list_databases <- function(key) {
  stopifnot(is.character(key))
  # Empty content list to grab content from each page
  content_ls <- list()
  recurse_cursors_get(endpoint = "https://api.notion.com/v1/databases", 
                      key = key)
  # # Determine which elements are empty
  # empty_idx <- unlist(lapply(content_ls, function(i) identical(i$results, list())))
  # if (all(empty_idx)) {
  #   warning("No databases found", call. = FALSE, immediate. = TRUE)
  #   return(jsonlite::toJSON(content_ls[[1]], auto_unbox = TRUE))
  # }
  # content_ls <- content_ls[!empty_idx]
  # jsonlite::toJSON(content_ls, auto_unbox = TRUE)
  content_ls <- unlist(
    lapply(content_ls, function(i) {
      lapply(i$results, new_database)
    }),
    recursive = FALSE
  )
  return(content_ls)
}

# Function that returns all database IDs
list_database_ids <- function(key) {
  dplyr::bind_rows(
    lapply(
      list_databases(key),
      id
    )
  )
}
