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
  x$title <- unlist(x$title, recursive = FALSE)
  # Add database class
  class(x) <- "notionr_db"
  return(x)
}

# Method for summarizing database
summary.notionr_db <- function(x, ..., start.with = "\r") {
  formatted_db <- format(x)
  cat(formatted_db)
}

# Method for printing database
print.notionr_db <- function(x, ...) {
  els <- c("Title" = replace_null_zchar(x$title$plain_text),
           "Id" = replace_null_zchar(x$id),
           "Created date" = replace_null_zchar(as.character(as.Date(x$created_time))),
           "Last edited date" = replace_null_zchar(as.character(as.Date(x$last_edited_time))),
           "Parent type" = replace_null_zchar(x$parent$type),
           "Parent Id" = replace_null_zchar(x$parent[[2]]))
  cat(
    paste0(names(els), 
           ": ", 
           els,
           collapse = "\n\r")
  )
  invisible(x)
}

# Method for formatting database information
format.notionr_db <- function(x, ..., start.with = "\r") {
  unlist(
    lapply(names(x), function(i) {
      if (!is.list(x[[i]])) {
        return(paste0(start.with, "", i, ": ", x[[i]], "\n"))
      } else {
        c(paste0(start.with, "", i, ":\n"),
          unlist(format.notionr_db(x[[i]], start.with = paste0(start.with, "    "))))
      }
    })
  )
}

# Database method for generic 'properties'
properties.notionr_db <- function(x) {
  dplyr::bind_rows(
    lapply(names(x$properties), function(i) {
      id <- ifelse(is.null(x$properties[[i]]$id), NA, x$properties[[i]]$id)
      type <- ifelse(is.null(x$properties[[i]]$type), NA, x$properties[[i]]$type)
      dplyr::tibble("field" = i, "id" = id, "type" = type)
    })
  )
}

# Database method for generic 'id'
id.notionr_db <- function(x) {
  name <- ifelse(is.null(x$title$plain_text),
                 NA,
                 x$title$plain_text)
  dplyr::tibble("name" = name, "id" = x$id)
}

# Function that lists every database object
list_databases <- function(key) {
  stopifnot(is.character(key))
  # Empty content list to grab content from each page
  content_ls <- list()
  recurse_cursors_get(endpoint = "https://api.notion.com/v1/databases", 
                      key = key)
  unlist(
    lapply(content_ls, function(i) {
      lapply(i$results, new_database)
    }),
    recursive = FALSE
  )
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

# Function to retrieve a database
retrieve_database <- function(key, database.id) {
  url <- sprintf("https://api.notion.com/v1/databases/%s", database.id)
  db <- httr::content(
    httr::stop_for_status(
      httr::GET(
        url,
        httr::add_headers("Authorization" = paste("Bearer", key),
                          "Notion-Version" = "2021-05-13")
      )
    )
  )
  db_out <- new_database(db)
  return(db_out)
}
