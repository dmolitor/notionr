# Constructor for notion page class
new_page <- function(x) {
  stopifnot(is.list(x))
  if (
    !all(
      c("object", 
        "id", 
        "created_time",
        "last_edited_time",
        "parent",
        "archived",
        "properties") %in% names(x)
    )
  ) {
    stop("Object is missing essential page fields", call. = FALSE)
  }
  # Unnest title page
  x$properties$Name$title <- unlist(x$properties$Name$title, recursive = FALSE)
  # Add database class
  class(x) <- "notionr_page"
  return(x)
}

# Method for formatting page information
format.notionr_page <- function(x, ..., start.with = "\r") {
  unlist(
    lapply(names(x), function(i) {
      if (!is.list(x[[i]])) {
        return(paste0(start.with, "", i, ": ", x[[i]], "\n"))
      } else {
        c(paste0(start.with, "", i, ":\n"),
          unlist(format.notionr_page(x[[i]], start.with = paste0(start.with, "    "))))
      }
    })
  )
}

# Method for printing database
print.notionr_page <- function(x, ...) {
  els <- c("Title" = ifelse(is.null(x$properties$Name$title$plain_text),
                            "",
                            x$properties$Name$title$plain_text),
           "Id" = x$id,
           "Created date" = as.character(as.Date(x$created_time)),
           "Last edited date" = as.character(as.Date(x$last_edited_time)),
           "Parent type" = x$parent$type,
           "Parent Id" = x$parent[[2]])
  cat(
    paste0(names(els), 
           ": ", 
           els,
           collapse = "\n\r")
  )
}

# Method for summarizing page
summary.notionr_page <- function(x, ..., start.with = "\r") {
  formatted_page <- format(x)
  cat(formatted_page)
}

# Page method for generic 'properties'
properties.notionr_page <- function(x) {
  dplyr::bind_rows(
    lapply(names(x$properties), function(i) {
      id <- ifelse(is.null(x$properties[[i]]$id), NA, x$properties[[i]]$id)
      type <- ifelse(is.null(x$properties[[i]]$type), NA, x$properties[[i]]$type)
      dplyr::tibble("field" = i, "id" = id, "type" = type)
    })
  )
}

# Page method for generic 'children'
children.notionr_page <- function(x) {
  key <- attributes(x)$key
  page.id <- x$id
  retrieve_block_children(key, page.id)
}

# Database method for generic 'id'
id.notionr_page <- function(x) {
  name <- ifelse(is.null(x$properties$Name$title$plain_text),
                 NA,
                 x$properties$Name$title$plain_text)
  dplyr::tibble("name" = name, "id" = x$id)
}

# Queries database and returns list of pages based on the query body
query_database <- function(database.id, key, query.body = NULL) {
  # Empty content list
  content_ls <- list()
  recurse_cursors_pages(database.id, key, query.body)
  content_ls <- unlist(
    lapply(content_ls, function(i) {
      lapply(i$results, new_page)
    }),
    recursive = FALSE
  )
  lapply(content_ls, function(i) {
    `attr<-`(i, "key", key)
  })
}

# Function to retrieve a database
retrieve_page <- function(key, page.id) {
  url <- paste0("https://api.notion.com/v1/pages/", page.id)
  page <- httr::content(
    httr::stop_for_status(
      httr::GET(
        url,
        httr::add_headers("Authorization" = paste("Bearer", key),
                          "Notion-Version" = "2021-05-13")
      )
    )
  )
  page_out <- new_page(page)
  `attr<-`(page_out, "key", key)
}

# List all pages in a database based on a query filter/sort
list_page_ids <- function(database.id, key, query.body = NULL) {
  dplyr::bind_rows(
    lapply(
      query_database(database.id, key, query.body),
      id
    )
  )
}
