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
  # Add database class
  class(x) <- "notionr_page"
  classify_page_properties(x)
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
  els <- c("Title" = replace_null_zchar(x$properties$Name$title$plain_text),
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
children.notionr_page <- function(x, recursive = TRUE) {
  stopifnot(recursive %in% c(TRUE, FALSE))
  f <- ifelse(recursive, 
              retrieve_block_children_recursive,
              retrieve_block_children_nonrecursive)
  key <- attributes(x)$key
  page.id <- x$id
  f(key, page.id)
}

# Object content method for class page
object_content.notionr_page <- function(x) {
  lapply(x$properties, function(i) {
    object_content(i[[i$type]])
  })
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
  url <- paste0("https://api.notion.com/v1/databases/", database.id, "/query")
  # Empty content list
  content_ls <- list()
  recurse_cursors_post(url, key, query.body)
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
list_page_ids <- function(key, query = NULL, sort = NULL) {
  pages <- search(key, query = query, sort = sort, filter = search_filter())
  dplyr::tibble(
    "id" = unlist(lapply(pages, function(i) i$id)),
    "title" = unlist(lapply(pages, title))
  )
}

# Create title method for class notionr_page
title.notionr_page <- function(x, all.titles = FALSE) {
  title_property <- which(unlist(lapply(x$properties, function(i) i$type == "title")))
  if (identical(title_property, integer(0))) return(NA)
  title_node <- x$properties[[title_property]][[x$properties[[title_property]]$type]]
  if (identical(unclass(title_node), list())) return(NA)
  titles <- unlist(lapply(title_node, function(i) i$plain_text), recursive = FALSE)
  if (all.titles) return(titles)
  titles[[1]]
}
