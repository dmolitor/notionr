#' @export
details.notionr_db <- function(x, ..., start.with = "\r") {
  formatted_db <- format(x)
  cat(formatted_db)
  invisible(x)
}

# Format database information
#
# Formats database information in a format that is conducive for pretty printing
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

# Constructor for class `notionr_db`
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
  x$title <- new_rich_text_array(x$title)
  # Add database class
  class(x) <- "notionr_db"
  return(x)
}

#' @export
print.notionr_db <- function(x, ...) {
  els <- c("Title" = title(x),
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

#' @export
id.notionr_db <- function(x) {
  name <- title(x)
  dplyr::tibble("name" = name, "id" = x$id)
}

#' List all Databases
#' 
#' Return a list of all database objects. Allows the user to apply a query
#' filter or sort direction, as seen in the \code{\link{search}} function, to
#' alter how/which objects are returned.
#' 
#' @param key Notion access key as a character.
#' @param query A string which limits which pages are returned by comparing the 
#'   query to the page title. If `NULL`, no limiting occurs.
#' @param sort A search sort object. If `NULL`, no sorting will occur.
#' @return A list of database objects.
#' @seealso [search()] for examples of sorts.
#' @export
list_databases <- function(key, query = NULL, sort = NULL) {
  stopifnot(is.character(key))
  search(key, 
         query = query, 
         sort = sort, 
         filter = search_filter(value = "database"))
}

#' Retrieve all database names and IDs
#' 
#' Access all database names and IDs in a tidy format. This is particularly
#' useful for getting a quick overview of all databases and selecting a
#' specific ID to access an individual database.
#' 
#' @param key Notion access key as a character.
#' @param query A string which limits which pages are returned by comparing the 
#'   query to the page title. If `NULL`, no limiting occurs.
#' @param sort A search sort object. If `NULL`, no sorting will occur.
#' @seealso [search()] and [list_databases()]
#' @return A data.frame containing database names and IDs.
#' @export
list_database_ids <- function(key, query = NULL, sort = NULL) {
  dplyr::bind_rows(
    lapply(
      list_databases(key, query, sort),
      id
    )
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

#' Retrieve an individual database
#' 
#' Access a specific database using it's unique identifier.
#' 
#' @param key Notion access key as a string.
#' @param database.id Unique database identifier as a string.
#' @return A database object.
#' @export
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

# Title method for Databases
title.notionr_db <- function(x, all.titles = FALSE) {
  if (identical(unclass(x$title), list())) return(NA)
  titles <- plain_text_array(x$title)
  if (all.titles) return(titles)
  titles[[1]]
}
