# Hit the Search endpoint
search <- function(key, query = NULL, sort = NULL, filter = NULL) {
  stopifnot(nzchar(query) || is.null(query),
            inherits(sort, "notionr_search_sort") || is.null(sort),
            inherits(filter, "notionr_search_filter") || is.null(filter))
  # Construct POST body
  body <- list()
  body <- if (!is.null(query)) {
    append(body, list("query" = query))
  }
  body <- if (!is.null(sort)) {
    append(body, list("sort" = unclass(sort)))
  }
  body <- if (!is.null(filter)) {
    append(body, list("filter" = unclass(filter)))
  }
  if (identical(body, list())) body <- NULL
  # Initialize empty list for pagination
  content_ls <- list()
  recurse_cursors_post("https://api.notion.com/v1/search", key, body)
  content_ls <- unlist(
    lapply(content_ls, function(i) {
      i$results
    }),
    recursive = FALSE
  )
  objects <- unlist(lapply(content_ls, function(i) i$object))
  which_are_databases <- objects == "database"
  which_are_pages <- objects == "page"
  content_ls[which_are_databases] <- lapply(
    content_ls[which_are_databases],
    new_database
  )
  content_ls[which_are_pages] <- lapply(
    content_ls[which_are_pages],
    new_page
  )
  return(content_ls)
}

# Constructor for search_sort class
new_search_sort <- function(timestamp, direction) {
  stopifnot(timestamp == "last_edited_time",
            direction %in% c("ascending", "descending"))
  x <- list("direction" = direction,
            "timestamp" = timestamp)
  class(x) <- "notionr_search_sort"
  x
}

# Create a search sort
search_sort <- function(timestamp = "last_edited_time", direction = "descending") {
  new_search_sort(timestamp = timestamp, direction = direction)
}

# Constructor for search_filter class
new_search_filter <- function(value, property) {
  stopifnot(property == "object",
            value %in% c("page", "database"))
  x <- list("value" = value,
            "property" = property)
  class(x) <- "notionr_search_filter"
  x
}

# Create a search filter
search_filter <- function(value = "page", property = "object") {
  new_search_filter(value = value, property = property)
}
