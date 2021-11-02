# Constructor for search_filter class
new_search_filter <- function(value, property) {
  stopifnot(property == "object",
            value %in% c("page", "database"))
  x <- list("value" = value,
            "property" = property)
  class(x) <- "notionr_search_filter"
  x
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

#' Search for databases and pages
#'
#' Search for databases and pages, using filters and sorts to limit which/how
#' pages and databases are returned.
#'
#' @param key Notion access key.
#' @param query A string which limits which pages are returned by comparing the
#'   query to the page title. If `NULL`, no limiting occurs.
#' @param sort A search sort object. If `NULL`, no sorting will occur.
#' @param filter A search filter object. If `NULL` no filtering will occur.
#' @return A list of pages and/or databases.
#' @seealso [search_filter()] and [search_sort()] to see details on how to
#'   correctly construct filter and sort objects.
#' @export
search_workspace <- function(key, query = NULL, sort = NULL, filter = NULL) {
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
  content_ls <- recurse_cursors_post("https://api.notion.com/v1/search",
                                     key,
                                     body,
                                     cont.ls = NULL)
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
  # Assign key attribute
  content_ls <- lapply(content_ls, function(i) {
    `attr<-`(i, "key", key)
  })
  return(content_ls)
}

#' Create a search filter
#'
#' Construct a search filter to filter results returned from the \code{\link{search_workspace}}
#' function. This filter is only meant for use within the \code{\link{search_workspace}}
#' function.
#'
#' @param value Which type of property to filter to. Currently, `"page"` and
#' `"database"` are the only valid options.
#' @param property Which property type to apply the filter to. Currently
#' `"object"` is the only valid option.
#' @return A search filter object.
#' @export
search_filter <- function(value = "page", property = "object") {
  new_search_filter(value = value, property = property)
}

#' Create a search sort
#'
#' Construct a search sort to sort results returned from the \code{\link{search_workspace}}
#' function. This sort is only meant for use within the \code{\link{search_workspace}}
#' function.
#'
#' @param timestamp Which property to sort by. Currently, `"last_edited_time"`
#'   is the only valid option.
#' @param direction Which direction to sort in. Must be either `"descending"` or
#'   `"ascending"`.
#' @return A search sort object.
#' @export
search_sort <- function(timestamp = "last_edited_time", direction = "descending") {
  new_search_sort(timestamp = timestamp, direction = direction)
}
