#' @method children notionr_page
#' @export
children.notionr_page <- function(x, recursive = TRUE) {
  stopifnot(recursive %in% c(TRUE, FALSE))
  f <- ifelse(recursive,
              retrieve_block_children_recursive,
              retrieve_block_children_nonrecursive)
  key <- attributes(x)$key
  page.id <- x$id
  f(key, page.id)
}

#' @method details notionr_page
#' @export
details.notionr_page <- function(x, ..., start.with = "\r") {
  formatted_page <- format(x)
  cat(formatted_page)
  invisible(x)
}

#' @method format notionr_page
#' @export
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

#' @method id notionr_page
#' @export
id.notionr_page <- function(x) {
  name <- title(x)
  dplyr::tibble("name" = name, "id" = x$id)
}

#' List all Pages
#'
#' Return a list of all page objects. Allows the user to apply a query
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
list_pages <- function(key, query = NULL, sort = NULL) {
  stopifnot(is.character(key))
  search(key,
         query = query,
         sort = sort,
         filter = search_filter())
}

#' Retrieve all page names and IDs
#'
#' Access all page names and IDs in a tidy format. This is particularly
#' useful for getting a quick overview of all pages and selecting a
#' specific ID to access an individual page.
#'
#' @param key Notion access key as a character.
#' @param query A string which limits which pages are returned by comparing the
#'   query to the page title. If `NULL`, no limiting occurs.
#' @param sort A search sort object. If `NULL`, no sorting will occur.
#' @seealso [search()] and [list_pages()]
#' @return A data.frame containing page names and IDs.
#' @export
list_page_ids <- function(key, query = NULL, sort = NULL) {
  dplyr::bind_rows(
    lapply(
      list_pages(key, query, sort),
      id
    )
  )
}

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

#' @method object_content notionr_page
#' @export
object_content.notionr_page <- function(x) {
  lapply(x$properties, function(i) {
    object_content(i[[i$type]])
  })
}

#' @method print notionr_page
#' @export
print.notionr_page <- function(x, ...) {
  els <- c("Title" = ifelse(is.na(title(x)), "", title(x)),
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

#' @method properties notionr_page
#' @export
properties.notionr_page <- function(x) {
  dplyr::bind_rows(
    lapply(names(x$properties), function(i) {
      id <- ifelse(is.null(x$properties[[i]]$id), NA, x$properties[[i]]$id)
      type <- ifelse(is.null(x$properties[[i]]$type), NA, x$properties[[i]]$type)
      dplyr::tibble("field" = i, "id" = id, "type" = type)
    })
  )
}

#' Query a database
#'
#' Query a database and return pages contained in that database. Also allows the
#' user to apply sorting and filtering logic to customize which/how pages are
#' returned. For details on how to structure these filters and sorts, see
#' \code{\link{query_body}}, \code{\link{property_filter}}, and
#' \code{\link{property_sort}}.
#'
#' @param database.id The database ID as a character.
#' @param key Notion access key.
#' @param query.body The query body which contains potential filters and sorts.
#' @return A list of pages.
#' @export
query_database <- function(database.id, key, query.body = NULL) {
  url <- paste0("https://api.notion.com/v1/databases/", database.id, "/query")
  # Empty content list
  content_ls <- recurse_cursors_post(url, key, query.body, cont.ls = NULL)
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

#' Retrieve an individual page
#'
#' Access a specific page using it's unique identifier.
#'
#' @param key Notion access key as a string.
#' @param page.id Unique database identifier as a string.
#' @return A page object.
#' @export
retrieve_page <- function(key, page.id) {
  url <- paste0("https://api.notion.com/v1/pages/", page.id)
  page <- httr::content(
    httr::stop_for_status(
      httr::GET(
        url,
        httr::add_headers("Authorization" = paste("Bearer", key),
                          "Notion-Version" = "2021-08-16")
      )
    )
  )
  page_out <- new_page(page)
  `attr<-`(page_out, "key", key)
}

#' @method title notionr_page
#' @export
title.notionr_page <- function(x, all.titles = FALSE) {
  title_property <- which(unlist(lapply(x$properties, function(i) i$type == "title")))
  if (identical(title_property, integer(0))) return(NA)
  title_node <- x$properties[[title_property]][[x$properties[[title_property]]$type]]
  if (identical(unclass(title_node), list())) return(NA)
  titles <- unlist(lapply(title_node, function(i) i$plain_text), recursive = FALSE)
  if (all.titles) return(titles)
  titles[[1]]
}
