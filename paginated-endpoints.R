# For testing -------------------------------------------------------------

key <- cached_access_code()

# Functions ---------------------------------------------------------------

list_databases <- function(key = cached_access_code()) {
  stopifnot(is.character(key))
  # Empty content list to grab content from each page
  content_ls <- list()
  recurse_cursors_get(endpoint = "https://api.notion.com/v1/databases", 
                      key = key)
  # Determine which elements are empty
  empty_idx <- unlist(lapply(content_ls, function(i) identical(i$results, list())))
  if (all(empty_idx)) {
    warning("No databases found", call. = FALSE, immediate. = TRUE)
    return(jsonlite::toJSON(content_ls[[1]], auto_unbox = TRUE))
  }
  content_ls <- content_ls[!empty_idx]
  jsonlite::toJSON(content_ls, auto_unbox = TRUE)
}

list_database_ids <- function(key = cached_access_code()) {
  stopifnot(is.character(key))
  db_json <- list_databases(key)
  stopifnot(jsonlite::validate(db_json))
  dbs <- jsonlite::fromJSON(db_json, flatten = TRUE)
  if (identical(dbs$results, list())) return(dplyr::tibble())
  ## Function to extract relevant fields from nested dataframes
  grab_fields <- function(x) {
    stopifnot(all(c("id", "title") %in% names(x)))
    dplyr::bind_cols("id" = x$id, "title" = x$title[[1]]$plain_text)
  }
  ##
  dplyr::bind_rows(lapply(dbs$results, grab_fields))
}

list_users <- function(key = cached_access_code()) {
  stopifnot(is.character(key))
  # Empty content list to grab content from each page
  content_ls <- list()
  recurse_cursors_get(endpoint = "https://api.notion.com/v1/users", 
                      key = key)
  condense_list_users_content(content_ls)
}

# # Helper function for list_databases
# condense_list_databases_content <- function(list.databases.ls) {
#   stopifnot(length(list.databases.ls) > 0)
#   content_json <- jsonlite::fromJSON(
#     jsonlite::toJSON(list.databases.ls, auto_unbox = TRUE),
#     flatten = TRUE
#   )
#   if (identical(content_json$results[[1]], list())) stop("No databases found", call. = FALSE)
#   wide_results <- tidyr::unnest_wider(
#     dplyr::select(content_json, -c(object, next_cursor, has_more)), 
#     col = "results"
#   )
#   cols_to_elongate <- names(wide_results)[unlist(lapply(wide_results, is.list))]
#   cols_to_elongate <- cols_to_elongate[cols_to_elongate != "next_cursor"]
#   all_dat <- unnest_longer_cols(wide_results, cols_to_elongate)
#   janitor::clean_names(all_dat)
# }
# 
# # Helper function that applies unnest_longer to all specified columns of a tibble
# unnest_longer_cols <- function(dat, cols) {
#   stopifnot(is.data.frame(dat), is.vector(cols))
#   if (identical(cols, character(0)) || cols == "") return(dat)
#   names_order <- names(dat)
#   cols <- which(names(dat) %in% cols)
#   dat_to_unnest <- dat[cols]
#   dat_not_to_unnest <- dat[-cols]
#   unnested_dat <- dplyr::bind_cols(
#     lapply(1:ncol(dat_to_unnest), function(i) {
#       tidyr::unnest_longer(dat_to_unnest[, i], col = names(dat_to_unnest)[[i]])
#     })
#   )
#   unnested_dat <- dplyr::mutate(
#     .data = unnested_dat, 
#     dplyr::across(tidyselect::everything(),
#                   ~ replace(.x, .x == "NULL", NA))
#   )
#   dplyr::bind_cols(dat_not_to_unnest, unnested_dat)[names_order]
# }

# Helper function for list_users
condense_list_users_content <- function(list.users.ls) {
  if (!length(list.users.ls) > 0) stop("list_users returned empty content", call. = FALSE)
  result_fxn <- function(i) {
    dplyr::bind_rows(
      lapply(i$results, function(j) suppressWarnings(dplyr::bind_cols(j)))
    )
  }
  dplyr::bind_rows(lapply(list.users.ls, result_fxn))
}

is_null <- function(x) {
  stopifnot(length(x) > 0)
  unlist(lapply(x, is.null))
}

# Helper function for GETting all paginated Notion endpoints
recurse_cursors_get <- function(endpoint, key, cursor = NULL, pos.up.stack = 1) {
  url <- httr::modify_url(url = endpoint, 
                          query = list("start_cursor" = cursor))
  output <- httr::content(
    httr::stop_for_status(
      httr::GET(
        url = url,
        httr::add_headers("Authorization" = paste("Bearer", key),
                          "Notion-Version" = "2021-05-13")
      )
    )
  )
  assign(x = "content_ls", 
         value = append(get("content_ls", 
                            envir = parent.frame(n = pos.up.stack)), 
                        list(output)),
         envir = parent.frame(n = pos.up.stack))
  if (output$has_more) {
    recurse_cursors_get(endpoint, key, output$next_cursor, pos.up.stack + 1)
  }
}

query_database <- function(database.id, key, query.body = NULL) {
  # Empty content list
  content_ls <- list()
  recurse_cursors_pages(database.id, key, query.body)
  return(content_ls)
}

# Helper function for GETting all paginated Notion endpoints
recurse_cursors_pages <- function(database.id, key, query.body = NULL, cursor = NULL, pos.up.stack = 1) {
  if (length(get("content_ls", 
                 envir = parent.frame(n = pos.up.stack))) > 10) return(invisible(NULL))
  url <- paste0("https://api.notion.com/v1/databases/", database.id, "/query")
  query.body <- if (!is.null(query.body)) {
    append(query.body, list("start_cursor" = cursor))
  } else {
    list("start_cursor" = cursor)
  }
  output <- httr::content(
    httr::stop_for_status(
      httr::POST(
        url = url,
        body = query.body,
        httr::add_headers("Authorization" = paste("Bearer", key),
                          "Notion-Version" = "2021-05-13"),
        encode = "json"
      )
    )
  )
  assign(x = "content_ls", 
         value = append(get("content_ls", 
                            envir = parent.frame(n = pos.up.stack)), 
                        list(output)),
         envir = parent.frame(n = pos.up.stack))
  if (output$has_more) {
    recurse_cursors_pages(database.id, key, query.body, output$next_cursor, pos.up.stack + 1)
  }
}

# Function to retrieve a database
retrieve_database <- function(key, database.id) {
  url <- sprintf("https://api.notion.com/v1/databases/%s", database.id)
  db_json <- jsonlite::minify(
    httr::content(
      httr::stop_for_status(
        httr::GET(
          url,
          httr::add_headers("Authorization" = paste("Bearer", key),
                            "Notion-Version" = "2021-05-13")
        )
      ),
      as = "text"
    )
  )
  db_out <- database(db_json)
  return(db_out)
}

# Create class for notion page object
database <- function(x) {
  stopifnot(jsonlite::validate(x))
  x_ls <- jsonlite::fromJSON(x, simplifyDataFrame = FALSE)
  if (
    !all(
      c("object", 
        "id", 
        "created_time",
        "last_edited_time",
        "title",
        "properties") %in% names(x_ls)
    )
  ) {
    stop("Object is not coercible to a database", call. = FALSE)
  }
  # Unnest title field
  x_ls$title <- rich_text(x_ls$title[[1]])
  x_ls$properties <- lapply(x_ls$properties, convert_properties_rich_text)
  # Add database class
  class(x_ls) <- "notion.database"
  return(x_ls)
}

# Create a class for notion page object

rich_text <- function(x) {
  stopifnot(is.list(x))
  if (
    !all(
      c("plain_text", 
        "href", 
        "annotations",
        "type") %in% names(x)
    )
  ) {
    stop("Object is not coercible to class 'rich.text'", call. = FALSE)
  }
  x_type <- x$type
  stopifnot(x_type %in% valid_rich_text_types())
  class(x) <- "rich.text"
  return(x)
}

convert_properties_rich_text <- function(x) {
  if (x$type == "rich_text") {
    out <- tryCatch(
      rich_text(x),
      error = function(e) x
    )
    return(out)
  }
  return(x)
}

database_properties <- function(x) {
  if (!inherits(x, "notion.database")) stop("Object is not a notion database", call. = FALSE)
  return(x$properties)
}

valid_rich_text_types <- function() {
  c("text", "mention", "equation")
}

format.notion.database <- function(x, ..., start.with = "\r") {
  unlist(
    lapply(names(x), function(i) {
      if (!is.list(x[[i]])) {
        return(paste0(start.with, "", i, ": ", x[[i]], "\n"))
      } else {
        c(paste0(start.with, "", i, ":\n"),
          unlist(format.notion.database(x[[i]], start.with = paste0(start.with, "    "))))
      }
    })
  )
}

format.rich.text <- function(x, ...) {
  unlist(lapply(names(x), function(i) {
    if (length(x[[i]]) <= 1) return(paste0("\r", i, ": ", x[[i]], "\n"))
    list(
      paste0("\r", i, ":\n"),
      unlist(
        lapply(names(x[[i]]), function(j) {
          return(paste0("  - ", j, ": ", x[[i]][[j]], "\n"))
        })
      )
    )
  }))
}

print.notion.database <- function(x, ...) {
  cat(format(x))
}

print.rich.text <- function(x, ...) {
  cat(format(x))
}
