list_databases <- function(key = cached_access_code()) {
  # Empty content list to grab content from each page
  content_ls <- list()
  recurse_cursors(endpoint = "https://api.notion.com/v1/databases", 
                  key = key)
  condense_list_databases_content(content_ls)
}

list_users <- function(key = cached_access_code()) {
  # Empty content list to grab content from each page
  content_ls <- list()
  recurse_cursors(endpoint = "https://api.notion.com/v1/users", 
                  key = key)
  condense_list_users_content(content_ls)
}

# Helper function for list_databases
condense_list_databases_content <- function(list.databases.ls) {
  stopifnot(length(list.databases.ls) > 0)
  content_json <- jsonlite::fromJSON(
    jsonlite::toJSON(list.databases.ls, auto_unbox = TRUE),
    flatten = TRUE
  )
  if (identical(content_json$results[[1]], list())) stop("No databases found", call. = FALSE)
  wide_results <- tidyr::unnest_wider(
    dplyr::select(content_json, -c(object, next_cursor, has_more)), 
    col = "results"
  )
  cols_to_elongate <- names(wide_results)[unlist(lapply(wide_results, is.list))]
  cols_to_elongate <- cols_to_elongate[cols_to_elongate != "next_cursor"]
  all_dat <- unnest_longer_cols(wide_results, cols_to_elongate)
  janitor::clean_names(all_dat)
}

# Helper function that applies unnest_longer to all specified columns of a tibble
unnest_longer_cols <- function(dat, cols) {
  stopifnot(is.data.frame(dat), is.vector(cols))
  if (identical(cols, character(0)) || cols == "") return(dat)
  names_order <- names(dat)
  cols <- which(names(dat) %in% cols)
  dat_to_unnest <- dat[cols]
  dat_not_to_unnest <- dat[-cols]
  unnested_dat <- dplyr::bind_cols(
    lapply(1:ncol(dat_to_unnest), function(i) {
      tidyr::unnest_longer(dat_to_unnest[, i], col = names(dat_to_unnest)[[i]])
    })
  )
  unnested_dat <- dplyr::mutate(
    .data = unnested_dat, 
    dplyr::across(tidyselect::everything(),
                  ~ replace(.x, .x == "NULL", NA))
  )
  dplyr::bind_cols(dat_not_to_unnest, unnested_dat)[names_order]
}

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

# Helper function for all paginated Notion endpoints
recurse_cursors <- function(endpoint, key, cursor = NULL, pos.up.stack = 1) {
  url <- httr::modify_url(url = endpoint, 
                          query = list("start_cursor" = cursor,
                                       "page_size" = 1))
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
    recurse_cursors(endpoint, key, output$next_cursor, pos.up.stack + 1)
  }
}

# Function to query databases
query_database <- function(database.id, )