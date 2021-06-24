list_users <- function(key = cached_access_code()) {
  stopifnot(is.character(key))
  # Empty content list to grab content from each page
  content_ls <- list()
  recurse_cursors_get(endpoint = "https://api.notion.com/v1/users", 
                      key = key)
  condense_list_users_content(content_ls)
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

# Helper function for POSTing all paginated Notion endpoints
recurse_cursors_post <- function(url, key, query.body = NULL, cursor = NULL, pos.up.stack = 1) {
  body <- if (!is.null(query.body)) {
    append(query.body, list("start_cursor" = cursor))
  } else {
    list("start_cursor" = cursor)
  }
  output <- httr::content(
    httr::stop_for_status(
      httr::POST(
        url = url,
        body = body,
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
    recurse_cursors_post(url, key, query.body, output$next_cursor, pos.up.stack + 1)
  }
}
