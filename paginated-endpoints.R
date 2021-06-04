access_code <- cached_access_code()

list_users <- function(key = cached_access_code()) {
  # Empty content list to content from each page
  content_ls <- list()
  recurse_cursors(endpoint = "https://api.notion.com/v1/users", 
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

# Helper function for all paginated Notion endpoints
recurse_cursors <- function(endpoint, key, cursor = NULL) {
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
         value = append(get("content_ls", envir = parent.frame()), list(output)),
         envir = parent.frame())
  if (output$has_more) {
    recurse_cursors(endpoint, key, output$next_cursor)
  }
}
