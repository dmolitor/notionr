# Constructor for User class
new_user <- function(x) {
  
}

# Acces list users endpoint
list_users <- function(key) {
  stopifnot(is.character(key))
  # Empty content list to grab content from each page
  content_ls <- list()
  recurse_cursors_get(endpoint = "https://api.notion.com/v1/users", 
                      key = key)
  content_ls <- unlist(content_ls, recursive = FALSE)
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