# Constructor for User class
new_user <- function(x) {
  stopifnot(is.list(x))
  if (!all(c("object", "id", "type", "name", "avatar_url") %in% names(x))) {
    stop("Missing essential user fields", call. = FALSE)
  }
  stopifnot(x$type %in% c("person", "bot"))
  class(x) <- "notionr_user"
  x
}

#' List all workspace users
#'
#' This function retrieves user information for all users with access to a
#' workspace and returns it in a tidy format.
#'
#' @param key Notion access key.
#' @return A data.frame with user information.
#' @export
list_users <- function(key) {
  user_ls <- users(key)
  dplyr::bind_rows(
    lapply(user_ls, object_content)
  )
}

#' @method object_content notionr_user
#' @export
object_content.notionr_user <- function(x) {
  x <- unclass(x)
  if (x$type == "person") x[["email"]] <- x$person$email
  x[[x$type]] <- NULL
  dplyr::bind_cols(x)
}

#' Get users
#'
#' This endpoint retrieves all users with authorized access to a workspace
#'
#' @param key Notion access key
#' @return A list of user objects
#' @export
users <- function(key) {
  stopifnot(is.character(key))
  # Empty content list to grab content from each page
  content_ls <- recurse_cursors_get(endpoint = "https://api.notion.com/v1/users",
                                    key = key,
                                    cont.ls = list())
  content_ls <- unlist(
    lapply(content_ls, function(i) {
      i$results
    }),
    recursive = FALSE
  )
  lapply(content_ls, new_user)
}

#' Retrieve a user object
#'
#' This function will retrieve a particular user, referenced by ID. This ID can
#' most easily be accessed using the \code{\link{list_users}} function.
#'
#' @param key Notion access key.
#' @param user.id Unique user ID as a string.
#' @return A user object.
#' @export
retrieve_user <- function(key, user.id) {
  url <- paste0("https://api.notion.com/v1/users/", user.id)
  cont <- httr::content(
    httr::stop_for_status(
      httr::GET(url = url,
                httr::add_headers("Authorization" = paste("Bearer", key),
                                  "Notion-Version" = "2021-05-13"))
    )
  )
  new_user(cont)
}
