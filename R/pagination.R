# Helper function for GETting all paginated Notion endpoints
recurse_cursors_get <- function(endpoint, key, cursor = NULL, cont.ls) {
  url <- httr::modify_url(url = endpoint,
                          query = list("start_cursor" = cursor))
  output <- httr::content(
    httr::stop_for_status(
      httr::GET(
        url = url,
        httr::add_headers("Authorization" = paste("Bearer", key),
                          "Notion-Version" = "2021-08-16")
      )
    )
  )
  cont.ls <- append(cont.ls, list(output))
  if (output$has_more) {
    return(recurse_cursors_get(endpoint, key, output$next_cursor, cont.ls))
  } else {
    return(return(cont.ls))
  }
}

# Helper function for POSTing all paginated Notion endpoints
recurse_cursors_post <- function(url, key, query.body = NULL, cursor = NULL, cont.ls) {
  body <- if (!is.null(query.body)) {
    append(query.body, list("start_cursor" = cursor, "page_size" = 100))
  } else {
    list("start_cursor" = cursor, "page_size" = 100)
  }
  output <- httr::content(
    httr::stop_for_status(
      httr::POST(
        url = url,
        body = body,
        httr::add_headers("Authorization" = paste("Bearer", key),
                          "Notion-Version" = "2021-08-16"),
        encode = "json"
      )
    )
  )
  cont.ls <- append(cont.ls, list(output))
  if (output$has_more) {
    return(recurse_cursors_post(url, key, query.body, output$next_cursor, cont.ls))
  } else {
    return(cont.ls)
  }
}
