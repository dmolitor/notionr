# Create generics for Pages and Databases
properties <- function(x) {
  UseMethod("properties")
}

# Extract properties of a database/page
properties.notionr_db <- function(x) {
  dplyr::bind_rows(
    lapply(names(x$properties), function(i) {
      id <- ifelse(is.null(x$properties[[i]]$id), NA, x$properties[[i]]$id)
      type <- ifelse(is.null(x$properties[[i]]$type), NA, x$properties[[i]]$type)
      dplyr::tibble("field" = i, "id" = id, "type" = type)
    })
  )
}


# Create generics for Pages and Databases
id <- function(x) {
  UseMethod("id")
}

# Extract IDs of a database/page
id.notionr_db <- function(x) {
  dplyr::tibble("name" = x$title$plain_text, "id" = x$id)
}
