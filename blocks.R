# Constructor for notion page class
new_block <- function(x) {
  stopifnot(is.list(x))
  if (
    !all(
      c("object", 
        "id",
        "type",
        "created_time",
        "last_edited_time",
        "has_children") %in% names(x)
    )
  ) {
    stop("Object is missing essential page fields", call. = FALSE)
  }
  # Add block class
  class(x) <- "notionr_block"
  return(x)
}

# Function to retrieve block children
retrieve_block_children <- function(key, block.id) {
  stopifnot(is.character(key), is.character(block.id))
  url <- sprintf("https://api.notion.com/v1/blocks/%s/children", block.id)
  # Empty content list to grab content from each page
  content_ls <- list()
  recurse_cursors_get(endpoint = url, 
                      key = key)
  unlist(
    lapply(content_ls, function(i) {
      lapply(i$results, new_block)
    }),
    recursive = FALSE
  )
}
