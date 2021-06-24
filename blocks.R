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

# Function to retrieve first layer block children
retrieve_block_children_nonrecursive <- function(key, block.id) {
  stopifnot(is.character(key), is.character(block.id))
  url <- sprintf("https://api.notion.com/v1/blocks/%s/children", block.id)
  # Empty content list to grab content from each page
  content_ls <- list()
  recurse_cursors_get(endpoint = url, 
                      key = key)
  c(
    "child" = unlist(
      lapply(content_ls, function(i) {
        lapply(i$results, function(j) {
          blk <- new_block(j)
          `attr<-`(blk, "key", key)
        })
      }),
      recursive = FALSE
    )
  )
}

# Function to retrieve all block children recursively
retrieve_block_children_recursive <- function(key, block.id) {
  stopifnot(is.character(key), is.character(block.id))
  url <- sprintf("https://api.notion.com/v1/blocks/%s/children", block.id)
  # Empty content list to grab content from each page
  content_ls <- list()
  recurse_cursors_get(endpoint = url, 
                      key = key)
  out <- c(
    "child" = unlist(
      lapply(content_ls, function(i) {
        lapply(i$results, function(j) {
          blk <- new_block(j)
          `attr<-`(blk, "key", key)
        })
      }),
      recursive = FALSE
    )
  )
  which_have_children <- unlist(lapply(out, function(i) i$has_children))
  if (all(!which_have_children)) {
    return(out)
  } else {
    out[which_have_children] <- lapply(
      out[which_have_children],
      function(i) {
        append(i, list("children" = retrieve_block_children_recursive(key, i$id)))
      }
    )
  }
  return(out)
}

# Method for formatting page information
format.notionr_block <- function(x, ..., start.with = "\r") {
  unlist(
    lapply(names(x), function(i) {
      if (!is.list(x[[i]])) {
        return(paste0(start.with, "", i, ": ", x[[i]], "\n"))
      } else {
        c(paste0(start.with, "", i, ":\n"),
          unlist(format.notionr_block(x[[i]], start.with = paste0(start.with, "    "))))
      }
    })
  )
}

# Method for printing database
print.notionr_block <- function(x, ...) {
  els <- c("Id" = x$id,
           "Type" = x$type,
           "Created date" = as.character(as.Date(x$created_time)),
           "Last edited date" = as.character(as.Date(x$last_edited_time)),
           "Has children" = x$has_children)
  cat(
    paste0(names(els), 
           ": ", 
           els,
           collapse = "\n\r")
  )
  invisible(x)
}

# Method for summarizing page
summary.notionr_block <- function(x, ..., start.with = "\r") {
  formatted_page <- format(x)
  cat(formatted_page)
}

# Block method for 'children'
children.notionr_block <- function(x, recursive = TRUE) {
  stopifnot(recursive %in% c(TRUE, FALSE))
  f <- ifelse(recursive, 
              retrieve_block_children_recursive,
              retrieve_block_children_nonrecursive)
  key <- attributes(x)$key
  block.id <- x$id
  f(key, block.id)
}
