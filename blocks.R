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
  if (!x$type %in% c("title", "unsupported")) {
    x[[x$type]]$text <- new_rich_text_array(x[[x$type]]$text)
  }
  return(x)
}

# Block children
#
# Retrieves block children non-recursively. Aka only the first layer of children
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

# Block children
#
# Retrieves block children recursively. All block children.
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
  # If results are empty, return
  if (identical(out, list())) return(out)
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

#' Print block information
#' 
#' Prints high-level information about a block object
#' 
#' @param x An object of class `notionr_block`.
#' @param ... Used to swallow additional arguments.
#' @return Invisibly returns itself. Primarily called for side effects.
#' @export
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

#' Print detailed block information
#' 
#' Prints low-level information about a block object
#' 
#' @param x An object of class `notionr_block`.
#' @param ... Used to swallow additional arguments.
#' @param start.with Which character to preface each line in 
#'   the tree structure with.
#' @return Invisibly returns itself. Primarily called for side effects.
#' @export
details.notionr_block <- function(x, ..., start.with = "\r") {
  formatted_block <- format(x)
  cat(formatted_block)
  invisible(x)
}

#' Retrieve block children
#' 
#' Retrieves first level of block children or recursively retrieves all block
#' children.
#' 
#' @param x An object of class `notionr_block`.
#' @param recursive A logical scalar. Should it return the first level
#'   of block children or recursively return all block children.
#' @return A list of `notionr_block` objects.
#' @export
children.notionr_block <- function(x, recursive = TRUE) {
  stopifnot(inherits(x, "notionr_block"), recursive %in% c(TRUE, FALSE))
  f <- ifelse(recursive, 
              retrieve_block_children_recursive,
              retrieve_block_children_nonrecursive)
  key <- attributes(x)$key
  block.id <- x$id
  f(key, block.id)
}

# Does a block have children?
has_children <- function(x) {
  if (!(inherits(x, "notionr_page") || inherits(x, "notionr_block"))) {
    stop("Object must be of class `notionr_page` or `notionr_block`", call. = FALSE)
  }
  children_query <- children(x, recursive = FALSE)
  !identical(children_query, list())
}

# Object content method for block
object_content.notionr_block <- function(x) {
  x <- unclass(x)
  if (!(x$type %in% c("title", "unsupported"))) {
    x[[x$type]]$text <- object_content(x[[x$type]]$text)
  }
  x[[x$type]]
}