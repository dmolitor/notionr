# Constructor for equation class
new_equation <- function(x) {
  stopifnot(names(x) == "equation", length(x) == 1)
  class(x) <- "notionr_equation"
  x
}

# Constructor for mention class
new_mention <- function(x) {
  stopifnot(is.list(x), length(x) == 2, "type" %in% names(x))
  class(x) <- "notionr_mention"
  x
}

# constructor for rich text class
new_rich_text <- function(x) {
  stopifnot(is.list(x))
  if (!all(c("plain_text", "href", "annotations", "type") %in% names(x))) {
    stop("Missing essential rich text properties", call. = FALSE)
  }
  type <- match.arg(x$type, c("text", "mention", "equation"))
  if (type == "text") {
    x[["text"]] <- new_text(x[["text"]])
  } else if (type == "mention") {
    x[["mention"]] <- new_mention(x[["mention"]])
  } else {
    x[["equation"]] <- new_equation(x[["equation"]])
  }
  class(x) <- "notionr_rich_text"
  x
}

# constructor for rich text array
new_rich_text_array <- function(x) {
  stopifnot(is.list(x))
  x <- lapply(x, new_rich_text)
  class(x) <- "notionr_rich_text_array"
  x
}

# Constructor for text class
new_text <- function(x) {
  stopifnot(is.list(x))
  if (!all(c("content", "link") %in% names(x))) {
    stop("Missing essential text properties", call. = FALSE)
  }
  class(x) <- "notionr_text"
  x
}

#' @method object_content notionr_rich_text
#' @export
object_content.notionr_rich_text <- function(x) {
  object_content(x[[x$type]])
}

#' @method object_content notionr_rich_text_array
#' @export
object_content.notionr_rich_text_array <- function(x) {
  unlist(lapply(x, object_content), recursive = FALSE)
}

#' @method object_content notionr_text
#' @export
object_content.notionr_text <- function(x) {
  list("text" = x$content)
}

#' @method object_content notionr_mention
#' @export
object_content.notionr_mention <- function(x) {
  mention_type <- x$type
  list("mention" = x[[mention_type]])
}

#' @method object_content notionr_equation
#' @export
object_content.notionr_equation <- function(x) {
  list("equation" = x$expression)
}

#' Extract plain text from rich text object
#'
#' A rich text object has both a content property and a plain-text property. To
#' access the content property, see \code{\link{object_content}}, and to access
#' the plain text, use this function.
#'
#' @param x A rich text object
#' @return The plain text as a string.
#' @export
plain_text <- function(x) {
  stopifnot(inherits(x, "notionr_rich_text"))
  x$plain_text
}

#' Extract plain text from a rich text array
#'
#' This function extracts all plain text elements from a rich text array object.
#'
#' @param x A rich text array.
#' @return A list of plain text elements.
#' @seealso [plain_text()]
#' @export
plain_text_array <- function(x) {
  stopifnot(inherits(x, "notionr_rich_text_array"))
  unlist(lapply(x, plain_text), recursive = FALSE)
}
