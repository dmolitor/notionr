# Rich text ---------------------------------------------------------------

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

# Object content method for rich_text class
object_content.notionr_rich_text <- function(x) {
  object_content(x[[x$type]])
}

# Object content method for rich_text_array class
object_content.notionr_rich_text_array <- function(x) {
  unlist(lapply(x, object_content), recursive = FALSE)
}

# Object content method for text class
object_content.notionr_text <- function(x) {
  list("text" = x$content)
}

# Object content method for mention class
object_content.notionr_mention <- function(x) {
  mention_type <- x$type
  list("mention" = x[[mention_type]])
}

# Object content method for equation class
object_content.notionr_equation <- function(x) {
  list("equation" = x$expression)
}

# Function to get the plain text from rich_text object
plain_text <- function(x) {
  stopifnot(inherits(x, "notionr_rich_text"))
  x$plain_text
}

# Function to get all plain text elements from rich_text_array
plain_text_array <- function(x) {
  stopifnot(inherits(x, "notionr_rich_text_array"))
  unlist(lapply(x, plain_text), recursive = FALSE)
}

# Basics ------------------------------------------------------------------

# constructor for basic (number, checkbox, url, email, phone_number, created_time, created_by, last_edited_time, last_edited_by, select) class
new_basic <- function(x) {
  stopifnot(is.vector(x) && is.atomic(x))
  class(x) <- "notionr_basic"
  x
}

# Method for object content for basic class
object_content.notionr_basic <- function(x) {
  unclass(x)
}

# Nested Basics -----------------------------------------------------------

# constructor for nested basics (formula, rollup) classes
new_nested_basic <- function(x) {
  stopifnot(is.list(x))
  class(x) <- "notionr_nested_basic"
  x
}

# Method for object content for basic class
object_content.notionr_nested_basic <- function(x) {
  x <- unclass(x)
  x[[x$type]]
}

# Classify page property objects ------------------------------------------

classify_page_properties <- function(x) {
  stopifnot(inherits(x, "notionr_page"))
  x[["properties"]] <- lapply(
    x[["properties"]],
    classify_page_property
  )
  x
}

# Function to classify single page property
classify_page_property <- function(x) {
  type <- match.arg(
    x$type,
    c("rich_text", 
      "number",
      "select", 
      "multi_select", 
      "date",
      "formula",
      "relation", 
      "rollup",
      "title",
      "people", 
      "files",
      "checkbox",
      "url", 
      "email", 
      "phone_number",
      "created_time", 
      "created_by", 
      "last_edited_time", 
      "last_edited_by")
  )
  if (type == "rich_text") {
    x[[type]] <- new_rich_text_array(x[[type]])
  } else if (type == "title") {
    x[[type]] <- new_rich_text_array(x[[type]])
  } else if (type %in% c("number", 
                         "checkbox",
                         "url",
                         "email", 
                         "phone_number",
                         "created_time",
                         "created_by", 
                         "last_edited_time",
                         "last_edited_by",
                         "select",
                         "multi_select",
                         "date",
                         "relation",
                         "people",
                         "files")) {
    x[[type]] <- new_basic(x[[type]])
  } else {
    x[[type]] <- new_nested_basic(x[[type]])
  }
  x
}
