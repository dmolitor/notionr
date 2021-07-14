# Rich text ---------------------------------------------------------------

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

# Constructor for mention class
new_mention <- function(x) {
  stopifnot(is.list(x), length(x) == 2, "type" %in% names(x))
  class(x) <- "notionr_mention"
  x
}

# Constructor for equation class
new_equation <- function(x) {
  stopifnot(names(x) == "equation", length(x) == 1)
  class(x) <- "notionr_equation"
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

# Title -------------------------------------------------------------------

# Constructor for title class
new_title <- function(x) {
  if (is.null(x)) {
    x <- list()
    class(x) <- "notionr_title"
    return(x)
  }
  class(x) <- "notionr_title"
  x
}

# Method for object content for title class
object_content.notionr_title <- function(x) {
  x$plain_text
}

# Basics ------------------------------------------------------------------

# constructor for basic (number, checkbox, url, email, phone_number, created_time, created_by, last_edited_time, last_edited_by) class
new_basic <- function(x) {
  stopifnot(is.vector(x) && is.atomic(x))
  class(x) <- "notionr_basic"
  x
}

# Method for object content for basic class
object_content.notionr_basic <- function(x) {
  unclass(x)
}

# Select ------------------------------------------------------------------

# Constructor for select class
new_select <- function(x) {
  stopifnot(is.list(x), names(x) %in% c("id", "name", "color"))
  class(x) <- "notionr_select"
  x
}

# Method for object content for select class
object_content.notionr_select <- function(x) {
  unclass(x)
}

# Multi-select ------------------------------------------------------------

# Constructor for multiselect class
new_multi_select <- function(x) {
  stopifnot(is.list(x))
  class(x) <- "notionr_multi_select"
  x
}

# Method for object content for multi select class
object_content.notionr_multi_select <- function(x) {
  unclass(x)
}

# Date --------------------------------------------------------------------

# Constructor for date class
new_date <- function(x) {
  stopifnot(is.list(x), names(x) %in% c("start", "end"))
  class(x) <- "notionr_date"
  x
}

# Method for object content for date class
object_content.notionr_date <- function(x) {
  unclass(x)
}

# Formula -----------------------------------------------------------------

# Constructor for formula class
new_formula <- function(x) {
  stopifnot(is.list(x), x$type %in% c("string", "number", "boolean", "date"))
  if (x$type == "date") x[["date"]] <- new_date(x[["date"]])
  class(x) <- "notionr_formula"
  x
}

# Method for object content for formula class
object_content.notionr_formula <- function(x) {
  type <- x$type
  if (type == "date") return(object_content(x[[type]]))
  x[[type]]
}

# Relation ----------------------------------------------------------------

# Constructor for relation class
new_relation <- function(x) {
  stopifnot(is.list(x))
  class(x) <- "notionr_relation"
  x
}

# Method for object content for relation class
object_content.notionr_relation <- function(x) {
  unclass(x)
}

# Rollup ------------------------------------------------------------------

# Constructor for rollup class
new_rollup <- function(x) {
  stopifnot(is.list(x), x$type %in% c("number", "date", "array"))
  if (x$type == "date") x[["date"]] <- new_date(x[["date"]])
  class(x) <- "notionr_rollup"
  x
}

# Method for object content for rollup class
object_content.notionr_rollup <- function(x) {
  type <- x$type
  if (type == "date") return(object_content(x[[type]]))
  x[[type]]
}

# People ------------------------------------------------------------------

# constructor for people class
new_people <- function(x) {
  stopifnot(is.list(x))
  class(x) <- "notionr_people"
  x
}

# Method for object content for people class
object_content.notionr_people <- function(x) {
  unclass(x)
}

# Files -------------------------------------------------------------------

# constructor for files class
new_files <- function(x) {
  stopifnot(is.list(x))
  class(x) <- "notionr_files"
  x
}

# Method for object content for files class
object_content.notionr_files <- function(x) {
  unclass(x)
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
                         "last_edited_by")) {
    x[[type]] <- new_basic(x[[type]])
  } else {
    f <- paste0("new_", type)
    x[[type]] <- eval(call(f, x[[type]]))
  }
  x
}
