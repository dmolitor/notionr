# Classify every page property
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

# Constructor for basic (number, checkbox, url, email, phone_number, created_time, created_by, last_edited_time, last_edited_by, select) class
new_basic <- function(x) {
  stopifnot((is.vector(x) && is.atomic(x)) || is.list(x))
  class(x) <- "notionr_basic"
  x
}

# Method for object content for basic class
object_content.notionr_basic <- function(x) {
  unclass(x)
}

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