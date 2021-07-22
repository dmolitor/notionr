# Compound and function
and <- function(x, y) {
  stopifnot(is.list(x), is.list(y))
  list("and" = list(x, y))
}

#' Compound database filter
#'
#' The `compound_filter` function combines two filters into a compound one. These
#' filters can either be compound filters themselves, or single-level filters as
#' described in \code{\link{property_filter}}. This filter is only meant for use
#' within the \code{\link{query_database}} function.
#'
#' Compound filters can be nested up to 2 levels deep. Currently there is no
#' checking for this, so it's on the user to adhere to it.
#'
#' @param x A property filter; either compound or single-level.
#' @param y A property filter; either compound or single-level.
#' @param type A string indicating what type of compound filter. Must be either
#'   `"and"` or `"or"`.
#' @examples
#' compound_filter(
#'   x = property_filter(
#'     property = "Landmark",
#'     type = "text",
#'     body = contains ~ Bridge
#'   ),
#'   y = property_filter(
#'     property = "Bodies of Water",
#'     type = "checkbox",
#'     body = equals ~ TRUE
#'   ),
#'   type = "and"
#' )
#' @return A structured compound filter.
#' @export
compound_filter <- function(x, y, type = "and") {
  if (!type %in% c("and", "or")) stop("Only supports type 'and' or 'or'", call. = FALSE)
  if (type == "and") return(and(x, y))
  or(x, y)
}

#' Compound database sort
#'
#' The `compound_sort` function combines a list of sorts into a compound one.
#' Each element in this list should be an output of \code{\link{property_sort}}.
#' This function is only meant for use
#' within the \code{\link{query_database}} function.
#'
#' @param ... A list of property sorts.
#' @examples
#' compound_sort(
#'   property_sort(
#'     property = "Ingredients",
#'     timestamp = "last_edited_time",
#'     direction = "descending"
#'   ),
#'   property_sort(
#'     property = "Travel Destinations",
#'     timestamp = "created_time",
#'     direction = "ascending"
#'   )
#' )
#' @return An object of class `notionr_compund_sort`.
#' @export
compound_sort <- function(...) {
  x <- list(...)
  if (!all(unlist(lapply(x, is.list)))) {
    stop("Each object must be a list returned by the 'property_sort' function")
  }
  class(x) <- c("list", "notionr_compound_sort")
  return(x)
}

#' @method format notionr_filter
#' @export
format.notionr_filter <- function(x, encode.as.character = TRUE) {
  x <- unclass(x)
  val <- if (encode.as.character) {
    encode_character(x[[1]])
  } else {
    x[[1]]
  }
  attrs <- attributes(x)
  property <- attrs$property
  type <- attrs$type
  expr <- paste0("list(", type, "=", "list(", property, "=", val, "))")
  eval(str2expression(expr))
}

#' @method format notionr_sort
#' @export
format.notionr_sort <- function(x) {
  x <- unclass(x)
  val <- encode_character(x[[1]])
  attrs <- attributes(x)
  timestamp <- encode_character(attrs$timestamp)
  direction <- encode_character(attrs$direction)
  expr <- paste0("list(property=",
                 val,
                 ",timestamp=",
                 timestamp,
                 ",direction=",
                 direction,
                 ")")
  eval(str2expression(expr))
}

# Define constructor for text filter class
new_filter <- function(x = list(), type = "text", property = "equals") {
  stopifnot(is.list(x))
  type <- match.arg(
    type,
    c("text",
      "number",
      "checkbox",
      "select",
      "multi_select",
      "date",
      "people",
      "files",
      "relation",
      "formula")
  )

  if (type == "text") stopifnot(property %in% valid_text_properties())
  if (type == "number") stopifnot(property %in% valid_number_properties())
  if (type == "checkbox") stopifnot(property %in% valid_checkbox_properties())
  if (type == "select") stopifnot(property %in% valid_select_properties())
  if (type == "multi_select") stopifnot(property %in% valid_multiselect_properties())
  if (type == "date") stopifnot(property %in% valid_date_properties())
  if (type == "people") stopifnot(property %in% valid_people_properties())
  if (type == "files") stopifnot(property %in% valid_files_properties())
  if (type == "relation") stopifnot(property %in% valid_relation_properties())
  if (type == "formula") stopifnot(property %in% valid_formula_properties())

  structure(x,
            class = "notionr_filter",
            type = type,
            property = property)
}

# Create simple sort class
new_sort <- function(x = list(), timestamp = "last_edited_time", direction = "descending") {
  stopifnot(is.list(x))
  if (!timestamp %in% c("last_edited_time", "created_time")) {
    stop("timestamp must be 'last_edited_time' or 'created_time'", call. = FALSE)
  }
  if (!direction %in% c("ascending", "descending")) {
    stop("direction must be 'ascending' or 'descending'", call. = FALSE)
  }

  structure(x,
            class = "notionr_sort",
            timestamp = timestamp,
            direction = direction)
}

# Compound or function
or <- function(x, y) {
  stopifnot(is.list(x), is.list(y))
  list("or" = list(x, y))
}

#' Filter condition for database query
#'
#' The `property_filter()` function applies a filter condition within a database
#' query to limit which pages are returned.
#'
#' The `property_filter()` applies to a particular database property, by name or
#' id. It applies a user-supplied condition to this property to limit which
#' pages are returned. For a full set of valid properties and property types,
#' see the \href{https://developers.notion.com/reference/post-database-query#post-database-query-filter}{documentation here}.
#' This filter is only meant for use within the \code{\link{query_database}}
#' function.
#'
#' @param property A character string; the name of the property to apply the
#'   filter to.
#' @param type A character string; the property type as identified by Notion.
#' @param body A two-sided formula. The left hand side specifies the filter
#'   condition property - valid condition properties depend on the filter type -
#'   and the right had side specifies the filtering value.
#' @examples
#' property_filter(
#'   property = "Landmark",
#'   type = "rich_text",
#'   body = contains ~ "Bridge"
#' )
#' @return A list; a formatted property filter.
#' @export
property_filter <- function(property, type, body) {
  type <- reverse_code_types(type)
  body <- parse_formula(body)
  encode.as.character <- is.character(body$rhs)
  body <- replace_null_empty_json(body)
  # Create a new filter object
  fltr <- new_filter(x = list(body$rhs), type = type, property = body$lhs)
  # format filter
  fltr_formatted <- format(fltr, encode.as.character = encode.as.character)
  # Add property field
  fltr_formatted <- append(fltr_formatted,
                           list("property" = property),
                           after = 0)
  return(fltr_formatted)
}

#' Sort condition for database query
#'
#' The `property_sort()` function applies a sort condition within a database
#' query to sort how pages are returned.
#'
#' The `property_sort()` function applies to a particular database property,
#' by name or id. It applies a user-supplied condition to this property to sort
#' pages are returned. This sort is only meant for use within the
#' \code{\link{query_database}} function.
#'
#' @param property A character string; the name of the property to apply the
#'   sort to.
#' @param timestamp A character string; must be one of `last_edited_time` or
#'   `created_time`.
#' @param direction A character string; must be one of `descending` or `ascending`.
#' @examples
#' property_sort(
#'   property = "Ingredients",
#'   timestamp = "last_edited_time",
#'   direction = "descending"
#' )
#' @return A list; a formatted property sort.
#' @export
property_sort <- function(property, timestamp = "last_edited_time", direction = "descending") {
  if (length(unlist(property)) > 1) {
    stop("To combine many sorts, please use the 'compound_sort' function")
  }
  srt <- new_sort(list(property), timestamp, direction)
  format(srt)
}

#' Construct body for database query
#'
#' This function constructs the query body that is used to filter/sort the
#' results of the database query.
#'
#' @param filter A property filter object.
#' @param sorts A property sort or compound sort object.
#' @seealso [property_filter()] and [property_sort()] to see how to properly
#'   create property filters and sorts.
#' @return The formatted query body.
#' @export
query_body <- function(filter = NULL, sorts = NULL) {
  body_ls <- list()
  if (!is.null(filter)) {
    stopifnot(is.list(filter))
    body_ls <- append(body_ls, list("filter" = filter))
  }
  if (!is.null(sorts)) {
    stopifnot(is.list(sorts))
    if (!"notionr_compound_sort" %in% class(sorts)) sorts <- list(sorts)
    body_ls <- append(body_ls, list("sorts" = sorts))
  }
  if (identical(body_ls, list())) body_ls <- NULL
  return(body_ls)
}

# Reverse code property types
reverse_code_types <- function(type) {
  real_type <- if (type %in% c("rich_text", "url", "email", "phone")) {
    "text"
  } else if (type %in% c("date", "created_time", "last_edited_time")) {
    "date"
  } else if (type %in% c("text",
                         "date",
                         "people",
                         "files",
                         "relation",
                         "formula",
                         "number",
                         "checkbox",
                         "select",
                         "multi_select")) {
    type
  } else {
    stop("Invalid entry for 'type'")
  }
  return(real_type)
}

# Valid checkbox properties
valid_checkbox_properties <- function() {
  c("equals",
    "does_not_equal")
}

# Valid date properties
valid_date_properties <- function() {
  c("equals",
    "before",
    "after",
    "on_or_before",
    "is_empty",
    "is_not_empty",
    "on_or_after",
    "past_week",
    "past_month",
    "past_year",
    "next_week",
    "next_month",
    "next_year")
}

# Valid files properties
valid_files_properties <- function() {
  c("is_empty",
    "is_not_empty")
}

# Valid formula properties
valid_formula_properties <- function() {
  c("text",
    "checkbox",
    "number",
    "date")
}

# Valid multi-select properties
valid_multiselect_properties <- function() {
  c("contains",
    "does_not_contain",
    "is_empty",
    "is_not_empty")
}

# Valid number properties
valid_number_properties <- function() {
  c("equals",
    "does_not_equal",
    "greater_than",
    "less_than",
    "greater_than_or_equal_to",
    "less_than_or_equal_to",
    "is_empty",
    "is_not_empty")
}

# Valid people properties
valid_people_properties <- function() {
  c("contains",
    "does_not_contain",
    "is_empty",
    "is_not_empty")
}

# Valid relation properties
valid_relation_properties <- function() {
  c("contains",
    "does_not_contain",
    "is_empty",
    "is_not_empty")
}

# Valid select properties
valid_select_properties <- function() {
  c("equals",
    "does_not_equal",
    "is_empty",
    "is_not_empty")
}

# Valid text properties
valid_text_properties <- function() {
  c("equals",
    "does_not_equal",
    "contains",
    "does_not_contain",
    "starts_with",
    "ends_with",
    "is_empty",
    "is_not_empty")
}
