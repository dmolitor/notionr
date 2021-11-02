# Base64 encode url
base64url <- function (x) {
  if (is.character(x)) {
    x <- charToRaw(x)
  }
  out <- chartr("+/", "-_", openssl::base64_encode(x))
  gsub("=+$", "", out)
}

# Is something a formula or coercible to one?
coerce_formula <- function(x) {
  if (inherits(x, "formula")) {
    form <- x
    is_formula <- TRUE
  } else {
    form <- tryCatch(stats::as.formula(x), error = function(e) NULL)
    is_formula <- inherits(form, "formula")
  }
  return(list("is_formula" = is_formula,
              "formula" = form))
}

# Encode character values
encode_character <- function(x) {
  stopifnot(is.character(x))
  paste0("'", x, "'")
}

# Format filter and sort body as json
jsonize <- function(x, pretty = FALSE) {
  jsonlite::toJSON(x, auto_unbox = TRUE, pretty = pretty)
}

# Is a variable a non-zero character element?
nzchar <- function(x) {
  is.character(x) && nchar(x) > 0
}

# Parse a formula into its variables
parse_formula <- function(x) {
  coerced_x <- coerce_formula(x)
  stopifnot(coerced_x$is_formula)
  x <- coerced_x$formula
  if (length(x) < 3) stop("Missing formula LHS", call. = FALSE)
  lhs <- as.character(x[[2]])
  rhs <- if (inherits(x[[3]], "name")) {
    as.character(x[[3]])
  } else {
    x[[3]]
  }
  if (!(is.null(rhs) ||
        is.numeric(rhs) ||
        nzchar(rhs) ||
        isTRUE(rhs) ||
        isFALSE(rhs))) {
    stop("Formula rhs must be one of class 'character', 'numeric', 'NULL', or 'logical'")
  }
  return(list(lhs = lhs, rhs = rhs))
}

# Replace NULL with empty json body within filter query
replace_null_empty_json <- function(x) {
  stopifnot(all(c("lhs", "rhs") %in% attributes(x)$names))
  x$rhs <- if (is.null(x$rhs)) {
    "{}"
  } else {
    x$rhs
  }
  return(x)
}

# Helper to replace Null value with empty character
replace_null_zchar <- function(x) {
  if (is.null(x)) return("")
  x
}

# Wrap NULLs in a list
wrap_null <- function(x) {
  if(!is.null(x)) return(x)
  list(x)
}
