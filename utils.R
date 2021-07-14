# Helper to replace Null value with empty character
replace_null_zchar <- function(x) {
  if (is.null(x)) return("")
  x
}

# Is a variable a non-zero character element?
nzchar <- function(x) {
  is.character(x) && nchar(x) > 0
}