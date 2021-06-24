# Helper to replace Null value with empty character
replace_null_zchar <- function(x) {
  if (is.null(x)) return("")
  x
}
