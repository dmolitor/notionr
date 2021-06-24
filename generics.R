# Create generics for Pages and Databases
properties <- function(x) {
  UseMethod("properties")
}

# Create generics for Pages and Databases
id <- function(x) {
  UseMethod("id")
}

# Create generic for extracting block children
children <- function(x, recursive = TRUE) {
  UseMethod("children")
}
