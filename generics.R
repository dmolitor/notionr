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

# Create generic for object content
object_content <- function(x) {
  UseMethod("object_content")
}

# Default method for object content
object_content.default <- function(x) {
  NULL
}

# Create generic for title property
title <- function(x, ...) {
  UseMethod("title")
}
