#' Retrieve page and database properties
#'
#' Database and page properties contain important structural information and
#' content. This function retrieves high-level information about these
#' properties and returns them in a tidy format.
#'
#' @param x A page or database.
#' @return A data.frame containing structured, high-level information about the
#'   object's properties.
#' @export
properties <- function(x) {
  UseMethod("properties")
}

#' Retrieve the children of a block object
#'
#' This function allows the user to recursively/non-recursively access a block
#' object's children. NOTE: Pages are also blocks and therefore may have
#' children as well.
#'
#' @param x A page or block.
#' @param recursive A boolean value. If `TRUE`, this will recursively access
#'   all children of the specified block. If `FALSE`, it will return only the
#'   first level of block children.
#' @return If recursive, a tree containing all block children. Otherwise a
#'   single-level list of block children.
#' @export
children <- function(x, recursive = TRUE) {
  UseMethod("children")
}

#' Print detailed object summary
#'
#' `details` will display a detailed summary for pages or databases
#' in a tree-like fashion. This is particularly helpful for getting a quick
#' overview of an object's structure.
#'
#' @param x A page or database.
#' @return Invisibly returns itself. Called primarily for the side effects.
#' @export
details <- function(x) {
  UseMethod("details")
}

#' Retrieve page and database IDs
#'
#' The `id` function returns the title and ID of pages and databases. In some
#' functions the object name and ID can be used interchangeably, although
#' using the ID is usually recommended.
#'
#' @param x A page or database.
#' @return A data.frame containing the object's name and ID in a tidy format.
#' @export
id <- function(x) {
  UseMethod("id")
}

#' Extract object's primary content
#'
#' Pages and blocks contain different content. This function provides a
#' straightforward way to access the content from these relevant object types.
#'
#' @param x A page or block object.
#' @return A list; this contains the content contained within all relevant
#'   children or properties within a block or a page.
#' @export
object_content <- function(x) {
  UseMethod("object_content")
}

#' @method object_content default
#' @export
object_content.default <- function(x) {
  NULL
}

#' Retrieve page or database title
#'
#' Pages and Database objects store their title property as an array of
#' \href{https://developers.notion.com/reference/rich-text}{rich text objects}.
#' Typically the title only consists of one relevant element, and thus this
#' function will return the first element as a default.
#'
#' @param x A page or database.
#' @param all.titles A boolean indicating whether to return only the first title
#'   array element, or all elements.
#' @return If `all.titles` is `TRUE`, a list of all title elements. Otherwise,
#'   a single title value.
title <- function(x, all.titles = FALSE) {
  UseMethod("title")
}
