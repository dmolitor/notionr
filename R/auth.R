# Retrieves Notion access information
#
# Uses OAuth bearer token to retrieve access information
access_json <- function(id, secret) {
  stopifnot(is.character(id), is.character(secret))
  code <- auth_code(id)
  token_req <- httr::stop_for_status(
    httr::POST(
      url = "https://api.notion.com/v1/oauth/token",
      httr::add_headers(
        "Authorization" = paste(
          "Basic",
          base64url(
            paste0(id, ":", secret)
          )
        )
      ),
      body = list("grant_type" = "authorization_code",
                  "code" = code,
                  "redirect_uri" = httr::oauth_callback()),
      encode = "json"
    ),
    task = "Failed to exchange authorization grant for access code"
  )
  httr::content(token_req, as = "text")
}

# NotionR Integration client ID and client secret
app_id_secret <- function() {
  list(
    "oauth_client_id" = "ec6ea066-3f97-40c3-8a30-b4ac5d9fe927",
    "oauth_client_secret" = "secret_HwN0JHGuehx5cOofTOrcju91P1A5sbzjHzoO2iODVdA"
  )
}

# Retrieves OAuth bearer token
#
# This function launches a web browser to run through OAuth validation
auth_code <- function(id) {
  state <- state_gen()
  auth_vals <- httr::oauth_listener(
    request_url = auth_url(id = id, state = state)
  )
  if (auth_vals$state != state) stop("Bad return state value", call. = FALSE)
  auth_vals$code
}

# OAuth URL
#
# This function creates and OAuth url using the NotionR Client ID and Secret
auth_url <- function(id, state) {
  stopifnot(is.character(id), is.character(state))
  sprintf(
    "https://api.notion.com/v1/oauth/authorize?client_id=%s&redirect_uri=%s&response_type=code&state=%s&owner=user",
    id,
    utils::URLencode(httr::oauth_callback(),
                     reserved = TRUE),
    state
  )
}

# Find closest available cache directory
#
# If NotionR cache directories clash, find the closest available directory name
available_cache_name <- function(cache.dir, workspace.name) {
  which_cache <- unlist(
    lapply(2:10, function(i) {
      !dir.exists(paste0(cache.dir, "/", workspace.name, "-", i))
    })
  )
  if (all(!which_cache)) {
    stop(paste0("Too many directories that begin with ",
                paste0(cache.dir, "/", workspace.name)),
         call. = FALSE)
  }
  cache_idx <- (2:10)[which_cache][[1]]
  paste0(workspace.name, "-", cache_idx)
}

#' Retrieve cached Notion access token
#'
#' `cached_access_code` retrieves a cached Notion access token from the default
#' cache directory, directory defined by environment variable, or user defined path
#'
#' @param path A path to the local directory that contains the Notion access
#'   token. If left `NULL`, it will be set as the default cache directory or
#'   search for the environment variable `NOTIONR_CACHE`.
#'
#' @return A character string that is the user's Notion access token.
#'   If no access token exists, an error will be thrown
#'
#' @export
cached_access_code <- function(path = NULL) {
  if (is.null(path) && cached_access_exists() == "none") {
    stop("No access code found. See notion_auth_configure for details on setting access code",
         call. = FALSE)
  }
  if (!is.null(path)) {
    if (!file.exists(paste0(path, "/notionr_oauth_access.json"))) stop("Access code not found in the provided directory")
    json_body <- jsonlite::fromJSON(
      jsonlite::read_json(paste0(path, "/notionr_oauth_access.json"),
                          simplifyVector = TRUE)
    )
    return(json_body$access_token)
  }
  if (cached_access_exists() == "both") {
    stop(paste0("You appear to already have multiple Notion access codes cached in the following directories:\n\t",
                paste0(c(default_cache_dir(),
                         Sys.getenv("NOTIONR_CACHE")),
                       collapse = "\n\t"),
                "\nTo avoid confusion, you should condense access codes into one of the listed directories"),
         call. = FALSE)
  }
  if (cached_access_exists() == "default") {
    workspace.name <- current_workspace_name(cache.dir = default_cache_dir(), type = "grab")
    json_body <- jsonlite::fromJSON(
      jsonlite::read_json(paste0(default_cache_dir(),
                                 workspace.name,
                                 "/notionr_oauth_access.json"),
                          simplifyVector = TRUE)
    )
  }
  if (cached_access_exists() == "environment") {
    workspace.name <- current_workspace_name(cache.dir = Sys.getenv("NOTIONR_CACHE"), type = "grab")
    json_body <- jsonlite::fromJSON(
      jsonlite::read_json(paste0(Sys.getenv("NOTIONR_CACHE"),
                                 "/",
                                 workspace.name,
                                 "/notionr_oauth_access.json"),
                          simplifyVector = TRUE)
    )
  }
  return(json_body$access_token)
}

# Check if cached Notion access information exists
#
# Checks default directory and environment variables to see if cached access exists
cached_access_exists <- function() {
  default <- default_cache_dir()
  custom <- Sys.getenv("NOTIONR_CACHE")
  default_dirs <- list.dirs(default, full.names = FALSE)[!list.dirs(default, full.names = FALSE) == ""]
  custom_dirs <- list.dirs(custom, full.names = FALSE)[!list.dirs(custom, full.names = FALSE) == ""]
  default_exists <- !identical(character(0), default_dirs)
  custom_exists <- !identical(character(0), custom_dirs)
  if (!(default_exists || custom_exists))  return("none")
  if (default_exists && custom_exists) return("both")
  if (default_exists) return("default")
  return("environment")
}

# Caches Notion access token
#
# Takes Notion access information and caches it in local directory
cache_access_info <- function(access, cache.dir = default_cache_dir(), workspace.name = NULL) {
  if (!jsonlite::validate(access)) stop("Access code is invalid .json format", call. = FALSE)
  stopifnot(is.character(cache.dir))
  if (!is.null(workspace.name)) {
    jsonlite::write_json(access,
                         paste0(cache.dir,
                                "/",
                                workspace.name,
                                "/notionr_oauth_access.json"))
    cat("Access credentials have been stored at", paste0(cache.dir, "/", workspace.name, "/notionr_oauth_access.json\n"))
    return(invisible(NULL))
  }
  access_info <- jsonlite::fromJSON(access)
  workspace_name <- workspace_name_clean(access_info$workspace_name)
  if (dir.exists(paste0(cache.dir, "/", workspace_name))) {
    what_to_do <- handle_same_workspace_name(cache.dir, workspace_name)
    if (what_to_do == 0) return(invisible(NULL))
    if (what_to_do == 1) workspace_name <- current_workspace_name(cache.dir, type = "refresh")
    if (what_to_do == 2) workspace_name <- available_cache_name(cache.dir, workspace_name)
  }
  if (cache.dir == default_cache_dir()) {
    open_default_cache_dir(workspace.dir = workspace_name)
  } else {
    if (!dir.exists(paste0(cache.dir, "/", workspace_name))) {
      dir.create(paste0(cache.dir, "/", workspace_name))
    }
  }
  jsonlite::write_json(access,
                       paste0(cache.dir,
                              "/",
                              workspace_name,
                              "/notionr_oauth_access.json"))
  cat("Access credentials have been stored at", paste0(cache.dir, "/", workspace_name, "/notionr_oauth_access.json\n"))
}

# Which workspace is the relevant one
#
# If multiple workspaces are cached, which workspace is the user looking for
current_workspace_name <- function(cache.dir, type = "refresh") {
  if (!type %in% c("refresh", "grab")) stop("type must be 'refresh' or 'grab'")
  workspace_names <- list.dirs(cache.dir, full.names = FALSE)
  workspace_names <- workspace_names[!workspace_names == ""]
  if (identical(workspace_names, character(0))) stop("No workspaces found in specified cache directory", call. = FALSE)
  if (length(workspace_names) == 1) return(workspace_names)
  which_workspace <- utils::menu(
    choices = workspace_names,
    title = paste0("Which workspace would you like to ", type, " the access code for?")
  )
  if (which_workspace == 0) stop("Please make a valid selection", call. = FALSE)
  return(workspace_names[[which_workspace]])
}

# Function that defines the default Notion cache directory
default_cache_dir <- function() {
  "~/.R/notionr/oauth/"
}

# Handle clashing cache names
#
# Ask the user how they want to handle clashing cache names
handle_same_workspace_name <- function(cache.dir, workspace.name) {
  refresh_or_nah <- utils::menu(choices = c("Refresh a code for an existing workspace",
                                            "This is a new workspace with the same name"),
                                title = "You've already cached access credentials for a workspace with the same name. What do you want to do?")
  return(refresh_or_nah)
}

#' Orchestrate the Notion authorization ritual
#'
#' `notion_auth` guides the user to retrieving their Notion access token.
#'
#' You are directed to a web browser, asked to sign in to your Notion account,
#' and to grant notionr permission to operate on your behalf with Notion.
#' By default, these user credentials are cached in a folder below your home
#' directory, ~/.R/notionr/oauth/, from where they can be automatically
#' refreshed, as necessary. Storage at the user level means the same token can
#' be used across multiple projects and tokens are less likely to be synced to
#' the cloud by accident.
#'
#' @param return.key A logical value indicating whether to return the auth
#'   token without caching it.
#' @param cache.dir A directory path indicating where to cache the auth token.
#'
#' @return If `return.key` is `TRUE`, it will return the auth token as a
#'   character string. Otherwise it will cache the token and return nothing.
#'
#' @export
notion_auth <- function(return.key = FALSE, cache.dir = default_cache_dir()) {
  stopifnot(return.key %in% c(TRUE, FALSE))
  stopifnot(is.character(cache.dir) || is.null(cache.dir))
  notion_id <- app_id_secret()$oauth_client_id
  notion_secret <- app_id_secret()$oauth_client_secret
  access_code <- access_json(id = notion_id, secret = notion_secret)
  if (!return.key) {
    cache_access_info(access = access_code, cache.dir = cache.dir)
  } else {
    access_code <- jsonlite::fromJSON(access_code)$access_token
    return(access_code)
  }
}

# Create cache directory
#
# Function that creates the cache directory to house the Notion auth information
open_default_cache_dir <- function(workspace.dir = NULL) {
  if (!dir.exists("~/.R/")) dir.create("~/.R/")
  if (!dir.exists("~/.R/notionr/")) dir.create("~/.R/notionr/")
  if (!dir.exists("~/.R/notionr/oauth/")) dir.create("~/.R/notionr/oauth/")
  if (!is.null(workspace.dir)) {
    if (!dir.exists(paste0("~/.R/notionr/oauth/", workspace.dir))) dir.create(paste0("~/.R/notionr/oauth/", workspace.dir))
  }
}

# Create random state string
#
# Creates state string to ensure that the OAuth dance hasn't been compromised
state_gen <- function() {
  sprintf("%s%s%s",
          paste0(sample(LETTERS, 5, TRUE), collapse = ""),
          paste0(sample(0:9, 4, TRUE), collapse = ""),
          paste0(sample(LETTERS, 1, TRUE), collapse = ""))
}

# Remove stray "-"s from workspace name
workspace_name_clean <- function(x) {
  stringr::str_replace_all(x, " ", "-")
}
