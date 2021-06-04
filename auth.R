app_id_secret <- function() {
  list(
    "oauth_client_id" = "ec6ea066-3f97-40c3-8a30-b4ac5d9fe927",
    "oauth_client_secret" = "secret_HwN0JHGuehx5cOofTOrcju91P1A5sbzjHzoO2iODVdA"
  )
}

auth_url <- function(id, state) {
  stopifnot(is.character(id), is.character(state))
  sprintf(
    "https://api.notion.com/v1/oauth/authorize?client_id=%s&redirect_uri=%s&response_type=code&state=%s",
    id,
    URLencode(httr::oauth_callback(),
              reserved = TRUE),
    state
  )
}

auth_code <- function(id) {
  state <- state_gen()
  auth_vals <- httr::oauth_listener(
    request_url = auth_url(id = id, state = state)
  )
  if (auth_vals$state != state) stop("Bad return state value", call. = FALSE)
  auth_vals$code
}

access_json <- function(id, secret) {
  stopifnot(is.character(id), is.character(secret))
  code <- auth_code(id)
  token_req <- httr::stop_for_status(
    httr::POST(
      url = "https://api.notion.com/v1/oauth/token",
      httr::add_headers(
        "Authorization" = paste(
          "Basic",
          httr:::base64url(
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
    if (what_to_do == 2) workspace_name <- paste0(workspace_name, "-2")
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

default_cache_dir <- function() {
  "~/.R/notionr/oauth/"
}

# clashing_notion_caches <- function(d) {
#   if (cached_access_exists() == "both") {
#     stop(paste0("You appear to already have multiple Notion access codes cached in the following directories:\n\t",
#                    paste0(c(default_cache_dir(),
#                             Sys.getenv("NOTIONR_CACHE")),
#                           collapse = "\n\t"),
#                    "\nTo avoid confusion, you should remove one of the listed files"),
#          call. = FALSE)
#   }
#   if (cached_access_exists() == "default" && (d != default_cache_dir() && paste0(d, "/") != default_cache_dir())) {
#     stop(paste0("You appear to already have a Notion access code cached in the following directory:\n\t",
#                 default_cache_dir(),
#                 "\nTo avoid confusion, you should remove this file before caching an access code in a new directory"),
#          call. = FALSE)
#   }
#   if (cached_access_exists() == "environment" && (d != Sys.getenv("NOTIONR_CACHE") && paste0(d, "/") != Sys.getenv("NOTIONR_CACHE"))) {
#     stop(paste0("You appear to already have a Notion access code cached in the following directory:\n\t",
#                 Sys.getenv("NOTIONR_CACHE"),
#                 "\nTo avoid confusion, you should remove this file before caching an access code in a new directory"),
#          call. = FALSE)
#   }
# }

cached_access_code <- function(path = NULL) {
  if (is.null(path) && cached_access_exists() == "none") {
    stop("No access code found. See notion_auth for details on setting access code",
         call. = FALSE)
  }
  if (!is.null(path)) {
    if (!file.exists(paste0(path, "/notionr_oauth_access.json"))) stop("Access code not found in the provided directory")
    json_body <- jsonlite::fromJSON(
      jsonlite::read_json(paste0(path, "/notionr_oauth_access.json"),
                          simplifyVector = TRUE)
    )
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

current_workspace_name <- function(cache.dir, type = "refresh") {
  if (!type %in% c("refresh", "grab")) stop("type must be 'refresh' or 'grab'")
  workspace_names <- list.dirs(cache.dir, full.names = FALSE)
  workspace_names <- workspace_names[!workspace_names == ""]
  if (length(workspace_names) == 1) return(workspace_names)
  which_workspace <- menu(
    choices = workspace_names,
    title = paste0("Which workspace would you like to ", type, " the access code for?")
  )
  if (which_workspace == 0) stop("Please make a valid selection", call. = FALSE)
  return(workspace_names[[which_workspace]])
}

handle_same_workspace_name <- function(cache.dir, workspace.name) {
  refresh_or_nah <- menu(choices = c("Refresh a code for an existing workspace",
                                     "This is a new workspace with the same name"),
                         title = "You've already cached access credentials for a workspace with the same name. What do you want to do?")
  return(refresh_or_nah)
}

notion_auth_configure <- function(cache.dir = default_cache_dir()) {
  # # Check for clashing cached Notion access codes and warn/throw error
  # clashing_notion_caches(d = cache.dir)
  # # Check if access code already exists & needs refreshing
  # needs_refresh <- cached_access_exists()
  # if (needs_refresh %in% c("default", "environment")) {
  #   choice <- menu(
  #     choices = c("Yes, let's do it",
  #                 "Nope, I don't need this"), 
  #     title = paste0(
  #       "It appears that you already have an access code cached at ",
  #       ifelse(needs_refresh == "default", default_cache_dir(), Sys.getenv("NOTIONR_CACHE")),
  #       ". Would you like to refresh anyways?"
  #     )
  #   )
  #   if (!choice || choice == 2) return(invisible(NULL))
  # }
  # Proceed to obtaining & caching access code
  stopifnot(is.character(cache.dir))
  notion_id <- app_id_secret()$oauth_client_id
  notion_secret <- app_id_secret()$oauth_client_secret
  access_code <- access_json(id = notion_id, secret = notion_secret)
  cache_access_info(access = access_code, cache.dir = cache.dir)
}

notion_auth_refresh <- function(cache.dir = default_cache_dir()) {
  stopifnot(is.character(cache.dir))
  workspace <- current_workspace_name(cache.dir = cache.dir, type = "refresh")
  notion_id <- app_id_secret()$oauth_client_id
  notion_secret <- app_id_secret()$oauth_client_secret
  access_code <- access_json(id = notion_id, secret = notion_secret)
  cache_access_info(access = access_code, cache.dir = cache.dir, workspace.name = workspace)
}

nzchar <- function(x) {
  is.character(x) && nchar(x) > 0
}

open_default_cache_dir <- function(workspace.dir = NULL) {
  if (!dir.exists("~/.R/")) dir.create("~/.R/")
  if (!dir.exists("~/.R/notionr/")) dir.create("~/.R/notionr/")
  if (!dir.exists("~/.R/notionr/oauth/")) dir.create("~/.R/notionr/oauth/")
  if (!is.null(workspace.dir)) {
    if (!dir.exists(paste0("~/.R/notionr/oauth/", workspace.dir))) dir.create(paste0("~/.R/notionr/oauth/", workspace.dir))
  }
}

state_gen <- function() {
  sprintf("%s%s%s", 
          stringi::stri_rand_strings(1, 5, '[A-Z]'),
          stringi::stri_rand_strings(1, 4, '[0-9]'), 
          stringi::stri_rand_strings(1, 1, '[A-Z]'))
}

workspace_name_clean <- function(x) {
  stringr::str_replace_all(x, " ", "-")
}
