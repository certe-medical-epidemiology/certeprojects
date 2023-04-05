# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

#' Connect to Microsoft Teams via Microsoft 365
#'
#' These functions create a connection to Microsoft Teams via Microsoft 365 and saves the connection to the `certeprojects` package environment. The `teams_*()` functions allow to work with this connection.
#' @param team_name name of the team
#' @param ... arguments passed on to [get_microsoft365_token()]
#' @param account a Microsoft 365 account to use for looking up properties. This has to be an object as returned by [teams_connect()] or [Microsoft365R::get_team()].
#' @param channel name of the Teams channel, such as "General"
#' @rdname teams
#' @name teams
#' @importFrom Microsoft365R get_team
#' @export
#' @examples 
#' \dontrun{
#' 
#' teams_upload("myfile.docx", channel = "My Channel")
#' teams_upload("myfile.docx", "my channel/my folder/test.docx")
#' 
#' # also supports data frames, they will be saved locally in a temp folder
#' teams_upload(mtcars, channel = "My Channel")
#' mtcars |> 
#'   teams_upload(channel = "My Channel")
#'   
#'   
#' # integrates with department projects:
#' 
#' # upload "myfile.docx" from the project folder of p123 to "My Channel":
#' teams_upload("myfile.docx", channel = "My Channel", card_number = 123)
#' # download "myfile.docx" from "My Channel" to the project folder of p123:
#' teams_download("myfile.docx", channel = "My Channel", card_number = 123)
#' teams_download("mychannel/myfile.docx", card_number = 123)
#' 
#' # direct import from Teams requires the 'certetoolbox' package
#' x <- teams_import("mychannel/myfile.docx")
#' x <- teams_import("mychannel/myfile.docx", card_number = 123)
#' x <- teams_import("myfile.docx", channel = "My Channel", card_number = 123)
#' 
#' # open a file in Excel Online
#' teams_open("test.xlsx", "My Channel")
#' teams_open("my channel/test.xlsx") # shorter version, tries to find channel
#' }
teams_connect <- function(team_name = read_secret("teams.name"), ...) {
  
  if (is.null(pkg_env$microsoft365_teams)) {
    # not yet connected to Teams in Microsoft 365, so set it up
     pkg_env$microsoft365_teams <- get_team(team_name = team_name,
                                            token = get_microsoft365_token(...))
  }
  # this will auto-renew authorisation when due
  return(invisible(pkg_env$microsoft365_teams))
}

#' @rdname teams
#' @export
teams_name <- function(account = teams_connect()) {
  get_teams_property(account, "displayName")
}

#' @rdname teams
#' @export
teams_channels <- function(account = teams_connect()) {
  if (!is_valid_teams(account)) {
    return(NA_character_)
  }
  sort(vapply(FUN.VALUE = character(1),
              account$list_channels(), 
              function(ch) ch$properties$displayName))
}

#' @rdname teams
#' @export
teams_view_sharepoint <- function(channel, account = teams_connect()) {
  if (!is_valid_teams(account)) {
    return(NA_character_)
  }
  channel <- retrieve_channel(path = channel, account = account)
  if (is.na(channel)) {
    return(NA_character_)
  } else {
    account$
      get_channel(channel)$
      get_folder()$
      open()
  }
}

#' @param body text of the message
#' @param content_type type of content, must be "text" or "html"
#' @param attachments vector of file locations of attachments to add to the message
#' @rdname teams
#' @export
teams_send_message <- function(body,
                               channel,
                               content_type = c("text", "html"),
                               attachments = NULL,
                               account = teams_connect()) {
  if (!is_valid_teams(account)) {
    stop("No valid Teams account")
  }
  out <- account$
    get_channel(channel)$
    send_message(body = body,
                 content_type = content_type[1],
                 attachments = attachments)
  invisible(out)
}

#' @param local_path file location on the local system, can also be a [data.frame] which will then be saved locally to the temp folder first
#' @param teams_path file location in Microsoft Teams, may also contain the channel name if `channel` is `NULL`, e.g., `teams_path = "channel name/test.xlsx"`
#' @rdname teams
#' @export
teams_upload <- function(local_path,
                         teams_path = basename(local_path),
                         card_number = project_get_current_id(ask = FALSE),
                         channel = NULL,
                         account = teams_connect()) {
  if (!is_valid_teams(account)) {
    stop("No valid Teams account")
  }
  if (is.data.frame(local_path)) {
    message("Saving data set as RDS file to temporary location...", appendLF = FALSE)
    filename <- deparse(substitute(local_path))
    if (filename == ".") {
      filename <- paste0("data_", concat(sample(c(letters[seq_len(6)], 0:9), size = 8, replace = TRUE)))
    }
    tmp <- paste0(tempdir(), "/", filename, ".rds")
    saveRDS(object = local_path, file = tmp, compress = "xz")
    local_path <- tmp
    teams_path <- paste0(filename, ".rds")
    message("OK.")
  }
  if (!is.null(card_number)) {
    local_path <- project_set_file(filename = local_path, card_number = card_number)
  }
  if (!file.exists(local_path)) {
    stop("Path not found: ", local_path)
  }
  if (is.null(channel)) {
    # find channel based on teams path
    channel <- retrieve_channel(path = teams_path, account = account)
    if (!is.na(channel) && teams_path %like% "[/]") {
      # a channel was found, so remove first part of name from teams_path
      teams_path <- gsub("^(.*?)/(.*)", "\\2", teams_path)
    } else if (is.na(channel)) {
      stop("No valid channel set")
    }
  }
  message("Uploading '", local_path,
          "' to channel '", channel,
          "' as '", teams_path, "'...", appendLF = FALSE)
  tryCatch({
    account$
      get_channel(channel)$
      upload_file(src = local_path,
                  dest = teams_path)
    message("OK.")
  }, error = function(e) message("ERROR.\n", e$message))
  
  # remove temp file
  try(unlink(tmp), silent = TRUE)
}

#' @inheritParams project_properties
#' @rdname teams 
#' @export
teams_download <- function(teams_path,
                           local_path = basename(teams_path),
                           card_number = project_get_current_id(ask = FALSE),
                           channel = NULL,
                           account = teams_connect()) {
  if (!is_valid_teams(account)) {
    stop("No valid Teams account")
  }
  if (is.null(channel)) {
    # find channel based on teams path
    channel <- retrieve_channel(path = teams_path, account = account)
    if (!is.na(channel) && teams_path %like% "[/]") {
      # a channel was found, so remove first part of name from teams_path
      teams_path <- gsub("^(.*?)/(.*)", "\\2", teams_path)
    } else if (is.na(channel)) {
      stop("No valid channel set")
    }
  }
  if (!is.null(card_number)) {
    local_path <- project_set_file(filename = local_path, card_number = card_number)
  }
  message("Downloading Teams file '", teams_path,
          "' from channel '", channel,
          "' as '", local_path, "'...", appendLF = FALSE)
  tryCatch(
    account$
      get_channel(channel)$
      download_file(src = teams_path,
                    dest = local_path,
                    overwrite = TRUE),
    error = function(e) message("ERROR.\n", e$message))
  if (file.exists(local_path)) {
    message("OK.")
    return(invisible(local_path))
  } else {
    return(NULL)
  }
}

#' @inheritParams project_properties
#' @rdname teams 
#' @export
teams_import <- function(teams_path,
                         card_number = project_get_current_id(ask = FALSE),
                         channel = NULL,
                         account = teams_connect()) {
  suppressMessages(
    tempfile <- teams_download(teams_path = teams_path,
                               local_path = paste0(tempdir(), "/", basename(teams_path)),
                               card_number = card_number,
                               channel = channel,
                               account = account)
  )
  certetoolbox::import(tempfile)
}

#' @rdname teams
#' @export
teams_open <- function(teams_path, channel = NULL, account = teams_connect()) {
  if (!is_valid_teams(account)) {
    stop("No valid Teams account")
  }
  if (is.null(channel)) {
    # find channel based on teams path
    channel <- retrieve_channel(path = teams_path, account = account)
    if (!is.na(channel) && teams_path %like% "[/]") {
      # a channel was found, so remove first part of name from teams_path
      teams_path <- gsub("^(.*?)/(.*)", "\\2", teams_path)
    } else if (is.na(channel)) {
      stop("No valid channel set")
    }
  }
  account$
    get_channel(channel)$
    get_folder()$
    get_item(teams_path)$
    open()
}

#' @rdname teams
#' @param share_type type of share, must be `"view"` (default) or `"edit"`
#' @param expire_after time span after which the share link expires, defaults to `"1 month"`, can also be e.g. `"7 days"`
#' @param password password to set for share link, defaults to blank
#' @export
teams_get_link <- function(teams_path,
                           share_type = c("view", "edit"),
                           expire_after = "1 month",
                           password = NULL,
                           channel = NULL,
                           account = teams_connect()) {
  if (!is_valid_teams(account)) {
    stop("No valid Teams account")
  }
  if (is.null(channel)) {
    # find channel based on teams path
    channel <- retrieve_channel(path = teams_path, account = account)
    if (!is.na(channel) && teams_path %like% "[/]") {
      # a channel was found, so remove first part of name from teams_path
      teams_path <- gsub("^(.*?)/(.*)", "\\2", teams_path)
    } else if (is.na(channel)) {
      stop("No valid channel set")
    }
  }
  account$
    get_channel(channel)$
    get_folder()$
    get_item(teams_path)$
    create_share_link(type = share_type[1L],
                      expiry = expire_after,
                      password = password)
}

is_valid_teams <- function(account) {
  # inherits() returns FALSE for NULL, so no need to check is.null(account)
  inherits(account, "ms_object") &&
    tryCatch(!is.null(account$list_channels), error = function(e) FALSE) &&
    is.function(account$list_channels)
}

get_teams_property <- function(account, property_names, account_fn = NULL) {
  if (is_valid_teams(account)) {
    if (!is.null(account_fn)) {
      account <- account[[account_fn]]()
    }
    out <- unique(as.character(unname(unlist(account$properties[c(property_names)]))))
    if (length(out) == 0 || all(is.na(out))) {
      out <- NA_character_
    }
  } else {
    out <- NA_character_
  }
  return(out)
}

retrieve_channel <- function(path, account = teams_connect()) {
  channels <- teams_channels(account = account)
  if (path %in% channels) {
    return(path)
  }
  channels_plain <- trimws(gsub("[^a-zA-Z0-9 ]", "", channels))
  path_plain <- gsub("[^a-zA-Z0-9 /]", "", gsub("\\\\", "/", path, fixed = TRUE))
  out <- channels[path_plain %like% channels_plain][1L]
  if (is.na(out)) {
    out <- channels[channels_plain %like% path_plain][1L]
  }
  if (is.na(out)) {
    # still hard to find, then try first part of path
    path_plain <- gsub("^(.*?)/(.*)", "\\1", path_plain)
    out <- channels[channels_plain %like% path_plain][1L]
  }
  out
}
