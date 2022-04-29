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
#' @param tenant the tenant to use for [Microsoft365R::get_team()]
#' @param error_on_fail a [logical] to indicate whether an error must be thrown if no connection can be made
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
#' mtcars %>%
#'   teams_upload(channel = "My Channel")
#' 
#' # open a file in Excel Online
#' teams_open("test.xlsx", "My Channel")
#' teams_open("my channel/test.xlsx") # shorter version, tries to find channel
#' 
#' }
teams_connect <- function(team_name = read_secret("teams.name"), 
                          tenant = read_secret("mail.tenant"),
                          error_on_fail = FALSE) {
  # see here: https://docs.microsoft.com/en-us/graph/permissions-reference
  scopes <- c("Channel.ReadBasic.All",
              "ChannelMessage.Send",
              "Chat.ReadWrite",
              "ChatMessage.Send",
              "Files.ReadWrite.All",
              "Sites.ReadWrite.All",
              "Team.ReadBasic.All",
              "User.Read")
  if (tenant == "") {
    tenant <- NULL
  }
  if (is.null(pkg_env$teams)) {
    # not yet connected to Microsoft 365, so set it up
    tryCatch({
      if (is.null(tenant)) {
        pkg_env$teams <- suppressWarnings(suppressMessages(get_team(team_name = team_name, 
                                                                    scopes = scopes)))
      } else {
        pkg_env$teams <- suppressWarnings(suppressMessages(get_team(team_name = team_name, 
                                                                    scopes = scopes,
                                                                    tenant = tenant)))
      }
      message("Connected to Teams '", teams_name(account = pkg_env$teams), "' through Microsoft 365.")
    }, warning = function(w) {
      return(invisible())
    }, error = function(e, fail = error_on_fail) {
      if (isTRUE(fail)) {
        stop("Could not connect to Microsoft 365: ", paste0(e$message, collapse = ", "), call. = FALSE)
      } else {
        warning("Could not connect to Microsoft 365: ", paste0(e$message, collapse = ", "), call. = FALSE)
      }
      return(NULL)
    })
  }
  if (isTRUE(error_on_fail) && is.null(pkg_env$teams)) {
    stop("Could not connect to Microsoft 365.", call. = FALSE)
  }
  # this will auto-renew authorisation when due
  return(invisible(pkg_env$teams))
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
    account$get_channel(channel)$get_folder()$open()
  }
}

#' @param body text of the message
#' @param content_type type of content, must be "text" or "html"
#' @param attachments vector of file locations of attachments to add to the message
#' @rdname teams
#' @export
teams_send_message <- function(body,
                               content_type = c("text", "html"),
                               attachments = NULL,
                               channel,
                               account = teams_connect()) {
  if (!is_valid_teams(account)) {
    stop("No valid Teams account")
  }
  account$get_channel(channel)$send_message(body = body, content_type = content_type[1], attachments = attachments)
}

#' @param local_path file location on the local system, can also be a [data.frame] which will then be saved locally to the temp folder first
#' @param teams_path file location in Microsoft Teams, may also contain the channel name if `channel` is `NULL`, e.g., `teams_path = "channel name/test.xlsx"`
#' @rdname teams
#' @export
teams_upload <- function(local_path, teams_path = basename(local_path), channel = NULL, account = teams_connect()) {
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
    saveRDS(object = local_path, file = tmp, version = 2, compress = "xz")
    local_path <- tmp
    teams_path <- paste0(filename, ".rds")
    message("OK.")
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
          "' to Teams channel '", channel,
          "' as '", teams_path, "'...", appendLF = FALSE)
  tryCatch({
    account$get_channel(channel)$upload_file(src = local_path, dest = teams_path)
    message("OK.")
  }, error = function(e) message("ERROR.\n", e$message))
}

#' @inheritParams project_properties
#' @rdname teams 
#' @export
teams_download <- function(teams_path, local_path = basename(teams_path), card_number = project_get_current_id(ask = FALSE), channel = NULL, account = teams_connect()) {
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
  tryCatch({
    account$get_channel(channel)$download_file(src = teams_path, dest = local_path, overwrite = TRUE)
    if (file.exists(local_path)) {
      message("OK.")
    }
  }, error = function(e) message("ERROR.\n", e$message))
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
  account$get_channel(channel)$get_folder()$get_item(teams_path)$open()
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
