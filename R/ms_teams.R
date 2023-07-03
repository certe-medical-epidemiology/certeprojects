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
#' @importFrom Microsoft365R get_team ms_team
#' @export
#' @examples 
#' \dontrun{
#' # PROJECT-RELATED --------------------------------------------------------
#' 
#' # Project-related Teams function rely on existing Planner tasks.
#' 
#' # create a new project, which will be a folder in the Teams channel
#' # for this, the task 'My Planner task' must already exist
#' teams_new_project("My Planner task")
#' 
#' # the task 'My Planner task' will now contain the URL to the project
#' 
#' # upload a file there
#' teams_upload_project_file("analysis.Rmd", "My Planner task")
#' 
#' # render R markdown or Quarto from and to the cloud
#' teams_render_project_file("analysis.Rmd", "My Planner task")
#' # this will put the output file in the same Teams folder as 'analysis.Rmd'
#' 
#' 
#' # PROJECT-UNRELATED ------------------------------------------------------
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
teams_connect <- function(team_name = read_secret("team.name"), ...) {
  if (is.null(pkg_env$m365_getteam)) {
    # not yet connected to Teams in Microsoft 365, so set it up
    pkg_env$m365_getteam <- create_graph_login(token = get_microsoft365_token(scope = "teams", ...))$
      get_group(name = team_name)$
      get_team()
  }
  # this will auto-renew authorisation when due
  return(suppressMessages(invisible(pkg_env$m365_getteam)))
}


# PROJECT FUNCTIONS -----------------------------------------------------------

#' @rdname teams
#' @param projects_channel Teams channel name of the projects
#' @export
teams_projects_channel <- function(projects_channel = read_secret("teams.projects.channel"),
                                   account = teams_connect()) {
  channel_name <- find_channel(projects_channel, account = account)
  if (is.null(pkg_env$project_folder)) {
    pkg_env$project_folder <- account$
      get_channel(channel_name = channel_name)$
      get_folder()
  }
  return(pkg_env$project_folder)
}

#' @rdname teams
#' @param task any task title, task ID, or [`ms_plan_task`][Microsoft365R::ms_plan_task] object (e.g. from [planner_task_find()])
#' @param channel a Teams folder object. This has to be an object as returned by [teams_projects_channel()].
#' @param planner a Microsoft 365 account for Planner. This has to be an object as returned by [planner_connect()].
#' @details
#' The [teams_new_project()] function:
#' 1. Checks if there is a Planner task with the correct task title
#' 2. Creates a new folder in Teams in the projects channel
#' 3. Updates the task to contain the project folder URL as an attachment
#' 
#' @export
teams_new_project <- function(task, channel = teams_projects_channel(), planner = planner_connect()) {
  # validate that the task exists
  task <- planner_task_find(task, account = planner)
  task_title <- task$properties$title
  # create the folder
  if (task_title %in% channel$list_files()$name) {
    message("Project folder already exists, skipping.")
  } else {
    channel$create_folder(path = task_title)
  }
  # add link to Planner task
  link <- c(Projectmap = channel$get_item(task_title)$properties$webUrl)
  planner_task_update(task = task_title, attachment_urls = link)
}

#' @rdname teams
#' @export
teams_browse_project <- function(task, channel = teams_projects_channel(), planner = planner_connect()) {
  # validate that the task exists
  task <- planner_task_find(task, account = planner)
  # open in the browser
  channel$get_item(task$properties$title)$open()
}

#' @rdname teams
#' @importFrom Microsoft365R ms_drive ms_drive_item
#' @details
#' The [teams_download_project_file()] function will download the given project file to a temporary location, and will return the path of this location. This makes it possible to use [source()], [rmarkdown::render()] or [quarto::quarto_render()] using the [teams_download_project_file()] function as input.
#' @export
teams_download_project_file <- function(file, task, channel = teams_projects_channel(), planner = planner_connect()) {
  task <- planner_task_find(task, account = planner)
  file_teams <- teams_get_project_file(file = file, task = task, channel = channel, planner = planner)
  
  # download to temporary file
  file_local <- paste0(tempdir(), "/", file_teams$properties$name)
  try(unlink(file_local, force = TRUE), silent = TRUE)
  
  tryCatch(
    file_teams$download(dest = file_local, overwrite = TRUE),
    error = function(e) stop("Error while downloading file '", file_teams$properties$name, "' from '", task$properties$title, ":\n", e$message))
  if (file.exists(file_local)) {
    # download succeeded - return the file
    return(invisible(file_local))
  } else {
    return(NULL)
  }
}

#' @rdname teams
#' @importFrom certestyle font_bold
#' @export
teams_get_project_file <- function(file, task, channel = teams_projects_channel(), planner = planner_connect()) {
  # validate that the task exists
  task <- planner_task_find(task, account = planner)
  file_teams <- channel$get_item(task$properties$title)$list_items()
  files_found <- file_teams$name[which(file_teams$name %like% file)]
  sizes <- vapply(FUN.VALUE = character(1),
                  file_teams$size,
                  function(x) format(structure(x, class = "object_size"), units = "auto"))
  
  if (length(files_found) == 1) {
    filename <- files_found
  } else if (length(files_found) == 0) {
    warning("No files found")
    return(NA_character_)
  } else if (interactive()) {
    choice <- utils::menu(choices = paste0(font_bold(files_found, collapse = NULL), " (", sizes, ")"),
                          title = paste0("Files found in ", task$properties$title, " (0 to cancel):\n"))
    if (choice == 0) {
      return(NA_character_)
    }
    filename <- files_found[choice]
  } else {
    message("Files found:\n  - ", paste(files_found, collapse = "\n  - "),
            "\nSelecting first match.")
    filename <- files_found[1L]
  }
  channel$get_item(task$properties$title)$get_item(filename)
}

#' @rdname teams
#' @importFrom rstudioapi navigateToFile showDialog
#' @export
teams_open_project_analysis_file <- function(task, channel = teams_projects_channel(), planner = planner_connect()) {
  task <- planner_task_find(task, account = planner)
  path <- teams_download_project_file(file = ".*[.](R|qmd|Rmd|sql|txt|csv|tsv|css|ya?ml|js)$",
                                      task = task, channel = channel, planner = planner)
  if (is.na(path)) {
    stop("No syntax files found in project '", task$properties$title, "'")
  } else {
    invisible(navigateToFile(path))
    showDialog(title = "Temporary File",
               message = paste0("NOTE: File '", basename(path), "' has been opened from a temporary location."))
  }
}

#' @rdname teams
#' @param output_file path of the output file
#' @param fun function to use for rendering. Can be e.g. [rmarkdown::render] or [quarto::quarto_render].
#' @param ... arguments passed on to `fun`
#' @details
#' The [teams_render_project_file()] function allows to render a Teams file. It downloads the Teams file using [teams_download_project_file()], runs the rendering function set in `fun`, and uploads the resulting output file back to Teams using the same file name a `file`, but with the new file extension (such as pdf, html, or docx). It **invisibly returns** the temporary local file location, so that the output of [teams_render_project_file()] can be given to e.g. [certemail::mail()] as an attachment.
#' @importFrom Microsoft365R ms_drive ms_drive_item
#' @export
teams_render_project_file <- function(file,
                                      task,
                                      output_file = NULL,
                                      fun = rmarkdown::render,
                                      ...,
                                      channel = teams_projects_channel(),
                                      planner = planner_connect()) {
  temp_file <- teams_download_project_file(file = file, task = task, channel = channel, planner = planner)
  temp_dir <- dirname(temp_file)
  
  message(format(Sys.time()), " - Rendering file '", basename(temp_file), "'... ", appendLF = FALSE)
  
  if (is.null(output_file) || !"output_file" %in% names(formals(fun))) {
    if ("quiet" %in% names(formals(fun))) {
      fun(temp_file, ..., quiet = TRUE)
    } else {
      fun(temp_file, ...)
    }
    message("OK")
    output_file <- list.files(path = temp_dir,
                              pattern = gsub(paste0("[.]", tools::file_ext(temp_file), "$"), "", basename(temp_file)),
                              full.names = TRUE)
    output_file <- output_file[output_file %unlike% basename(temp_file)][1L]
  } else {
    if ("quiet" %in% names(formals(fun))) {
      fun(temp_file, ..., output_file = output_file, quiet = TRUE)
    } else {
      fun(temp_file, ..., output_file = output_file)
    }
    message("OK")
    if (!file.exists(output_file)) {
      # then it's in the temp location
      output_file <- paste0(temp_dir, "/", output_file)
    }
  }
  
  if (!is.na(output_file) && file.exists(output_file)) {
    # upload to Teams again
    # validate that the task exists
    task <- planner_task_find(task, account = planner)
    task_title <- task$properties$title
    message(format(Sys.time()), " - Uploading file '", basename(output_file), "'... ", appendLF = FALSE)
    new_file <- NULL
    tryCatch({
      # upload
      channel$get_item(task_title)$upload(src = output_file, dest = basename(output_file))
      # get file to return it in this function
      new_file <- channel$get_item(task_title)$get_item(basename(output_file))
      url <- new_file$get_path()
      if (interactive()) {
        # this will create a clickable link in the console, it was taken from cli::style_hyperlink
        url <- paste0("\033]8;;", new_file$properties$webUrl, "\a", "Teams-link", "\033]8;;\a")
      }
      message("OK (", url, ", ",
              format(structure(file.size(output_file), class = "object_size"), units = "auto"),
              ", v", get_version_number(new_file),
              ")")
    }, error = function(e) message("ERROR.\n", e$message))
    return(invisible(output_file))
  } else {
    stop("Rendered file not found: ", output_file)
  }
}

#' @rdname teams
#' @param file the file name to open
#' @export
teams_view_project_file <- function(file,
                                    task,
                                    channel = teams_projects_channel(),
                                    planner = planner_connect()) {
  # validate that the task exists
  task <- planner_task_find(task, account = planner)
  # open in the browser
  channel$get_item(task$properties$title)$get_item(file)$open()
}

#' @rdname teams
#' @param files the files to upload
#' @export
teams_upload_project_file <- function(files,
                                      task,
                                      channel = teams_projects_channel(),
                                      planner = planner_connect()) {
  # validate that the task exists
  task <- planner_task_find(task, account = planner)
  task_title <- task$properties$title
  # open in the browser
  folder <- channel$get_item(task_title)
  for (f in files) {
    if (!file.exists(f)) {
      stop("File ", f, " does not exist")
    }
    base <- basename(f)
    message("Uploading ", base, "... ", appendLF = FALSE)
    folder$upload(src = f, dest = base)
    message("OK")
  }
}


# OTHER FUNCTIONS -------------------------------------------------------------

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
  channel <- find_channel(path = channel, account = account)
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
  channel <- find_channel(path = channel, account = account)
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
    channel <- find_channel(path = teams_path, account = account)
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
    channel <- find_channel(path = teams_path, account = account)
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
    channel <- find_channel(path = teams_path, account = account)
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
    channel <- find_channel(path = teams_path, account = account)
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


# HELPER FUNCTIONS ------------------------------------------------------------

is_valid_teams <- function(account) {
  # inherits() returns FALSE for NULL, so no need to check is.null(account)
  inherits(account, "ms_object") &&
    tryCatch(!is.null(account$list_channels), error = function(e) FALSE) &&
    is.function(account$list_channels)
}

get_version_number <- function(file) {
  v <- file$do_operation("versions")
  this_version <- v$value[[1]]
  structure(this_version$id,
            modified = as.POSIXct(as.POSIXct(gsub("[TZ]", "", this_version$lastModifiedDateTime),
                                             tz = "UTC"),
                                  tz = "Europe/Amsterdam"),
            size = structure(this_version$size, class = "object_size"))
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

find_channel <- function(path, account = teams_connect()) {
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
