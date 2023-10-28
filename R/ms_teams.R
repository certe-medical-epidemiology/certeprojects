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
#' These functions use the connection to Microsoft Teams set up with [connect_teams()].
#' @param ... arguments passed on to [get_microsoft365_token()]
#' @param account a Microsoft 365 account to use for looking up properties. This has to be an object as returned by [connect_teams()] or [Microsoft365R::get_team()].
#' @inheritParams connect
#' @param projects_channel_id Teams channel ID of the projects
#' @rdname teams
#' @name teams
#' @importFrom Microsoft365R get_team ms_team ms_drive_item ms_channel
#' @export
#' @examples 
#' \dontrun{
#' # PROJECT-RELATED ------------------------------------------------------
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
#' teams_open("test.xlsx", "My Channel")
#' teams_open("my channel/test.xlsx") # shorter version, tries to find channel
#' 
#' 
#' # PROJECT-UNRELATED ----------------------------------------------------
#' 
#' # by not specifying a remote location, a file picker will show in the console:
#' teams_download_file()
#' teams_download_folder("MyTeamName/MyChannelName/MySubFolder/")
#' 
#' teams_upload_file("myfile.docx", full_teams_path = "MyTeamName/MyChannelName/MySubFolder/")
#' 
#' # also supports data frames, they will be saved as RDS
#' mtcars |> 
#'   teams_upload_file("MyTeamName/MyChannelName/MySubFolder/")
#' }
teams_projects_channel <- function(projects_channel_id = read_secret("teams.projects.channel_id"),
                                   overwrite = FALSE,
                                   account = connect_teams()) {
  if (isTRUE(overwrite) || is.null(pkg_env$teams_project_folder)) {
    # try to get the project channel from callr
    conn <- tryCatch(pkg_env$callr$get_result()$teams_project_folder, error = function(e) NULL)
    if (!isTRUE(overwrite) && !is.null(conn) && inherits(conn, "ms_drive_item")) {
      pkg_env$teams_project_folder <- conn
      pkg_env$teams_project_folder_from_callr <- TRUE
    } else {
      message("Retrieving Teams channel...", appendLF = FALSE)
      # this manual way is by far the fastest:
      folder <- tryCatch(ms_drive_item$new(token = account$token,
                                           tenant = account$tenant,
                                           properties = ms_channel$new(token = account$token,
                                                                       tenant = account$tenant,
                                                                       properties = account$do_operation(file.path("channels/", projects_channel_id)),
                                                                       team_id = get_azure_property(account, "id"))$do_operation("filesFolder")),
                         error = function(e) stop("Could not retrieve Channel list: ", e$message))
      pkg_env$teams_project_folder <- folder
      pkg_env$teams_project_folder_from_callr <- FALSE
      message("OK.")
    }
  }
  return(pkg_env$teams_project_folder)
}

#' @rdname teams
#' @param task any task title, task ID, or [`ms_plan_task`][Microsoft365R::ms_plan_task] object (e.g. from [planner_task_find()])
#' @param channel a Teams folder object. This has to be an object as returned by [teams_projects_channel()].
#' @param planner a Microsoft 365 account for Planner. This has to be an object as returned by [connect_planner()].
#' @details
#' The [teams_new_project()] function:
#' 1. Checks if there is a Planner task with the correct task title
#' 2. Creates a new folder in Teams in the projects channel
#' 3. Updates the task to contain the project folder URL as an attachment
#' 
#' @export
teams_new_project <- function(task, channel = teams_projects_channel(), planner = connect_planner()) {
  # validate that the task exists
  task <- planner_task_find(task, account = planner)
  task_title <- get_azure_property(task, "title")
  # create the folder
  if (task_title %in% channel$list_files()$name) {
    message("Project folder already exists, skipping.")
  } else {
    channel$create_folder(path = task_title)
  }
  # add link to Planner task
  link <- c(Projectmap = channel$get_item(task_title) |> get_azure_property("webUrl"))
  planner_task_update(task = task_title, attachment_urls = link)
}

#' @rdname teams
#' @export
teams_browse_project <- function(task, channel = teams_projects_channel(), planner = connect_planner()) {
  # validate that the task exists
  task <- planner_task_find(task, account = planner)
  # open in the browser
  channel$get_item(get_azure_property(task, "title"))$open()
}

#' @rdname teams
#' @importFrom Microsoft365R ms_drive ms_drive_item
#' @details
#' The [teams_download_project_file()] function will download the given project file to a temporary location, and will return the path of this location. This makes it possible to use [source()], [rmarkdown::render()] or [quarto::quarto_render()] using the [teams_download_project_file()] function as input.
#' @export
teams_download_project_file <- function(file, task, channel = teams_projects_channel(), planner = connect_planner()) {
  task <- planner_task_find(task, account = planner)
  file_teams <- teams_get_project_file(file = file, task = task, channel = channel, planner = planner)
  
  # download to temporary file
  file_local <- paste0(tempdir(), "/", get_azure_property(file_teams, "name"))
  try(unlink(file_local, force = TRUE), silent = TRUE)
  
  tryCatch(
    file_teams$download(dest = file_local, overwrite = TRUE),
    error = function(e) stop("Error while downloading file '", get_azure_property(file_teams, "name"), "' from '", get_azure_property(task, "title"), ":\n", e$message))
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
teams_get_project_file <- function(file, task, channel = teams_projects_channel(), planner = connect_planner()) {
  # validate that the task exists
  task <- planner_task_find(task, account = planner)
  file_teams <- channel$get_item(get_azure_property(task, "title"))$list_items()
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
                          title = paste0("Files found in ", get_azure_property(task, "title"), " (0 to cancel):\n"))
    if (choice == 0) {
      return(NA_character_)
    }
    filename <- files_found[choice]
  } else {
    message("Files found:\n  - ", paste(files_found, collapse = "\n  - "),
            "\nSelecting first match.")
    filename <- files_found[1L]
  }
  channel$get_item(get_azure_property(task, "title"))$get_item(filename)
}

#' @rdname teams
#' @importFrom rstudioapi navigateToFile showDialog
#' @export
teams_open_project_analysis_file <- function(task, channel = teams_projects_channel(), planner = connect_planner()) {
  task <- planner_task_find(task, account = planner)
  path <- teams_download_project_file(file = ".*[.](R|qmd|Rmd|sql|txt|csv|tsv|css|ya?ml|js)$",
                                      task = task, channel = channel, planner = planner)
  if (is.na(path)) {
    stop("No syntax files found in project '", get_azure_property(task, "title"), "'")
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
                                      planner = connect_planner()) {
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
    task_title <- get_azure_property(task, "title")
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
        url <- paste0("\033]8;;", get_azure_property(new_file, "webUrl"), "\a", "Teams-link", "\033]8;;\a")
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
                                    planner = connect_planner()) {
  # validate that the task exists
  task <- planner_task_find(task, account = planner)
  # open in the browser
  channel$get_item(get_azure_property(task, "title"))$get_item(file)$open()
}

#' @rdname teams
#' @param files the files to upload
#' @export
teams_upload_project_file <- function(files,
                                      task,
                                      channel = teams_projects_channel(),
                                      planner = connect_planner()) {
  # validate that the task exists
  task <- planner_task_find(task, account = planner)
  task_title <- get_azure_property(task, "title")
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

#' @param full_teams_path a full path in Teams, **including the Team name and the channel name**. Leave blank to use interactive mode, which allows file/folder picking from a list in the console.
#' @param destination_dir a folder to download the file or folder to, defaults to the current working directory.
#' @details The [teams_download_file()] and [teams_download_folder()] functions use [pick_teams_item()] to select a file or folder, after which they will be downloaded to the destination folder.
#' @importFrom AzureGraph create_graph_login
#' @rdname teams
#' @export
teams_download_file <- function(full_teams_path = NULL,
                                account = connect_teams(),
                                destination_dir = getwd(),
                                overwrite = FALSE) {
  if (!is.null(full_teams_path) && tools::file_ext(full_teams_path) == "") {
    stop("'", full_teams_path, "' is not a file, but a folder")
  }
  item <- pick_teams_item(full_teams_path = full_teams_path,
                          account = account,
                          only_folders = FALSE)
  if (is.null(item)) {
    return(invisible())
  }
  token <- create_graph_login(token = account$token)
  message("Downloading file '", item$item_name, "' to ", destination_dir, "...", appendLF = FALSE)
  drive_item <- picked_teams_item_2_drive_item(token = token, item = item)
  drive_item$download(dest = paste0(destination_dir, "/", item$item_name),
                      overwrite = overwrite,
                      recursive = FALSE,
                      parallel = FALSE)
  message("OK.")
  return(invisible(paste0(destination_dir, "/", item$item_name)))
}


#' @param recursive download/upload all files within the folder
#' @importFrom AzureGraph create_graph_login
#' @rdname teams
#' @export
teams_download_folder <- function(full_teams_path = NULL,
                                  account = connect_teams(),
                                  destination_dir = getwd(),
                                  recursive = TRUE,
                                  overwrite = FALSE) {
  if (!is.null(full_teams_path) && tools::file_ext(full_teams_path) != "") {
    stop("'", full_teams_path, "' is not a folder, but a file")
  }
  item <- pick_teams_item(full_teams_path = full_teams_path,
                          account = account,
                          only_folders = TRUE)
  if (is.null(item)) {
    return(invisible())
  }
  token <- create_graph_login(token = account$token)
  message("Downloading folder '", item$item_name, "' to ", destination_dir, "...", appendLF = FALSE)
  drive_item <- picked_teams_item_2_drive_item(token = token, item = item)
  drive_item$download(dest = paste0(destination_dir, "/", item$item_name),
                      overwrite = overwrite,
                      recursive = recursive,
                      parallel = "parallel" %in% rownames(utils::installed.packages()))
  message("OK.")
  return(invisible(paste0(destination_dir, "/", item$item_name)))
}

#' @param file_path local path of the file to upload. Can also be an \R object to save it as RDS to Teams.
#' @param file_name a file name to use if `file_path` is an \R object
#' @importFrom certestyle format2
#' @importFrom AzureGraph create_graph_login
#' @details The [teams_upload_file()] and [teams_upload_folder()] functions use [pick_teams_item()] to select the destination folder on Teams. **Notice** that these upload functions have not `overwrite` argument - Microsoft365R does not support them since overwrite means that a new file version will be created on Teams.
#' @rdname teams
#' @export
teams_upload_file <- function(file_path,
                              full_teams_path = NULL,
                              account = connect_teams(),
                              file_name = NULL) {
  if (is.character(file_path) && tools::file_ext(file_path) == "") {
    stop("'", file_path, "' is not a file, but a folder")
  }
  if (!is.null(full_teams_path) && tools::file_ext(full_teams_path) != "") {
    stop("'", full_teams_path, "' is not a folder, but a file")
  }
  item <- pick_teams_item(full_teams_path = full_teams_path,
                          account = account,
                          only_folders = TRUE)
  if (is.null(item)) {
    return(invisible())
  }
  
  # save R object if given as file_path
  if (!is.character(file_path) || (exists(file_path) && !file.exists(file_path))) {
    if (!is.null(file_name)) {
      nm <- basename(tools::file_path_sans_ext(file_name))
    } else {
      nm <- deparse(substitute(file_path))
      if (nm == ".") {
        nm <- paste0("r_object_", format2(Sys.time(), "hhmmss"))
      }
    }
    nm_new <- trimws(gsub("[^a-zA-Z0-9_]+", " ", nm))
    if (nm %unlike% "^[a-zA-Z0-9_ ]+$") {
      warning("Setting file name to '", nm_new, ".rds' - Preferably set `file_name` using valid file name characters.", immediate. = TRUE)
    }
    # save to temp dir
    tmp <- paste0(tempdir(), "/", nm_new, ".rds")
    message("Saving R object to ", tmp, "...", appendLF = FALSE)
    saveRDS(file_path, tmp, version = 2, compress = "xz")
    message("OK (", format2(structure(file.size(tmp), class = "object_size"), decimal.mark = "."), ").")
    file_path <- tmp
  }
  
  # upload the file
  token <- create_graph_login(token = account$token)
  message("Uploading file '", basename(file_path), "' to ", item$full_path, "...", appendLF = FALSE)
  drive_item <- picked_teams_item_2_drive_item(token = token, item = item)
  drive_item$upload(src = file_path,
                    dest = basename(file_path),
                    recursive = FALSE,
                    parallel = FALSE)
  message("OK.")
}

#' @param folder_path local path of the folder to upload
#' @importFrom AzureGraph create_graph_login
#' @rdname teams
#' @export
teams_upload_folder <- function(folder_path,
                                full_teams_path = NULL,
                                account = connect_teams(),
                                recursive = TRUE) {
  if (tools::file_ext(folder_path) != "") {
    stop("'", folder_path, "' is not a folder, but a file")
  }
  if (!is.null(full_teams_path) && tools::file_ext(full_teams_path) != "") {
    stop("'", full_teams_path, "' is not a folder, but a file")
  }
  item <- pick_teams_item(full_teams_path = full_teams_path,
                          account = account,
                          only_folders = TRUE)
  if (is.null(item)) {
    return(invisible())
  }
  token <- create_graph_login(token = account$token)
  message("Uploading folder '", basename(folder_path), "' to ", item$full_path, "...", appendLF = FALSE)
  drive_item <- picked_teams_item_2_drive_item(token = token, item = item)
  drive_item$upload(src = folder_path,
                    dest = basename(folder_path),
                    recursive = recursive,
                    parallel = "parallel" %in% rownames(utils::installed.packages()))
  message("OK.")
}

#' @importFrom certestyle format2 font_bold
#' @importFrom AzureGraph create_graph_login
#' @param only_folders only show folders, not files
#' @details The [pick_teams_item()] function provides an interactive way to select a file in any Team, any channel. It returns a list with the properties `group_id`, `is_private`, `channel_id`, `item_id`, `item_name`, `item_path`, and `full_path` of the Team item.
#' @rdname teams
#' @export
pick_teams_item <- function(full_teams_path = NULL,
                            account = connect_teams(),
                            only_folders = FALSE) {
  if (!is_valid_teams(account)) {
    stop("No valid Teams account")
  }
  
  item_type <- ifelse(only_folders, "folder", "file")
  
  # non-interactive mode
  if (!is.null(full_teams_path)) {
    path_parts <- strsplit(full_teams_path, split = "/", fixed = TRUE)[[1]]
    message("Retrieving Team '", path_parts[1], "'...", appendLF = FALSE)
    login <- create_graph_login(token = account$token)
    group <- login$get_group(name = path_parts[1])
    drive <- group$get_drive()
    if (path_parts[2] %in% drive$list_files()$name) {
      # not a private channel
      is_private <- FALSE
      item <- drive$get_item(paste0(path_parts[-1], collapse = "/"))
    } else {
      # a private channel
      is_private <- TRUE
      message("OK.")
      message("Retrieving private channel '", path_parts[2], "'...", appendLF = FALSE)
      team <- group$get_team()
      channel <- team$get_channel(channel_name = path_parts[2])
      folder <- channel$get_folder()
      item <- folder$get_item(paste0(path_parts[-c(1, 2)], collapse = "/"))
    }
    message("OK.")
    return(list(group_id = group$properties$id,
                is_private = is_private,
                channel_id = ifelse(is_private, channel$properties$id, NA_character_),
                item_id = item$properties$id,
                item_name = item$properties$name,
                item_path = ifelse(is_private,
                                   paste0(path_parts[-c(1, 2)], collapse = "/"),
                                   paste0(path_parts[-1], collapse = "/")),
                full_path = full_teams_path))
    
  } else if (!interactive()) {
    stop(tools::toTitleCase(item_type), " picking only works in interactive mode. Set `full_teams_path` in non-interactive mode")
  }
  
  # interactive mode
  message("Retrieving list of Teams within ", account$token$tenant, "...", appendLF = FALSE)
  groups_names <- get_teams_groups_from_env(account)
  message("OK, n = ", length(groups_names), ".")
  continue <- FALSE
  while (!isTRUE(continue)) {
    searchterm <- readline("Search for Team name, allows regex - leave blank for 'Medische Epidemiologie': ")
    if (trimws(searchterm) == "") {
      found_groups <- which(groups_names %like% "Medische Epidemiologie")
      continue <- TRUE
      next
    }
    found_groups <- which(groups_names %like% searchterm)
    if (length(found_groups) == 0) {
      levensthein <- as.double(utils::adist(searchterm, groups_names, counts = FALSE, ignore.case = TRUE))
      found_groups <- order(levensthein)[1]
    }
    continue <- utils::askYesNo(paste0("Found Team '", groups_names[found_groups[1]], "'. Continue?"))
    if (is.na(continue)) {
      # has chosen 'cancel'
      return(invisible())
    }
  }
  get_icon <- function(ff) {
    case_when(ff$isdir ~ "\U1F5C2", # Folder
              ff$name %like% "xlsx?$" ~ "\U1F4CA", # Spreadsheet
              ff$name %like% "(docx?|pdf)$" ~ "\U1F4DD", # Document
              ff$name %like% "pptx?$" ~ "\U1F4C9", # Presentation
              ff$name %like% "csv$" ~ "\U1F9FE", # Receipt
              ff$name %like% "zip$" ~ "\U1F4E6", # Package
              ff$name %like% "(jpe?g|bmp|png|gif)$" ~ "\U1F5BC", # Image
              ff$name %like% "(eml|msg)$" ~ "\U2709\UFE0F", # Envelope with variant selector
              TRUE ~ "\U1F4C4") # Document
  }
  format_filesize <- function(x) {
    out <- format2(structure(x, class = "object_size"), decimal.mark = ".")
    out[is.na(x)] <- "\U1F512" # Padlock
    out
  }
  message("Retrieving ", item_type, " list...", appendLF = FALSE)
  group_name <- groups_names[found_groups[1]]
  group <- get_group(group_name, account = account)
  drive <- group$get_drive()
  files <- drive$list_files()
  files <- files[which(files$isdir), ] # in channel view, only show folders
  files$private <- FALSE
  team <- group$get_team()
  private_channels <- try_with_retry(team$list_channels())
  private_channels <- private_channels[which(vapply(FUN.VALUE = character(1), private_channels, function(cn) cn$properties$membershipType) == "private")]
  
  if (length(private_channels) > 0) {
    private_files <- data.frame(name = vapply(FUN.VALUE = character(1), private_channels, function(cn) cn$properties$displayName),
                                size = NA_real_,
                                isdir = TRUE,
                                id = vapply(FUN.VALUE = character(1), private_channels, function(cn) cn$properties$id),
                                private = TRUE)
    files <- rbind(files, private_files)
  }
  # order on name
  files_root <- files[order(tolower(trimws(gsub("[^a-zA-Z0-9 /.-]", "", files$name)))), ]
  file_choices_root <- paste0(font_bold(trimws(files_root$name), collapse = NULL),
                              " (", format_filesize(files_root$size), ")")
  message("OK.")
  cat(font_bold("Current folder:\n\n\U1F465 "), group_name, "\n\n", sep = "")
  picked <- utils::menu(choices = file_choices_root,
                        graphics = FALSE,
                        title = font_bold(paste0("Choose a channel (0 to Cancel):")))
  if (picked == 0) {
    # has chosen Cancel
    return(invisible())
  }
  item_root <- files_root[picked, ]
  item <- item_root
  if (item_root$private) {
    is_private <- TRUE
    channel <- team$get_channel(channel_id = item_root$id)
  } else {
    is_private <- FALSE
    channel <- NA
  }
  dive_levels <- item_root$name
  has_picked <- !item_root$isdir
  
  while (!has_picked) {
    cat(font_bold("Current folder:\n\n\U1F465 "), group_name, sep = "")
    if (length(dive_levels) > 0) {
      cat(paste0("\n", strrep("   ", seq_len(length(dive_levels))), "\U21B3 ", dive_levels), "\n\n")
    } else {
      cat("\n\n")
    }
    item_parent <- item
    if (length(dive_levels) == 0) {
      # we are in the root again
      is_root <- TRUE
      files <- files_root
      file_choices <- file_choices_root
      searchpath <- ""
      files_total_size <- sum(files$size, na.rm = TRUE)
    } else {
      # we are still in some subfolder
      is_root <- FALSE
      if (item$private == TRUE) {
        searchpath <- paste0(dive_levels[-1], collapse = "/")
        files <- channel$list_files(searchpath)
      } else {
        searchpath <- paste0(dive_levels, collapse = "/")
        files <- drive$list_files(searchpath)
      }
      files <- files[order(!files$isdir, tolower(trimws(gsub("[^a-zA-Z0-9 /.-]", "", files$name)))), ]
      files$private <- item$private
      files_total_size <- sum(files$size, na.rm = TRUE)
      if (only_folders == TRUE) {
        files <- files[which(files$isdir), ]
      }
      if (NROW(files) == 0) {
        file_choices <- character(0)
        files_total_size <- item$size
      } else {
        file_choices <- paste0(get_icon(files), " ",
                               trimws(files$name),
                               " (", format_filesize(files$size), ")")
      }
    }
    if (only_folders == TRUE && length(dive_levels) > 0) {
      file_choices <- c(file_choices, paste0("\U21AA Select this folder (", format_filesize(files_total_size), ")"))
    }
    if (length(dive_levels) > 0) {
      file_choices <- c(file_choices, "\U21A9 Go back to previous folder...")
    }
    picked <- utils::menu(choices = file_choices,
                          graphics = FALSE,
                          title = font_bold(paste0("Choose a ",
                                                   ifelse(is_root, "channel",
                                                          ifelse(only_folders, "folder", "file or folder ")), " (0 to Cancel):")))
    if (picked == 0) {
      # has chosen Cancel
      return(invisible())
    } else if (file_choices[picked] == "\U21A9 Go back to previous folder...") {
      # return one level
      dive_levels <- dive_levels[seq_len(length(dive_levels) - 1)]
      if (item$id == item_parent$id) {
        item_parent <- item_root
      }
      has_picked <- FALSE
    } else if (file_choices[picked] %like% "\U21AA Select this folder") {
      # if (item$id == item_parent$id) {
      #   item_parent <- item_root
      # }
      item <- item_parent
      has_picked <- TRUE
    } else {
      item <- files[picked, ]
      dive_levels <- c(dive_levels, item$name)
      has_picked <- !item$isdir && !only_folders
    }
  }
  
  list(group_id = group$properties$id,
       is_private = item$private,
       channel_id = ifelse(item$private, channel$properties$id, NA_character_),
       item_id = item$id,
       item_name = item$name,
       item_path = ifelse(item$private,  paste0(dive_levels[-1], collapse = "/"), paste0(dive_levels, collapse = "/")),
       full_path = paste0(group_name, "/", paste0(dive_levels, collapse = "/")))
}

#' @rdname teams
#' @export
teams_name <- function(account = connect_teams()) {
  get_teams_property(account, "displayName")
}

#' @rdname teams
#' @param plain return as plain names, not as `Azure` objects
#' @export
teams_channels_list <- function(account = connect_teams(), plain = TRUE) {
  if (!is_valid_teams(account)) {
    return(NA_character_)
  }
  channels <- try_with_retry(account$list_channels())
  if (plain == FALSE) {
    channels
  } else {
    sort(get_azure_property(channels, "displayName"))
  }
}

#' @rdname teams
#' @export
teams_view_sharepoint <- function(channel, account = connect_teams()) {
  if (!is_valid_teams(account)) {
    return(NA_character_)
  }
  channel <- find_channel(path = channel, account = account)
  if (is.na(channel)) {
    return(NA_character_)
  } else {
    try_with_retry(
      account$
        get_channel(channel)$
        get_folder()$
        open()
    )
  }
}

#' @param body text of the message
#' @param content_type type of content, must be "text" or "html"
#' @param attachments vector of file locations of attachments to add to the message
#' @details [teams_send_message()] can also take a [data.frame], which will be converted to HTML with [plain_html_table()][certestyle::plain_html_table()]. If the input is a vector length > 1, the input will be collapsed with linebreaks.
#' @importFrom certestyle plain_html_table
#' @rdname teams
#' @export
teams_send_message <- function(body,
                               channel,
                               content_type = c("text", "html"),
                               attachments = NULL,
                               account = connect_teams()) {
  if (!is_valid_teams(account)) {
    stop("No valid Teams account")
  }
  channel <- find_channel(path = channel, account = account)
  
  if (is.data.frame(body)) {
    body <- plain_html_table(body)
    content_type <- "html"
  } else {
    body <- as.character(body)
    if (length(body) > 1) {
      body <- paste(body, collapse = "<br>")
      content_type <- "html"
    }
  }
  
  invisible(
    try_with_retry(
      account$
        get_channel(channel)$
        send_message(body = body,
                     content_type = content_type[1],
                     attachments = attachments)
    )
  )
}

# #' @param local_pah file location on the local system, can also be a [data.frame] which will then be saved locally to the temp folder first
# #' @rdname teams
# #' @export
# teams_upload <- function(local_path,
#                          teams_path = basename(local_path),
#                          project_number = project_get_current_id(ask = FALSE),
#                          channel = NULL,
#                          account = connect_teams()) {
#   if (!is_valid_teams(account)) {
#     stop("No valid Teams account")
#   }
#   if (is.data.frame(local_path)) {
#     message("Saving data set as RDS file to temporary location...", appendLF = FALSE)
#     filename <- deparse(substitute(local_path))
#     if (filename == ".") {
#       filename <- paste0("data_", concat(sample(c(letters[seq_len(6)], 0:9), size = 8, replace = TRUE)))
#     }
#     tmp <- paste0(tempdir(), "/", filename, ".rds")
#     saveRDS(object = local_path, file = tmp, compress = "xz")
#     local_path <- tmp
#     teams_path <- paste0(filename, ".rds")
#     message("OK.")
#   }
#   if (!is.null(project_number)) {
#     local_path <- project_set_file(filename = local_path, project_number = project_number)
#   }
#   if (!file.exists(local_path)) {
#     stop("Path not found: ", local_path)
#   }
#   if (is.null(channel)) {
#     # find channel based on teams path
#     channel <- find_channel(path = teams_path, account = account)
#     if (!is.na(channel) && teams_path %like% "[/]") {
#       # a channel was found, so remove first part of name from teams_path
#       teams_path <- gsub("^(.*?)/(.*)", "\\2", teams_path)
#     } else if (is.na(channel)) {
#       stop("No valid channel set")
#     }
#   }
#   message("Uploading '", local_path,
#           "' to channel '", channel,
#           "' as '", teams_path, "'...", appendLF = FALSE)
#   tryCatch({
#     account$
#       get_channel(channel)$
#       upload_file(src = local_path,
#                   dest = teams_path)
#     message("OK.")
#   }, error = function(e) message("ERROR.\n", e$message))
#   
#   # remove temp file
#   try(unlink(tmp), silent = TRUE)
# }

# #' @inheritParams project_properties
# #' @rdname teams 
# #' @export
# teams_download <- function(teams_path,
#                            local_path = basename(teams_path),
#                            project_number = project_get_current_id(ask = FALSE),
#                            channel = NULL,
#                            account = connect_teams()) {
#   if (!is_valid_teams(account)) {
#     stop("No valid Teams account")
#   }
#   if (is.null(channel)) {
#     # find channel based on teams path
#     channel <- find_channel(path = teams_path, account = account)
#     if (!is.na(channel) && teams_path %like% "[/]") {
#       # a channel was found, so remove first part of name from teams_path
#       teams_path <- gsub("^(.*?)/(.*)", "\\2", teams_path)
#     } else if (is.na(channel)) {
#       stop("No valid channel set")
#     }
#   }
#   if (!is.null(project_number)) {
#     local_path <- project_set_file(filename = local_path, project_number = project_number)
#   }
#   message("Downloading Teams file '", teams_path,
#           "' from channel '", channel,
#           "' as '", local_path, "'...", appendLF = FALSE)
#   tryCatch(
#     account$
#       get_channel(channel)$
#       download_file(src = teams_path,
#                     dest = local_path,
#                     overwrite = TRUE),
#     error = function(e) message("ERROR.\n", e$message))
#   if (file.exists(local_path)) {
#     message("OK.")
#     return(invisible(local_path))
#   } else {
#     return(NULL)
#   }
# }

# #' @inheritParams project_properties
# #' @rdname teams 
# #' @export
# teams_import <- function(teams_path,
#                          project_number = project_get_current_id(ask = FALSE),
#                          channel = NULL,
#                          account = connect_teams()) {
#   suppressMessages(
#     tempfile <- teams_download(teams_path = teams_path,
#                                local_path = paste0(tempdir(), "/", basename(teams_path)),
#                                project_number = project_number,
#                                channel = channel,
#                                account = account)
#   )
#   certetoolbox::import(tempfile)
# }

#' @param teams_path file location in Microsoft Teams, may also contain the channel name if `channel` is `NULL`, e.g., `teams_path = "channel name/test.xlsx"`
#' @rdname teams
#' @export
teams_open <- function(teams_path, channel = NULL, account = connect_teams()) {
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
  try_with_retry(
    account$
      get_channel(channel)$
      get_folder()$
      get_item(teams_path)$
      open()
  )
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
                           account = connect_teams()) {
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
  try_with_retry(
    account$
      get_channel(channel)$
      get_folder()$
      get_item(teams_path)$
      create_share_link(type = share_type[1L],
                        expiry = expire_after,
                        password = password)
  )
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

find_channel <- function(path, account = connect_teams()) {
  channels <- teams_channels_list(account = account)
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

try_with_retry <- function(expr) {
  tryCatch(expr,
           error = function(e) {
             Sys.sleep(2)
             expr
           })
}

get_group <- function(group_name, account) {
  create_graph_login(token = account$token)$get_group(name = group_name)
}

#' @importFrom AzureGraph create_graph_login
get_teams_groups_from_env <- function(account) {
  if (is.null(pkg_env$teams_groups)) {
    # check if callr found it, then use it
    pkg_env$teams_groups <- tryCatch(pkg_env$callr$get_result()$teams_groups, error = function(e) NULL)
    if (is.null(pkg_env$teams_groups)) {
      login <- create_graph_login(token = account$token)
      pkg_env$teams_groups <- get_azure_property(login$list_groups(), "displayName")
    }
  }
  return(pkg_env$teams_groups)
}

picked_teams_item_2_drive_item <- function(token, item) {
  if (item$is_private) {
    team <- token$get_team(item$group_id)
    channel <- team$get_channel(channel_id = item$channel_id)
    folder <- channel$get_folder()
    return(folder$get_item(item$item_path))
  } else {
    group <- token$get_group(item$group_id)
    drive <- group$get_drive()
    return(drive$get_item(itemid = item$item_id))
  }
}
