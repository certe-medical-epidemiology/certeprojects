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

#' Project Properties
#' 
#' Retrieve project properties, such as the title, folder location and project number. It retrieves folder and file info from SharePoint, by using [teams_get_project_folders()]
#' @param ask logical to indicate whether the project number should always be asked. The default, `NULL`, will show a popup in [interactive][interactive()] \R sessions, allowing to search for projects. In non-interactive sessions, such as in Quarto and R Markdown, it will use the current [working directory][getwd()] to determine the project number.
#' @param project_number Planner project number
#' @param filename filename to set or get, case-insensitive, and can also be a [regular expression][base::regex]
#' @param foldername foldername to set
#' @param fixed [logical] to turn off regular expressions
#' @name project_properties
#' @details [project_get_current_id()] uses [planner_task_search()] to find a specific project based on any search string. 
#' @rdname project_properties
#' @importFrom rstudioapi getSourceEditorContext showPrompt isAvailable
#' @export
project_get_current_id <- function(ask = NULL, account = connect_teams()) {
  # first try project number from full file location:
  # /folder/p123 Name.Rmd
  # /p123 folder/Name.Rmd
  fix_id <- function(id) {
    if (isTRUE(suppressWarnings(id %like% "[^0-9]"))) {
      id <- gsub("[^0-9]", "", id)
    }
    if (all(is.na(id) | length(id) == 0) || identical(id, "")) {
      NULL
    } else {
      id
    }
  }
  asked <- FALSE
  path <- tryCatch(tools::file_path_as_absolute(getSourceEditorContext()$path), error = function(e) NULL)
  if (is.null(path)) {
    path <- full_path_to_currently_sourced_project_file()
    if (length(path) == 0 || path %unlike% "p[0-9]+") {
      path <- NULL
    }
    if (is.null(path) && interactive() && (is.null(ask))) {
      # still NULL
      search_term <- showPrompt("Zoekterm taak", "Zoekterm om naar een taak te zoeken:", "")
      if (is_empty(search_term)) return(invisible(FALSE))
      id <- search_project_folder(search_term = search_term, account = account)
      if (is_empty(id)) {
        return(NULL)
      }
      return(fix_id(id))
    } else if (is.null(path)) {
      # for when ask == FALSE, and for Quarto and R Markdown
      path <- getwd()
    }
  }
  parts <- tryCatch(unlist(strsplit(as.character(path), "[^a-zA-Z0-9]")), error = function(e) NULL)
  if (is.null(parts)) {
    return(NULL)
  }
  id <- parts[parts %like% "^p[0-9]+$"][1]
  if (all(length(id) == 0 | is.na(id)) && interactive() && is.null(ask)) {
    search_term <- showPrompt("Zoekterm taak", "Zoekterm om naar een taak te zoeken:", "")
    if (is_empty(search_term)) return(invisible(FALSE))
    id <- search_project_folder(search_term = search_term, account = account)
    if (is_empty(id)) {
      return(NULL)
    }
    asked <- TRUE
  }
  
  if (identical(ask, TRUE) && asked == FALSE) {
    if (interactive()) {
      search_term <- showPrompt("Zoekterm taak", "Zoekterm om naar een taak te zoeken:",
                                default = ifelse(!is.na(id) & length(id) > 0,
                                                 paste0("p", fix_id(id)),
                                                 ""))
      if (is_empty(search_term)) return(invisible(FALSE))
    } else {
      search_term <- ifelse(!is.na(id) & length(id) > 0,
                            paste0("p", fix_id(id)),
                            search_term)
    }
    id <- search_project_folder(search_term = search_term,
                                account = account)
    if (is_empty(id)) {
      return(NULL)
    }
  }
  fix_id(id)
}

search_project_folder <- function(search_term, as_title = FALSE, account = connect_teams()) {
  if (search_term %like% "^p?[0-9]+") {
    search_term <- paste0("p", gsub("p", "", search_term), "$")
  }
  folders <- teams_get_project_folders(account = account)
  hit <- folders[folders %like% search_term]
  if (length(hit) > 0) {
    if (as_title == TRUE) {
      return(hit[1])
    } else {
      return(as.integer(gsub(".*p([0-9]+).*", "\\1", hit[1])))
    }
  } else {
    return(NULL)
  }
}

#' @rdname project_properties
#' @details [project_identifier()] generates the project identifier for print on reports and in mails: a combination of the currently logged in user (in your case: '\Sexpr{Sys.info()\["user"\]}'), the current date/time (format: YYMMDDHHMM), and the project number. If the project number is not available, it will only return the current user and date/time (format: YYMMDDHHMM).
#' @importFrom certestyle format2
#' @export
#' @examples
#' project_identifier(123)
project_identifier <- function(project_number = project_get_current_id()) {
  user_datetime <- paste0(Sys.info()["user"], "-", format2(Sys.time(), "yymmddHHMM"))
  if (is_empty(project_number)) {
    user_datetime
  } else {
    paste0(user_datetime, "-", project_number)
  }
}

#' @rdname project_properties
#' @inheritParams teams_get_project_folders
#' @export
project_get_folder <- function(project_number = project_get_current_id(),
                               account = connect_teams()) {
  if (is_empty(project_number)) {
    return(NA_character_)
  }
  if (!is.null(attributes(project_number)$task)) {
    # when using project_get_current_id(), the result comes from planner_retrieve_project_id() which contains the task as attribute
    attributes(project_number)$task |> get_azure_property("title")
  } else {
    search_project_folder(project_number, as_title = TRUE, account = account)
  }
}

#' @rdname project_properties
#' @export
project_get_title <- function(project_number = project_get_current_id(),
                              account = connect_teams()) {
  if (is_empty(project_number)) {
    return(NA_character_)
  }
  project_number <- gsub("[^0-9]", "", project_number)
  trimws(gsub("-? ?p[0-9]+/?$", "", project_get_folder(project_number = project_number, account = account)))
}

#' @rdname project_properties
#' @importFrom certestyle format2 font_bold font_blue
#' @export
project_get_file <- function(filename = ".*",
                             project_number = project_get_current_id(),
                             fixed = FALSE,
                             account = connect_teams()) {
  channel <- teams_projects_channel(account = account)
  project_number <- gsub("[^0-9]", "", project_number)
  folder <- project_get_folder(project_number = project_number, account = account)
  filename <- filename[1L]
  if (isTRUE(fixed)) {
    filename <- paste0("^", filename, "$")
  }
  if (!is.null(folder)) {
    # regex, try to find file
    files <- channel$get_item(folder)$list_items(info = "all")
    # sort on last changed (desc):
    files$lastModifiedDateTime <- as.POSIXct(as.POSIXct(gsub("T", " ", files$lastModifiedDateTime), tz = "UTC"), tz = "Europe/Amsterdam")
    files <- files[order(files$lastModifiedDateTime, decreasing = TRUE), ]
    
    files <- files[files$name %like% filename & files$name %unlike% "^~[$]", ]
    files_names <- files$name
    
    if (length(files_names) == 0) {
      warning("No files found")
      return(NA_character_)
    }
    if (length(files_names) > 0) {
      if (length(files_names) > 1) {
        if (interactive()) {
          choice <- utils::menu(choices = paste0(font_bold(files_names, collapse = NULL),
                                                 " (last changed: ",
                                                 font_blue(format2(files$lastModifiedDateTime, "yyyy-mm-dd HH:MM"), collapse = NULL),
                                                 ", ", font_blue(files$lastModifiedBy$user$displayName, collapse = NULL),
                                                 ")"),
                                title = paste0("Sharepoint files found in '", folder, "', sorted on last changed (0 to cancel):"))
          if (choice == 0) {
            return(NA_character_)
          }
          filename <- files_names[choice]
        } else {
          message("Files found:\n  - ", paste(files_names, collapse = "\n  - "),
                  "\nSelecting first match.")
          filename <- files_names[1L]
        }
      } else {
        filename <- files_names
      }
    }
  } else {
    return(NA_character_)
  }
  file.path(folder, filename)
}

#' @rdname project_properties
#' @export
project_set_file <- function(filename,
                             project_number = project_get_current_id(),
                             account = connect_teams()) {
  project_number <- gsub("[^0-9]", "", project_number)
  folder <- project_get_folder(project_number = project_number, account = account)
  filename <- filename[1L]
  if (!is.null(folder)) {
    file.path(folder, filename)
  } else {
    file.path(dirname(filename), filename)
  }
}

#' @rdname project_properties
#' @details [project_set_folder()] will create the folder if it does not exist.
#' @export
project_set_folder <- function(foldername,
                               project_number = project_get_current_id(),
                               account = connect_teams()) {
  project_number <- gsub("[^0-9]", "", project_number)
  folder <- project_get_folder(project_number = project_number, account = account)
  foldername <- foldername[1L]
  if (!is.na(folder)) {
    foldername <- file.path(folder, foldername)
  }
  if (!dir.exists(foldername)) {
    invisible(dir.create(foldername))
  }
  paste0(foldername, "/")
}

#' @rdname project_properties
#' @importFrom rstudioapi navigateToFile
#' @export
project_open_analysis_file <- function(project_number = project_get_current_id(ask = TRUE),
                                       account = connect_teams()) {
  if (is_empty(project_number)) {
    return(invisible())
  }
  project_number <- gsub("[^0-9]", "", project_number)
  # furst argument in project_get_file is case-insensitive
  path <- project_get_file(".*[.](R|qmd|Rmd|sql|txt|csv|tsv|css|ya?ml|js)$",
                           project_number = project_number,
                           account = account)
  if (is.null(path)) {
    stop(paste0("No syntax files found for p", project_number))
  } else {
    invisible(navigateToFile(file.path(get_projects_path(), path)))
  }
}

#' @rdname project_properties
#' @export
project_open_folder <- function(project_number = project_get_current_id(ask = TRUE),
                                account = connect_teams()) {
  if (is_empty(project_number)) {
    return(invisible())
  }
  path <- project_get_folder(project_number = project_number, account = account)
  utils::browseURL(file.path(get_projects_path(), path))
}

#' @rdname project_properties
#' @param filename name for the new Quarto file
#' @details [project_add_qmd_skeleton()] initializes a new Quarto skeleton for a project.
#' @importFrom rstudioapi showPrompt navigateToFile
#' @export
project_add_qmd_skeleton <- function(filename = NULL,
                                     project_number = project_get_current_id(),
                                     account = connect_planner()) {
  project_folder <- project_get_folder(project_number = project_number, account = account)
  if (is.na(project_folder)) {
    return(invisible())
  }
  if (is.null(filename)) {
    filename <- showPrompt(title = "Bestandsnaam voor Quarto-document",
                           message = paste0("Bestandsnaam voor het Quarto-document voor project '", project_folder, "':"),
                           default = paste0("Analyse ", gsub(".*(p[0-9]+).*", "\\1", project_folder), ".qmd"))
    
    if (is.null(filename)) {
      return(invisible())
    }
  }
  if (filename %unlike% "[.]qmd$") {
    filename <- paste0(filename, ".qmd")
  }
  filename <- file.path(project_folder, filename)
  file.copy(from = system.file("qmd_skeleton.qmd", package = "certeprojects"), to = filename, overwrite = FALSE, copy.date = FALSE)
  invisible(navigateToFile(file = filename))
}

get_projects_path <- function() {
  out <- eval(parse(text = certetoolbox::read_secret("onedrive.projects.folder")))
  if (!dir.exists(out)) {
    warning("Project folder does not exist: ", out, call. = FALSE)
  }
  out
}

#' @rdname project_properties
#' @export
get_output_folder <- function(project_number = project_get_current_id(),
                              account = connect_teams()) {
  project_title <- project_get_folder(project_number = project_number, account = account)
  output_path <- file.path(read_secret("projects.output_path"), project_title)
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  return(paste0(output_path, "/"))
}

#' @rdname project_properties
#' @export
get_output_folder_consult <- function() {
  file.path(read_secret("projects.output_path"), "Consulten/")
}
