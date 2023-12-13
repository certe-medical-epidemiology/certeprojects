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
#' Retrieve project properties, such as the title, folder location and project number.
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
project_get_current_id <- function(ask = NULL) {
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
    path <- full_path_to_currently_sourced_script()
    if (is.null(path) && (is.null(ask) || isTRUE(ask))) {
      # still NULL
      search_term <- showPrompt("Zoekterm taak", "Zoekterm om naar een taak te zoeken:", "")
      if (is.null(search_term)) return(invisible(FALSE))
      id <- planner_retrieve_project_id(planner_task_search(search_term = search_term, limit = 25))
      return(fix_id(id))
    } else if (is.null(path)) {
      # for when ask == FALSE
      path <- getwd()
    }
  } else if (is.null(path)) {
    # for Quarto and R Markdown
    path <- getwd()
  }
  parts <- tryCatch(unlist(strsplit(path, "[^a-zA-Z0-9]")), error = function(e) NULL)
  if (is.null(parts)) {
    return(NULL)
  }
  id <- parts[parts %like% "^p[0-9]+$"][1]
  if (all(length(id) == 0 | is.na(id)) && interactive() && is.null(ask)) {
    search_term <- showPrompt("Zoekterm taak", "Zoekterm om naar een taak te zoeken:", "")
    if (is.null(search_term)) return(invisible(FALSE))
    task <- planner_task_search(search_term = search_term, limit = 25)
    if (is.null(task) || suppressWarnings(all(is.na(task)))) {
      return(invisible())
    }
    id <- planner_retrieve_project_id(task)
    asked <- TRUE
  }
  
  if (identical(ask, TRUE) && asked == FALSE) {
    if (interactive() && (length(id) == 0 || is.na(id))) {
      search_term <- showPrompt("Zoekterm taak", "Zoekterm om naar een taak te zoeken:", "")
      if (is.null(search_term)) return(invisible(FALSE))
    } else {
      search_term <- ""
    }
    task <- planner_task_search(search_term = ifelse(!is.na(id) & length(id) > 0,
                                                     paste0("p", fix_id(id)),
                                                     search_term),
                                limit = 25)
    if (is.null(task) || suppressWarnings(all(is.na(task)))) {
      return(invisible())
    }
    id <- planner_retrieve_project_id(task)
    if (is.null(id) || all(is.na(id))) {
      return(NULL)
    }
  }
  fix_id(id)
}

#' @rdname project_properties
#' @details [project_identifier()] generates the project identifier for print on reports and in mails: a combination of the currently logged in user (in your case: '\Sexpr{Sys.info()\["user"\]}'), the current date/time (format: YYMMDDHHMM), and the project number. If the project number is not available, it will only return the current user and date/time (format: YYMMDDHHMM).
#' @importFrom certestyle format2
#' @export
#' @examples
#' project_identifier(123)
project_identifier <- function(project_number = project_get_current_id()) {
  user_datetime <- paste0(Sys.info()["user"], "-", format2(Sys.time(), "yymmddHHMM"))
  if (is.null(project_number) || all(project_number %in% c("", NA, FALSE))) {
    user_datetime
  } else {
    paste0(user_datetime, "-", project_number)
  }
}

#' @rdname project_properties
#' @export
project_get_folder <- function(project_number = project_get_current_id(),
                               account = connect_planner()) {
  project_number <- gsub("[^0-9]", "", project_number)
  basename(project_get_folder_full(project_number = project_number, account = account))
}

#' @rdname project_properties
#' @inheritParams planner_create_project_from_path
#' @export
project_get_folder_full <- function(project_number = project_get_current_id(),
                                    projects_path = read_secret("projects.path"),
                                    account = connect_planner()) {
  if (isFALSE(project_number)) {
    return(NA_character_)
  }
  if (!is.null(attributes(project_number)$task)) {
    # when using project_get_current_id(), the result comes from planner_retrieve_project_id() which contains the task as attribute
    project_title <- attributes(project_number)$task |> get_azure_property("title")
  } else {
    project_title <- planner_task_search(project_number,
                                         include_completed = TRUE,
                                         include_description = FALSE,
                                         account = account) |> 
      get_azure_property("title")
  }
  
  folders <- list.dirs(projects_path,
                       full.names = FALSE,
                       recursive = FALSE)
  folder <- folders[folders == project_title][1]
  if (!is.na(folder)) {
    paste0(file.path(projects_path, folder), .Platform$file.sep)
  } else {
    warning("Project folder not found: \"", file.path(projects_path, project_title), "\"", call. = FALSE)
    NA_character_
  }
}

#' @rdname project_properties
#' @export
project_get_title <- function(project_number = project_get_current_id(),
                              account = connect_planner()) {
  if (is.null(project_number)) {
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
                             account = connect_planner()) {
  project_number <- gsub("[^0-9]", "", project_number)
  folder <- project_get_folder_full(project_number = project_number, account = account)
  filename <- filename[1L]
  if (isTRUE(fixed)) {
    filename <- paste0("^", filename, "$")
  }
  if (!is.na(folder)) {
    # regex, try to find file
    files_found <- sort(list.files(path = folder,
                                   pattern = filename,
                                   full.names = TRUE,
                                   recursive = FALSE,
                                   all.files = FALSE,
                                   include.dirs = FALSE,
                                   ignore.case = TRUE))
    files_found <- files_found[!basename(files_found) %like% "^~[$]"]
    files_found_base <- basename(files_found)
    # sort on last changed (desc):
    files_found_base <- files_found_base[order(file.mtime(files_found), files_found, decreasing = TRUE)]
    files_found <- files_found[order(file.mtime(files_found), files_found, decreasing = TRUE)]
    
    if (length(files_found) == 0) {
      warning("No files found")
      return(NA_character_)
    }
    if (length(files_found) > 0) {
      if (length(files_found) > 1) {
        if (interactive()) {
          choice <- utils::menu(choices = paste0(font_bold(files_found_base, collapse = NULL),
                                                 " (",
                                                 count_lines(files_found), 
                                                 " lines, last changed: ",
                                                 font_blue(format2(file.mtime(files_found), "yyyy-mm-dd HH:MM"), collapse = NULL),
                                                 ")"),
                                title = paste0("Files found in ", folder, " (0 to cancel):\n(sorted on last changed)"))
          if (choice == 0) {
            return(NA_character_)
          }
          filename <- files_found[choice]
        } else {
          message("Files found:\n  - ", paste(files_found_base, collapse = "\n  - "),
                  "\nSelecting first match.")
          filename <- files_found[1L]
        }
      } else {
        filename <- files_found
      }
    }
  } else {
    return(NA_character_)
  }
  tools::file_path_as_absolute(filename)
}

#' @rdname project_properties
#' @export
project_set_file <- function(filename,
                             project_number = project_get_current_id(),
                             account = connect_planner()) {
  project_number <- gsub("[^0-9]", "", project_number)
  folder <- project_get_folder_full(project_number = project_number, account = account)
  filename <- filename[1L]
  if (!is.na(folder)) {
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
                               account = connect_planner()) {
  project_number <- gsub("[^0-9]", "", project_number)
  folder <- project_get_folder_full(project_number = project_number, account = account)
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
                                       account = connect_planner()) {
  if (is.null(project_number)) {
    return(invisible())
  }
  project_number <- gsub("[^0-9]", "", project_number)
  # furst argument in project_get_file is case-insensitive
  path <- project_get_file(".*[.](R|qmd|Rmd|sql|txt|csv|tsv|css|ya?ml|js)$",
                           project_number = project_number,
                           account = account)
  if (is.na(path)) {
    stop(paste0("No syntax files found for p", project_number))
  } else {
    invisible(navigateToFile(path))
  }
}

#' @rdname project_properties
#' @export
project_open_folder <- function(project_number = project_get_current_id(ask = TRUE),
                                account = connect_planner()) {
  if (is.null(project_number)) {
    return(invisible())
  }
  path <- project_get_folder_full(project_number = project_number, account = account)
  utils::browseURL(path)
}

#' @importFrom rstudioapi showPrompt getSourceEditorContext showQuestion navigateToFile
project_save_file <- function(project_number = project_get_current_id(ask = TRUE),
                              account = connect_planner()) {
  current <- getSourceEditorContext()
  if (is.null(current)) {
    showDialog(title = "No file to be saved", message = "Open a (text) file first to save.")
    return(invisible(FALSE))
  }
  
  if (is.null(project_number)) {
    path <- getwd()
  } else {
    path <- project_get_folder_full(project_number = project_number, account = account)
  }
  
  new_file <- showPrompt(title = "Save File",
                         message = ifelse(is.null(project_number),
                                          paste0("In ", path, ":"),
                                          paste0("Project p", project_number, " in ", path, ":")),
                         default = ifelse(current$path != "",
                                          basename(current$path),
                                          "Analyse.R"))
  if (is.null(new_file)) {
    return(invisible(FALSE))
  }
  new_file <- paste0(path, "/", new_file)
  if (!file.exists(new_file) || isTRUE(showQuestion(title = "File exists", message = paste0("Overwrite ", new_file, "?")))) {
    # write to file
    fileConn <- file(new_file)
    writeLines(current$contents, fileConn)
    close(fileConn)
    invisible(navigateToFile(new_file,
                             line = current$selection[[1]]$range$start[[1]],
                             column = current$selection[[1]]$range$start[[2]]))
  } else {
    message("File save cancelled.")
  }
}

#' @rdname project_properties
#' @param filename name for the new Quarto file
#' @details [project_add_qmd_skeleton()] initializes a new Quarto skeleton for a project.
#' @importFrom rstudioapi showPrompt navigateToFile
#' @export
project_add_qmd_skeleton <- function(filename = NULL,
                                     project_number = project_get_current_id(),
                                     account = connect_planner()) {
  project_folder <- project_get_folder_full(project_number = project_number, account = account)
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
