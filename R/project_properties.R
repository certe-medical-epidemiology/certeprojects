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
  path <- full_path_to_currently_sourced_script()
  if (is.null(path) && interactive() && isAvailable()) {
    path <- tools::file_path_as_absolute(getSourceEditorContext()$path)
    if (is.null(path) && (is.null(ask) || isTRUE(ask))) {
      search_term <- showPrompt("Zoekterm taak", "Zoekterm om naar een taak te zoeken:", "")
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
project_get_folder <- function(project_number = project_get_current_id()) {
  project_number <- gsub("[^0-9]", "", project_number)
  basename(project_get_folder_full(project_number = project_number))
}

#' @rdname project_properties
#' @export
project_get_folder_full <- function(project_number = project_get_current_id()) {
  project_number <- gsub("[^0-9]", "", project_number)
  project_path <- read_secret("projects.path")
  
  folders <- list.dirs(project_path,
                       full.names = TRUE,
                       recursive = FALSE)
  folder <- folders[folders %like% paste0("p", project_number)][1L]
  if (!is.na(folder)) {
    out <- gsub("\\", "/", normalizePath(paste0(folder, "/")), fixed = TRUE)
    if (out %unlike% "/$") {
      out <- paste0(out, "/")
    }
    out
  } else {
    warning("Project folder of p", project_number, " not found")
    NA_character_
  }
}

#' @rdname project_properties
#' @export
project_get_title <- function(project_number = project_get_current_id()) {
  if (is.null(project_number)) {
    return(NA_character_)
  }
  project_number <- gsub("[^0-9]", "", project_number)
  trimws(gsub("-? ?p[0-9]+/?$", "", project_get_folder(project_number = project_number)))
}

#' @rdname project_properties
#' @importFrom certestyle format2 font_bold font_blue
#' @export
project_get_file <- function(filename, project_number = project_get_current_id(), fixed = FALSE) {
  project_number <- gsub("[^0-9]", "", project_number)
  folder <- project_get_folder_full(project_number = project_number)
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
project_set_file <- function(filename, project_number = project_get_current_id()) {
  project_number <- gsub("[^0-9]", "", project_number)
  folder <- project_get_folder_full(project_number = project_number)
  filename <- filename[1L]
  if (!is.na(folder)) {
    filename <- paste0(folder, filename)
  } else {
    filename <- paste0(tools::file_path_as_absolute(dirname(filename)), "/", filename)
  }
  gsub("//", "/", filename, fixed = TRUE)
}

#' @rdname project_properties
#' @details [project_set_folder()] will create the folder if it does not exist.
#' @export
project_set_folder <- function(foldername, project_number = project_get_current_id()) {
  project_number <- gsub("[^0-9]", "", project_number)
  folder <- project_get_folder_full(project_number = project_number)
  foldername <- foldername[1L]
  if (!is.na(folder)) {
    foldername <- gsub("//", "/", paste0(folder, "/", foldername), fixed = TRUE)
  }
  if (!dir.exists(foldername)) {
    invisible(dir.create(foldername))
  }
  paste0(tools::file_path_as_absolute(foldername), "/")
}

#' @rdname project_properties
#' @importFrom rstudioapi navigateToFile
#' @export
project_open_analysis_file <- function(project_number = project_get_current_id(ask = TRUE)) {
  if (is.null(project_number)) {
    return(invisible())
  }
  project_number <- gsub("[^0-9]", "", project_number)
  # furst argument in project_get_file is case-insensitive
  path <- project_get_file(".*[.](R|qmd|Rmd|sql|txt|csv|tsv|css|ya?ml|js)$", project_number = project_number)
  if (is.na(path)) {
    stop(paste0("No syntax files found for p", project_number))
  } else {
    invisible(navigateToFile(path))
  }
}

#' @rdname project_properties
#' @export
project_open_folder <- function(project_number = project_get_current_id(ask = TRUE)) {
  if (is.null(project_number)) {
    return(invisible())
  }
  path <- project_get_folder_full(project_number = project_number)
  utils::browseURL(path)
}

#' @importFrom rstudioapi showPrompt getSourceEditorContext showQuestion navigateToFile
project_save_file <- function(project_number = project_get_current_id(ask = TRUE)) {
  current <- getSourceEditorContext()
  if (is.null(current)) {
    showDialog(title = "No file to be saved", message = "Open a (text) file first to save.")
    return(invisible(FALSE))
  }
  
  if (is.null(project_number)) {
    path <- getwd()
    
  } else {
    path <- project_get_folder_full(project_number = project_number)
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
