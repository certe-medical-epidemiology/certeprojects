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
#' Retrieve project properties, such as the title, folder location and Trello card number.
#' @param ask logical to indicate whether the project number should always be asked. The default, `NULL`, will show a popup in [interactive][interactive()] \R sessions, allowing to search for projects. In non-interactive sessions, such as in Quarto and R Markdown, it will use the current [working directory][getwd()] to determine the project number.
#' @param card_number Trello card number
#' @param filename filename to set or get
#' @param foldername foldername to set
#' @param fixed [logical] to turn off regular expressions
#' @name project_properties
#' @details [project_get_current_id()] uses [trello_search_card()] to find a specific project based on any search string. 
#' @rdname project_properties
#' @importFrom rstudioapi getSourceEditorContext
#' @export
project_get_current_id <- function(ask = NULL) {
  # first try project number from full file location:
  # /folder/p123 Name.Rmd
  # /p123 folder/Name.Rmd
  fix_id <- function(id) {
    id <- gsub("[^0-9]", "", id)
    if (all(is.na(id) | length(id) == 0) || identical(id, "")) {
      NULL
    } else {
      id
    }
  }
  asked <- FALSE
  path <- full_path_to_currently_sourced_script()
  if (is.null(path) && interactive() && rstudioapi::isAvailable()) {
    path <- getSourceEditorContext()$path
    if (is.null(path) && (is.null(ask) || isTRUE(ask))) {
      id <- trello_search_card()
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
    id <- trello_search_card()
    asked <- TRUE
  }
  
  if (identical(ask, TRUE) && asked == FALSE) {
    id <- trello_search_card(x = ifelse(!is.na(id) & length(id) > 0,
                                        paste0("p", fix_id(id)),
                                        ""))
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
project_identifier <- function(card_number = project_get_current_id()) {
  user_datetime <- paste0(Sys.info()["user"], "-", format2(Sys.time(), "yymmddHHMM"))
  if (is.null(card_number) || all(card_number %in% c("", NA, FALSE))) {
    user_datetime
  } else {
    paste0(user_datetime, "-", card_number)
  }
}

#' @rdname project_properties
#' @export
project_get_folder <- function(card_number = project_get_current_id()) {
  card_number <- gsub("[^0-9]", "", card_number)
  basename(project_get_folder_full(card_number = card_number))
}

#' @rdname project_properties
#' @export
project_get_folder_full <- function(card_number = project_get_current_id()) {
  card_number <- gsub("[^0-9]", "", card_number)
  project_path <- read_secret("projects.path")
  
  folders <- list.dirs(project_path,
                       full.names = TRUE,
                       recursive = FALSE)
  folder <- folders[folders %like% paste0("p", card_number)][1L]
  if (!is.na(folder)) {
    out <- gsub("\\", "/", normalizePath(paste0(folder, "/")), fixed = TRUE)
    if (out %unlike% "/$") {
      out <- paste0(out, "/")
    }
    out
  } else {
    warning("Project folder of p", card_number, " not found")
    NA_character_
  }
}

#' @rdname project_properties
#' @export
project_get_title <- function(card_number = project_get_current_id()) {
  card_number <- gsub("[^0-9]", "", card_number)
  trimws(gsub("-? ?p[0-9]+/?$", "", project_get_folder(card_number = card_number)))
}

#' @rdname project_properties
#' @importFrom certestyle format2 font_bold font_blue
#' @export
project_get_file <- function(filename, card_number = project_get_current_id(), fixed = FALSE) {
  card_number <- gsub("[^0-9]", "", card_number)
  folder <- project_get_folder_full(card_number = card_number)
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
project_set_file <- function(filename, card_number = project_get_current_id()) {
  card_number <- gsub("[^0-9]", "", card_number)
  folder <- project_get_folder_full(card_number = card_number)
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
project_set_folder <- function(foldername, card_number = project_get_current_id()) {
  card_number <- gsub("[^0-9]", "", card_number)
  folder <- project_get_folder_full(card_number = card_number)
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
project_open_analysis_file <- function(card_number = project_get_current_id(ask = TRUE)) {
  if (is.null(card_number)) {
    return(invisible())
  }
  card_number <- gsub("[^0-9]", "", card_number)
  path <- project_get_file(".*[.](R|Rmd|sql|txt|csv|tsv|css|ya?ml|js)$", card_number = card_number)
  if (is.na(path)) {
    stop(paste0("No .R or .Rmd files found for p", card_number))
  } else {
    navigateToFile(path)
  }
}

#' @rdname project_properties
#' @export
project_open_folder <- function(card_number = project_get_current_id(ask = TRUE)) {
  if (is.null(card_number)) {
    return(invisible())
  }
  path <- project_get_folder_full(card_number = card_number)
  utils::browseURL(path)
}
