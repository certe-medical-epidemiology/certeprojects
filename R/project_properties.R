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
#' @param ask logical to indicate whether the project number should always be asked
#' @param card_number Trello card number
#' @param filename filename to set or get
#' @param foldername foldername to set
#' @param fixed logical to turn off regular expressions
#' @name project_properties
#' @details [project_set_folder()] will create the folder if it does not exist.
#' @rdname project_properties
#' @importFrom rstudioapi getSourceEditorContext showPrompt
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
  if (interactive()) {
    path <- getSourceEditorContext()$path
    if (is.null(path) && is.null(ask)) {
      id <- showPrompt("Project Number", "Enter Project Number:")
      return(fix_id(id))
    }
  } else {
    # for markdown
    path <- getwd()
  }
  parts <- tryCatch(unlist(strsplit(path, "[^a-zA-Z0-9]")), error = function(e) NULL)
  if (is.null(parts)) {
    return(NULL)
  }
  id <- parts[parts %like% "^p[0-9]+$"][1]
  if (all(length(id) == 0 | is.na(id)) && interactive() && is.null(ask)) {
    id <- showPrompt("Project Number", "Enter Project Number:")
    asked <- TRUE
  }
  
  if (identical(ask, TRUE) && asked == FALSE) {
    id <- showPrompt("Project Number", "Enter Project Number:", ifelse(length(id) > 0, paste0("p", fix_id(id)), ""))
    if (is.null(id) || all(is.na(id))) {
      return(NULL)
    }
  }
  fix_id(id)
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
    # sort on length:
    files_found_base <- files_found_base[order(nchar(files_found), files_found)]
    files_found <- files_found[order(nchar(files_found), files_found)]
    
    if (length(files_found) == 0) {
      warning("No files found")
      return(NA_character_)
    }
    if (length(files_found) > 0) {
      if (length(files_found) > 1) {
        if (interactive()) {
          choice <- utils::menu(choices = files_found_base,
                                title = paste0("Files found with '", filename, "' in ", folder, ":"))
          if (choice == 0) {
            return(NA_character_)
          }
          filename <- files_found[choice]
        } else {
          message("Files found with '", filename, "':\n  - ", paste(files_found_base, collapse = "\n  - "),
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
  if (!missing(card_number) & is.null(card_number)) {
    return(invisible())
  }
  card_number <- gsub("[^0-9]", "", card_number)
  path <- project_get_file(".*[.](R|Rmd)$", card_number = card_number)
  if (is.na(path)) {
    stop(paste0("No .R or .Rmd files found for p", card_number))
  } else {
    navigateToFile(path)
  }
}
