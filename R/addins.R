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


# Positron addins ---------------------------------------------------------

positron_moveTask <- function() {
  file <- get_file_details(include_teams = FALSE)
  planner <- connect_planner()
  task <- planner_task_find(file$folder_local)
  project_update(task, planner = planner)
}
#' @importFrom httr BROWSE
positron_openFolder <- function() {
  file <- get_file_details(include_teams = FALSE)
  BROWSE(file$folder_local)
}
positron_copyFolderLink <- function() {
  file <- get_file_details()
  url <- file$folder_remote$create_share_link(type = "view", expiry = NULL, password = NULL, scope = NULL)
  clipr::write_clip(url, object_type = "character")
  message("URL copied to clipboard (permission: view, expires: never)")
}
positron_validate <- function() {
  file <- get_file_details()
  message("Tadaaaa")
}
#' @importFrom httr BROWSE
positron_openSharePoint <- function() {
  file <- get_file_details()
  BROWSE(file$folder_remote$properties$webUrl)
}

get_file_details <- function(include_teams = TRUE) {
  current_path <- path.expand(rstudioapi::getSourceEditorContext()$path)
  parts <- strsplit(current_path, "/", fixed = TRUE)[[1]]
  start <- which(parts == read_secret("onedrive.projects.folder"))[1] + 1
  file_path <- paste(parts[c(start:length(parts))], collapse = "/")
  file_local <- basename(file_path)
  folder_local <- dirname(current_path)
  
  if (include_teams == FALSE) {
    return(list(file_local = file_local,
                folder_local = folder_local))
  }
  
  teams <- connect_teams()
  projects <- teams$get_channel(channel_id = read_secret("teams.projects.channel_id"))$get_folder()
  file_remote <- projects$get_item(file_local)
  folder_remote <- file_remote$get_parent_folder()
  
  list(file_local = file_local,
       folder_local = folder_local,
       file_remote = file_remote,
       folder_remote = folder_remote)
}


# RStudio addins ----------------------------------------------------------

addin1_projects_new <- function() {
  project_add()
}
addin1b_consult_new <- function() {
  consult_add()
}
addin2_projects_update <- function() {
  project_update()
}
addin5_quarto_skeleton <- function() {
  project_add_qmd_skeleton()
}
addin6_projects_save <- function() {
  project_save_file()
}
addin3_projects_open_file <- function() {
  project_open_analysis_file()
}
addin6_projects_teams_open_file <- function() {
  teams_file <- teams_download_file()
  file_object <- certetoolbox::import(teams_file)
  env <- base::globalenv()
  assign(x = gsub(paste0(".", tools::file_ext(basename(teams_file)), "$"),
                  "",
                  basename(teams_file)),
         value = file_object,
         envir = env)
}
addin4_projects_open_folder <- function() {
  project_open_folder()
}

#' @importFrom rstudioapi showPrompt filesPaneNavigate showQuestion
set_wd <- function(new_dir = NULL) {
  if (is.null(new_dir)) {
    if ("clipr" %in% rownames(utils::installed.packages())) {
      default <- clipr::read_clip()
      if (!is.character(default) || length(default) != 1 || !dir.exists(as.character(default)[1L])) {
        default <- ""
      }
    } else {
      default <- ""
    }
    new_dir <- showPrompt(title = "Set Working Directory",
                          message = "New Working Directory:",
                          default = default)
  }
  if (is.null(new_dir) || !dir.exists(new_dir)) {
    return(invisible())
  }
  setwd(new_dir)
  filesPaneNavigate(new_dir)
  
  rdata <- paste0(new_dir, "/.Rdata")
  if (file.exists(rdata)) {
    if (isTRUE(showQuestion(title = "Load RData",
                            message = paste0("Also load RData from folder ", basename(new_dir), "?"),
                            ok = "Yes",
                            cancel = "No"))) {
      load(rdata, envir = globalenv())
    }
  }
}
