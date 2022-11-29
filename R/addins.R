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

addin1_projects_new <- function() {
  project_add()
}
addin2_qmd_new <- function() {
  project_new_qmd()
}
addin3_projects_edit <- function() {
  project_edit()
}
addin4_projects_save <- function() {
  project_save_file()
}
addin5_projects_open_file <- function() {
  project_open_analysis_file()
}
addin6_projects_open_folder <- function() {
  project_open_folder()
}
addin7_projects_trellocard <- function() {
  trello_open_card()
}
addin8_projects_trelloboard <- function() {
  trello_open_board()
}
addin9_projects_mine <- function() {
  trello_my_cards()
}

#' @importFrom rstudioapi showPrompt filesPaneNavigate
set_wd <- function() {
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
  if (is.null(new_dir) || !dir.exists(new_dir)) {
    return(invisible())
  }
  setwd(new_dir)
  rstudioapi::filesPaneNavigate(new_dir)
}

