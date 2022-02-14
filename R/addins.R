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
addin2_projects_edit <- function() {
  project_edit()
}
addin3_projects_open_file <- function() {
  project_open_analysis_file()
}
addin4_projects_open_folder <- function() {
  project_open_folder()
}
addin5_projects_trellocard <- function() {
  trello_open_card()
}
addin6_projects_trelloboard <- function() {
  trello_open_board()
}
