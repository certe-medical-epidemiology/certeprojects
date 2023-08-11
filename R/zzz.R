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

#' @importFrom callr r_bg
.onAttach <- function(...) {
  # connect on attach with a background R process
  # this will make sure that using Outlook, Planner, or Teams can be done instantly
  # this must be in .onAttach() and not .onLoad() as it would otherwise lead to a loop
  try(pkg_env$callr <- r_bg(function() 
    list(outlook = tryCatch(certeprojects::connect_outlook(),
                            error = function(e) NULL),
         planner = tryCatch(certeprojects::connect_planner(),
                            error = function(e) NULL),
         teams = tryCatch(certeprojects::connect_teams(),
                          error = function(e) NULL),
         teams_project_folder = tryCatch(certeprojects::teams_projects_channel(),
                                         error = function(e) NULL))),
    silent = TRUE)
}