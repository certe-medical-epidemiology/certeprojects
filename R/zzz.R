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
#' @importFrom AzureGraph create_graph_login
#' @importFrom rstudioapi registerCommandCallback isAvailable
.onAttach <- function(...) {
  # connect on attach with a background R process
  # this will make sure that using Outlook, Planner, or Teams can be done instantly
  # this must be in .onAttach() and not .onLoad() as it would otherwise lead to a loop
  try(pkg_env$callr <- r_bg(function() 
    list(azure_token = tryCatch(certeprojects::get_microsoft365_token(),
                                error = function(e) NULL),
         
         teams_groups = tryCatch({
           grps <- AzureGraph::create_graph_login(token = certeprojects::get_microsoft365_token())$list_groups()
           nms <- certeprojects::get_azure_property(grps, "displayName")
           vis <- certeprojects::get_azure_property(grps, "visibility")
           # return names of Teams groups that are visible
           sort(nms[!is.na(vis)])
         }, error = function(e) NULL),
         
         teams_project_folder = tryCatch(certeprojects::teams_projects_channel(),
                                         error = function(e) NULL))),
    silent = TRUE)
  
  # save handler for Teams files
  # TODO remove the FALSE here to make it work
  if (FALSE && interactive() && isAvailable()) {
    try(
      pkg_env$save_handle <- rstudioapi::registerCommandCallback(
        commandId = "saveSourceDoc",
        callback = function() {
          tryCatch(rs_teams_save(), error = function(e) message("Could not upload to Teams: ", e$message))
        }),
      silent = TRUE)
  }
}

#' @importFrom rstudioapi unregisterCommandCallback isAvailable
.onDetach <- function(...) {
  # TODO remove the FALSE here to make it work
  if (FALSE && interactive() && isAvailable()) {
    rstudioapi::unregisterCommandCallback(handle = pkg_env$save_handle)
  }
}
