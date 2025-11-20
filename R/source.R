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

#' Run R Code from SharePoint
#' 
#' This runs [source()] on a SharePoint file, by downloading it to a temporary folder first.
#' @param full_path Full path of file, including the project folder: `"Project - p123/my_file.R"`. Will be passed on to [sharepoint_to_local_temp()].
#' @param ... arguments passed on to [source()]
#' @inheritParams project_get_file
#' @rdname project_properties
#' @export
source_sharepoint <- function(full_path,
                              account = connect_teams(),
                              ...) {
  file_local <- sharepoint_to_local_temp(full_path = full_path, account = account, ...)
  source(file_local, ...)
}
