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

#' Work with Pins
#' 
#' These functions can be used to work with [pins](https://pins.rstudio.com), developed by RStudio.
#' @inheritParams pins::pin_write
#' @importFrom pins pin_write
#' @details
#' The board will be automatically retrieved based on the Microsoft 365 account (by running [board_certeprojects()]), leading to the SharePoints folder "pins" within the Microsoft 365 account.
#' 
#' For Pins functions of the `pins` package, use [board_certeprojects()] as board, e.g.:
#' 
#' ```r
#'   pin_list(board_certeprojects())
#' ```
#' 
#' The following `pins` functions are re-exported by this package: [pin_list()], [pin_meta()], and [pin_versions()].
#' @rdname pins
#' @name pins
#' @export
export_pin <- function(x,
                       name = NULL,
                       title = NULL,
                       type = NULL,
                       description = NULL,
                       board = board_certeprojects()) {
  nm <- deparse(substitute(x))
  if (is.null(name) && nm != ".") {
    name <- nm
  }
  pin_write(board = board,
            x = x,
            name = name,
            title = title,
            type = type,
            description = description,
            metadata = list(username = unname(Sys.info()["user"]),
                            device = unname(Sys.info()["nodename"])))
}

#' @importFrom pins pin_read
#' @inheritParams pins::pin_read
#' @rdname pins
#' @name pins
#' @export
import_pin <- function(name,
                       version = NULL,
                       hash = NULL,
                       board = board_certeprojects()) {
  pin_read(board = board,
           name = name,
           version = version,
           hash = hash)
}

#' @importFrom pins pin_delete pin_version_delete
#' @inheritParams pins::pin_delete
#' @inheritParams pins::pin_version_delete
#' @rdname pins
#' @name pins
#' @export
remove_pin <- function(name,
                       version = NULL,
                       board = board_certeprojects()) {
  if (is.null(version)) {
    pin_delete(board = board,
               names = name)
  } else {
    pin_version_delete(board = board,
                       name = name,
                       version = version)
  }
}

#' @param site_name Name of the SharePoint site
#' @inheritParams pins::board_ms365
#' @param ... Arguments passed on to [get_microsoft365_token()]
#' @importFrom Microsoft365R get_sharepoint_site
#' @importFrom pins board_ms365
#' @rdname pins
#' @name pins
#' @export
board_certeprojects <- function(site_name = read_secret("teams.name"),
                                delete_by_item = TRUE,
                                ...) {
  if (is.null(pkg_env$microsoft365_pins)) {
    # not yet connected to Teams in Microsoft 365, so set it up
    sharepoint <- get_sharepoint_site(site_name = site_name, token = get_microsoft365_token("mail", ...))
    drive <- sharepoint$get_drive()
    pkg_env$microsoft365_pins <- board_ms365(drive = drive,
                                             path = drive$get_item("pins"),
                                             delete_by_item = delete_by_item)
    
  }
  # this will auto-renew authorisation when due
  pkg_env$microsoft365_pins
}

#' @importFrom pins pin_list
#' @export
pins::pin_list

#' @importFrom pins pin_meta
#' @export
pins::pin_meta

#' @importFrom pins pin_versions
#' @export
pins::pin_versions
