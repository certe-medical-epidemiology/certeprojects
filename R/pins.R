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
#' These functions can be used to work with [`pins`](https://pins.rstudio.com), developed by RStudio.
#' @inheritParams pins::pin_write
#' @importFrom pins pin_write
#' @details
#' These functions from the [`pins`](https://pins.rstudio.com) package are integrated into the team's Microsoft 365 account, using the `"pins"` folder in the given MS Teams channel.
#' 
#' For Pins functions of the `pins` package, use [pins_board()] as input, e.g.:
#' 
#' ```r
#'   pin_list(pins_board())
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
                       board = pins_board()) {
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
                       board = pins_board()) {
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
                       board = pins_board()) {
  if (is.null(version)) {
    pin_delete(board = board,
               names = name)
  } else {
    pin_version_delete(board = board,
                       name = name,
                       version = version)
  }
}

#' @inheritParams teams
#' @inheritParams pins::board_ms365
#' @details The [pins_board()] function returns a [pins::board_ms365] object based on the `"pins"` folder in the Teams channel *Projects*, which is retrieved with [teams_projects_channel()].
#' @importFrom pins board_ms365
#' @rdname pins
#' @name pins
#' @export
pins_board <- function(projects_channel = read_secret("teams.projects.channel"),
                       account = connect_teams(),
                       delete_by_item = TRUE) {
  if (is.null(pkg_env$microsoft365_pins)) {
    drives <- account$list_drives()
    drive <- drives[[which(get_azure_property(drives, "name") %unlike% "Wiki")[1]]]
    projects <- teams_projects_channel(projects_channel = projects_channel, account = account)
    pkg_env$microsoft365_pins <- board_ms365(drive = drive,
                                             path = projects$get_item("pins"),
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
