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

# this is the package environment. The Microsoft 365 connection will be saved to this env.
pkg_env <- new.env(hash = FALSE)


#' Get Azure Property
#' 
#' This function retrieves a property from an Azure object, such as [`ms_plan`][Microsoft365R::ms_plan], [`ms_plan_task`][Microsoft365R::ms_plan_task], [`ms_team`][Microsoft365R::ms_team], [`ms_team_member`][Microsoft365R::ms_team_member], [`ms_drive_item`][Microsoft365R::ms_drive_item].
#' @param x an Azure object
#' @param property the name of the property, such as `"id"` or `"displayName"`. This must exist in `x` or in `x$properties`, and will return `NA` otherwise.
#' @export
get_azure_property <- function(x, property) {
  if (is.list(x)) {
    return(unlist(lapply(x, get_azure_property, property = property), use.names = FALSE))
  }
  out <- NULL
  if ("properties" %in% names(x) && property %in% names(x$properties)) {
    out <- x$properties[[property]]
  } else if (property %in% names(x)) {
    out <- x[[property]]
  }
  if (is.null(out) || length(out) == 0) {
    NA
  } else {
    out
  }
}


globalVariables(c("is_like",
                  "levenshtein_delta",
                  "name"))

count_lines <- function(x) {
  vapply(FUN.VALUE = integer(1),
         x,
         function(f) length(readLines(f, ok = TRUE, warn = FALSE)))
}

is_empty <- function(x) {
  is.null(x) || isFALSE(x) || identical(x, "") || all(is.na(as.character(x)))
}

arg_is_empty <- function(x) {
  is.null(x) || length(x) == 0 || all(is.na(x))
}

#' @importFrom dplyr arrange filter
get_user <- function(..., property = "shiny") {
  user_file <- read_secret("users.csv.file")
  if (user_file == "") {
    return("")
  }
  users <- utils::read.csv(user_file, fileEncoding = "UTF-8")
  if (!is.null(users)) {
    users <- users |> 
      filter(...) |> 
      arrange(name)
    if (property == "shiny") {
      users <- users$name
    } else {
      users <- users[, property, drop = TRUE]
    }
  }
  users
}

# helper function for paths and source() calls
full_path_to_currently_sourced_script <- function() {
  out <- NULL
  for (i in sys.nframe():1) {  # go through all the call frames, in *reverse* order
    x <- sys.frame(i)$ofile
    if (!is.null(x) & is.null(out)) {
      out <- normalizePath(x)
    }
  }
  out
}

get_current_user <- function() {
  unname(Sys.info()["user"])
}
