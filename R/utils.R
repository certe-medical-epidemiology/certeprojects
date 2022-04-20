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
pkg_env$teams <- NULL

globalVariables(c(".",
                  "checkItems",
                  "closed",
                  "data.text",
                  "desc",
                  "fullName",
                  "id",
                  "idCard",
                  "idMembers",
                  "idShort",
                  "list.name",
                  "memberCreator.fullName",
                  "name",
                  "name.card",
                  "name.list",
                  "shortLink",
                  "shortUrl",
                  "text",
                  "title",
                  "username"))

#' @importFrom dplyr `%>%` arrange filter
get_user <- function(..., property = "shiny") {
  user_file <- read_secret("users.csv.file")
  if (user_file == "") {
    return("")
  }
  users <- utils::read.csv(user_file, fileEncoding = "UTF-8")
  if (!is.null(users)) {
    users <- users %>%
      filter(...) %>%
      arrange(name)
    if (property == "shiny") {
      users_id <- users$id
      if (!all(is.na(users$job))) {
        users$job[is.na(users$job) | users$job == ""] <- ""
        names(users_id) <- paste0(users$name, " (", tolower(users$job), ")")
      } else {
        names(users_id) <- users$name
      }
      users <- users_id
    } else {
      users <- users[, property, drop = TRUE]
    }
  }
  users
}
