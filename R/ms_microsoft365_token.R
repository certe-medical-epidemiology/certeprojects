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

#' Retrieve Microsoft 365 Access Token
#' 
#' This function uses retrieves an access token from the department's Microsoft 365 account.
#' @param scope this must be "mail", "teams", or "planner", and will set the right API permission for each
#' @param shared_mbox_email email address of the shared mailbox to use for [Microsoft365R::get_business_outlook()]
#' @param tenant the tenant to use for [Microsoft365R::get_business_outlook()]
#' @param app_id the Azure app id to use for [Microsoft365R::get_business_outlook()]
#' @param auth_type the authentication method to use for [Microsoft365R::get_business_outlook()]
#' @param error_on_fail a [logical] to indicate whether an error must be thrown if no connection can be made
#' @export
get_microsoft365_token <- function(scope,
                                   shared_mbox_email = read_secret("mail.auto_from"),
                                   tenant = read_secret("mail.tenant"),
                                   app_id = read_secret("mail.app_id"),
                                   auth_type = read_secret("mail.auth_type"),
                                   error_on_fail = FALSE) {
  # for the scopes, see here: https://docs.microsoft.com/en-us/graph/permissions-reference
  scope_options <- c("mail", "teams", "planner")
  scope <- tolower(scope)[1]
  
  # mail ----
  if (scope == "mail") {
    scopes <- c("Mail.ReadWrite", "Mail.Send", "User.Read")
    if (shared_mbox_email == "") {
      shared_mbox_email <- NULL
    } else {
      scopes <- c(scopes, "Mail.Send.Shared", "Mail.ReadWrite.Shared")
    }
    if (tenant == "") {
      tenant <- NULL
    }
    if (app_id == "") {
      app_id <- get(".microsoft365r_app_id", envir = asNamespace("Microsoft365R"))
    } else {
      scopes <- c("Mail.ReadWrite", "Mail.Send", "User.Read",
                  "Mail.Send.Shared", "Mail.ReadWrite.Shared")
    }
    if (auth_type == "") {
      auth_type <- NULL
    }
    
    # teams ----
  } else if (scope == "teams") {
    scopes <- c("Files.ReadWrite.All",
                "Sites.ReadWrite.All",
                "User.ReadWrite")
    
    # tasks ----
  } else if (scope == "planner") {
    scopes <- c("Group.Read.All",
                "Tasks.Read.Shared",
                "Tasks.ReadWrite",
                "User.ReadWrite")
    
  } else {
    stop("Invalid scope - must be one of ", toString(scope_options))
  }

  if (is.null(pkg_env[[scope]])) {
    # not yet connected to Microsoft 365, so set it up
    tryCatch({
      if (is.null(tenant)) {
        conn <- suppressWarnings(suppressMessages(
          Microsoft365R::get_business_outlook(shared_mbox_email = shared_mbox_email,
                                              scopes = scopes,
                                              app = app_id,
                                              auth_type = auth_type)))
      } else {
        conn <- suppressWarnings(suppressMessages(
          Microsoft365R::get_business_outlook(tenant = tenant,
                                              shared_mbox_email = shared_mbox_email,
                                              scopes = scopes,
                                              app = app_id,
                                              auth_type = auth_type)))
      }
      pkg_env[[scope]] <- conn$token
      message("Connected to Microsoft 365 as ",
              conn$properties$displayName,
              " (", conn$properties$mail, ").")
    }, warning = function(w) {
      return(invisible())
    }, error = function(e, fail = error_on_fail) {
      if (isTRUE(fail)) {
        stop("Could not connect to Microsoft 365: ", paste0(e$message, collapse = ", "), call. = FALSE)
      } else {
        warning("Could not connect to Microsoft 365: ", paste0(e$message, collapse = ", "), call. = FALSE)
      }
      return(NULL)
    })
  }
  if (isTRUE(error_on_fail) && is.null(pkg_env[[scope]])) {
    stop("Could not connect to Microsoft 365.", call. = FALSE)
  }
  # this will auto-renew authorisation when due
  pkg_env[[scope]]
}
