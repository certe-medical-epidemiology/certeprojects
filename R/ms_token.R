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
#' @param scope this must be "outlook", "teams", "planner", or "sharepoint", and will set the right API permission for each
#' @param tenant the tenant to use, passed on to [AzureGraph::create_graph_login()]
#' @param app_id the Azure app id to use, passed on to [AzureGraph::create_graph_login()]
#' @param auth_type the authentication method to use, passed on to [AzureGraph::create_graph_login()]
#' @param ... other arguments passed on to [AzureGraph::create_graph_login()]
#' @param error_on_fail a [logical] to indicate whether an error must be thrown if no connection can be made
#' @importFrom AzureGraph create_graph_login get_graph_login
#' @export
get_microsoft365_token <- function(scope,
                                   tenant = read_secret("azure.tenant"),
                                   app_id = read_secret("azure.app_id"),
                                   auth_type = read_secret("azure.auth_type"),
                                   ...,
                                   error_on_fail = FALSE) {
  # for the scopes, see here: https://docs.microsoft.com/en-us/graph/permissions-reference
  scope_options <- c("mail", "outlook", "teams", "planner", "sharepoint")
  scope <- tolower(scope)[1]
  if (scope == "mail") {
    scope <- "outlook"
  }
  
  if (tenant == "") {
    tenant <- "common"
  }
  if (app_id == "") {
    app_id <- NULL
  }
  if (auth_type == "") {
    auth_type <- NULL
  }
  
  # mail ----
  if (scope == "outlook") {
    scopes <- c("Mail.ReadWrite",
                "Mail.ReadWrite.Shared",
                "Mail.Send",
                "Mail.Send.Shared",
                "User.Read")
    
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
    
    # sharepoint ----
  } else if (scope == "sharepoint") {
    scopes <- c("Group.Read.All",
                "Files.ReadWrite.All",
                "Sites.ReadWrite.All",
                "User.ReadWrite")
    
  } else {
    stop("Invalid scope - must be one of ", toString(scope_options))
  }

  if (is.null(suppressMessages(pkg_env[[scope]]))) {
    # not yet connected to Microsoft 365, so set it up
    tryCatch({
      # try to get existing login first - this will prevent that the device code must be given every time
      conn <- try(suppressMessages(get_graph_login(tenant = tenant, app = app_id, scopes = scopes, refresh = FALSE)), silent = TRUE)
      if (inherits(conn, "try-error")) {
        # now create a new login
        conn <- suppressMessages(create_graph_login(tenant = tenant, app = app_id, scopes = scopes, auth_type = auth_type, ...))
      }
      pkg_env[[scope]] <- suppressMessages(conn$token)
      # if (interactive()) {
      #   message("Connected to Microsoft 365 ", tools::toTitleCase(scope), ".")
      # }
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
  suppressMessages(pkg_env[[scope]])
}
