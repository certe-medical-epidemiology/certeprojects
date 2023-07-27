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

#' Connect to Microsoft 365
#' 
#' These functions create a connection to Microsoft 365 and saves the connection to the `certeprojects` package environment. The `planner_*()` and `teams_*()` functions allow to work with these connections.
#' @param scope this must be "outlook", "teams", "planner", "tasks" (which is "planner" without group rights), or "sharepoint", and will set the right API permission for each
#' @param tenant the tenant to use, passed on to [AzureGraph::create_graph_login()]
#' @param app_id the Azure app id to use, passed on to [AzureGraph::create_graph_login()]
#' @param auth_type the authentication method to use, passed on to [AzureGraph::create_graph_login()]
#' @param ... other arguments passed on to [AzureGraph::create_graph_login()]
#' @param error_on_fail a [logical] to indicate whether an error must be thrown if no connection can be made
#' @param team_name name of the team, can be left blank to connect to an individual planner
#' @param plan name of the team's plan if `team_name` is not empty. Otherwise, a plan ID (for individual use).
#' @param email email address of the user, or a shared mailbox
#' @param overwrite a [logical] to overwrite an existing connection, useful for switching accounts
#' @param ... arguments passed on to [get_microsoft365_token()]
#' @details
#' ### Microsoft Outlook
#' 
#' To switch between different Outlook accounts, run [connect_outlook()] with another email address, and set `overwrite = TRUE`. This will allow all `certemail` functions to use the newly set account.
#' 
#' ```r
#' # at default connects to the department mailbox:
#' connect_outlook()
#' # afterwards, this does nothing since `overwrite` is not set:
#' connect_outlook("user@certe.nl")
#' # this switches to the user account:
#' connect_outlook("user@certe.nl", overwrite = TRUE)
#' ```
#' 
#' Using `overwrite` is needed, because running just [connect_outlook()] afterwards again (which many `certemail` functions do) will otherwise change back to the default account.
#' 
#' ### Microsoft Planner
#' 
#' To connect to MS Planner with a personal account, retrieve the plan ID (e.g., from the URL of the plan), and pass it on to [connect_planner()] as `plan`, and set `overwrite = TRUE` over replace an existing connection. Make sure that `team_name` is left blank:
#' 
#' ```r
#' connect_planner(plan = "AAA-0aa0AaAa-aaaAAAAAAAa", team_name = NULL, overwrite = TRUE)
#' ```
#' 
#' ### Microsoft Teams
#' 
#' Connecting to MS Teams can only be done on the group (= team) level. It is not possible to set up a connection without a valid team name.
#' 
#' ### Pre-loaded Settings
#' 
#' When attaching this `certeprojects` package using [library()], and external R process will be run in the background using the `callr` package to connect to MS Outlook, MS Planner, and MS Teams. This will increase speed when connecting using [connect_outlook()], [connect_planner()], or [connect_teams()].
#' @importFrom AzureGraph create_graph_login get_graph_login
#' @rdname connect
#' @name connect
#' @export
get_microsoft365_token <- function(scope,
                                   tenant = read_secret("azure.tenant"),
                                   app_id = read_secret("azure.app_id"),
                                   auth_type = read_secret("azure.auth_type"),
                                   ...,
                                   overwrite = FALSE,
                                   error_on_fail = TRUE) {
  # for the scopes, see here: https://docs.microsoft.com/en-us/graph/permissions-reference
  scope_options <- c("mail", "outlook", "teams", "planner", "tasks", "sharepoint")
  scope <- tolower(scope)[1]
  if (scope == "mail") {
    scope <- "outlook"
  }
  
  if (is.null(tenant) || tenant == "") {
    tenant <- "common"
  }
  if (is.null(app_id) || app_id == "") {
    app_id <- NULL
  }
  if (is.null(auth_type) || auth_type == "") {
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
  } else if (scope == "tasks") { # same as planner, but without the Groups permission for individual use
    scopes <- c("Tasks.Read.Shared",
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

  scope <- paste0("token_", scope)

  if (isTRUE(overwrite) || is.null(suppressMessages(pkg_env[[scope]]))) {
    # not yet connected to Microsoft 365, so set it up
    tryCatch({
      # try to get existing login first - this will prevent that the device code must be given every time
      conn <- try(suppressMessages(get_graph_login(tenant = tenant, app = app_id, scopes = scopes, refresh = FALSE)), silent = TRUE)
      if (inherits(conn, "try-error")) {
        # now create a new login
        conn <- suppressMessages(create_graph_login(tenant = tenant, app = app_id, scopes = scopes, auth_type = auth_type, ...))
      }
      pkg_env[[scope]] <- conn$token
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

#' @rdname connect
#' @importFrom Microsoft365R ms_outlook
#' @export
connect_outlook <- function(email = read_secret("mail.auto_from"),
                            overwrite = FALSE,
                            ...) {
  if (!is.null(pkg_env$outlook) && !isTRUE(overwrite) && !missing(email) && !identical(email, get_azure_property(pkg_env$outlook, "mail"))) {
    warning("Currently connected as ", get_azure_property(pkg_env$outlook, "mail"), " - add `overwrite = TRUE` to switch accounts")
  }
    
  if (isTRUE(overwrite) || is.null(pkg_env$outlook)) {
    # not yet connected to Outlook in Microsoft 365 or using a different account, so set it up
    # check the background callr first
    conn <- tryCatch(pkg_env$callr$get_result()$outlook, error = function(e) NULL)
    if (!isTRUE(overwrite) && !is.null(conn) && inherits(conn, "ms_outlook")) {
      pkg_env$outlook <- conn
      pkg_env$outlook_from_callr <- TRUE
    } else {
      login <- suppressMessages(create_graph_login(token = get_microsoft365_token(scope = "outlook", overwrite = overwrite, ...)))
      pkg_env$outlook <- login$get_user(email = email)$get_outlook()
      pkg_env$outlook_from_callr <- FALSE
    }
  }
  return(invisible(pkg_env$outlook))
}

#' @rdname connect
#' @importFrom Microsoft365R ms_plan
#' @importFrom AzureGraph create_graph_login call_graph_endpoint
#' @export
connect_planner <- function(plan = read_secret("planner.name"), team_name = read_secret("team.name"), overwrite = FALSE, ...) {
  if (isTRUE(overwrite) || is.null(pkg_env$planner)) {
    # not yet connected to Planner in Microsoft 365, so set it up
    if (!is.null(team_name) && team_name != "") {
      # check the background callr first
      conn <- tryCatch(pkg_env$callr$get_result()$planner, error = function(e) NULL)
      if (!isTRUE(overwrite) && !is.null(conn) && inherits(conn, "ms_plan")) {
        login <- conn
        pkg_env$planner_from_callr <- TRUE
      } else {
        login <- suppressMessages(create_graph_login(token = get_microsoft365_token(scope = "planner", overwrite = overwrite, ...)))
        if (!"get_group" %in% names(login)) {
          Sys.sleep(1)
        }
        login <- login$get_group(name = team_name)
        pkg_env$planner_members <- tryCatch(login$list_members(), error = function(e) NULL)
        pkg_env$planner_owners <- tryCatch(login$list_owners(), error = function(e) NULL)
        if (!"get_plan" %in% names(login)) {
          Sys.sleep(1)
        }
        login <- login$get_plan(plan_title = plan)
        pkg_env$planner_from_callr <- FALSE
      }
      pkg_env$planner <- login
    } else {
      # this part is for a personal planner:
      # team_name can be empty, but then plan must be a plan_id
      token <- get_microsoft365_token(scope = "tasks", overwrite = overwrite
, ...)
      res <- call_graph_endpoint(token = token, file.path("planner/plans", plan))
      pkg_env$planner <- ms_plan$new(token, token$tenant, res)
    }
  }
  return(invisible(pkg_env$planner))
}

#' @rdname connect
#' @importFrom Microsoft365R get_team ms_team ms_drive_item
#' @export
connect_teams <- function(team_name = read_secret("team.name"),
                          overwrite = FALSE,
                          ...) {
  if (isTRUE(overwrite) || is.null(pkg_env$teams)) {
    # not yet connected to Teams in Microsoft 365, so set it up
    # check the background callr first
    conn <- tryCatch(pkg_env$callr$get_result()$teams, error = function(e) NULL)
    if (!isTRUE(overwrite) && !is.null(conn) && inherits(conn, "ms_team")) {
      pkg_env$teams <- conn
      pkg_env$teams_from_callr <- TRUE
    } else {
      login <- suppressMessages(create_graph_login(token = get_microsoft365_token(scope = "teams", overwrite = overwrite
, ...)))
      if (!"get_group" %in% names(login)) {
        Sys.sleep(1)
      }
      login <- login$get_group(name = team_name)
      if (!"get_team" %in% names(login)) {
        Sys.sleep(1)
      }
      pkg_env$teams <- login$get_team()
      pkg_env$teams_from_callr <- FALSE
    }
  }
  return(invisible(pkg_env$teams))
}
