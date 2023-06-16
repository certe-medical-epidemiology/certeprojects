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

#' Connect to Microsoft Planner via Microsoft 365
#'
#' These functions create a connection to Microsoft Planner via Microsoft 365 and saves the connection to the `certeprojects` package environment. The `planner_*()` functions allow to work with this connection.
#' @param team_name name of the team
#' @param ... arguments passed on to [get_microsoft365_token()]
#' @param account a Microsoft 365 account to use for looking up properties. This has to be an object as returned by [planner_connect()] or via [AzureGraph::create_graph_login()].
#' @rdname planner
#' @name planner
#' @importFrom AzureGraph create_graph_login
#' @export
#' @examples 
#' \dontrun{
#' 
#' planner_upload("myfile.docx", channel = "My Channel")
#' planner_upload("myfile.docx", "my channel/my folder/test.docx")
#' 
#' # also supports data frames, they will be saved locally in a temp folder
#' planner_upload(mtcars, channel = "My Channel")
#' mtcars |> 
#'   planner_upload(channel = "My Channel")
#'   
#'   
#' # integrates with department projects:
#' 
#' # upload "myfile.docx" from the project folder of p123 to "My Channel":
#' planner_upload("myfile.docx", channel = "My Channel", card_number = 123)
#' # download "myfile.docx" from "My Channel" to the project folder of p123:
#' planner_download("myfile.docx", channel = "My Channel", card_number = 123)
#' planner_download("mychannel/myfile.docx", card_number = 123)
#' 
#' # direct import from Teams requires the 'certetoolbox' package
#' x <- planner_import("mychannel/myfile.docx")
#' x <- planner_import("mychannel/myfile.docx", card_number = 123)
#' x <- planner_import("myfile.docx", channel = "My Channel", card_number = 123)
#' 
#' # open a file in Excel Online
#' planner_open("test.xlsx", "My Channel")
#' planner_open("my channel/test.xlsx") # shorter version, tries to find channel
#' }
planner_connect <- function(team_name = read_secret("teams.name"), plan_name = read_secret("planner.name"), ...) {
  if (is.null(pkg_env$m365_getplans)) {
    # not yet connected to Teams in Microsoft 365, so set it up
    pkg_env$m365_getplans <- create_graph_login(token = get_microsoft365_token(scope = "planner", ...))$
      get_group(name = team_name)$
      get_plan(plan_title = plan_name)
  }
  # this will auto-renew authorisation when due
  return(invisible(pkg_env$m365_getplans))
}

#' @rdname planner
#' @param bucket_name name of the bucket
#' @export
planner_get_bucket <- function(bucket_name = read_secret("planner.default.bucket"), account = planner_connect()) {
  # account$get_bucket() does not work yet in Microsoft365R, return error 'Invalid bucket name', so do it manually:
  buckets <- account$list_buckets()
  buckets[[which(vapply(FUN.VALUE = logical(1), buckets, function(b) b$properties$name == bucket_name))]]
}

#' #' @rdname planner
#' #' @export
#' planner_create_bucket <- function(bucket_name, account = planner_connect()) {
#'   
#' }

#' @rdname planner
#' @param task_title title of the task
#' @param task_id ID of the task
#' @export
planner_get_task <- function(task_title, task_id = NULL, account = planner_connect()) {
  # account$get_bucket() does not work yet in Microsoft365R, return error 'Invalid bucket name', so do it manually:
  tasks <- account$list_tasks()
  if (!is.null(task_id)) {
    tasks[[which(vapply(FUN.VALUE = logical(1), tasks, function(b) b$properties$id == task_id))]]
  } else {
    tasks[[which(vapply(FUN.VALUE = logical(1), tasks, function(b) b$properties$title == task_title))]]
  }
}

#' @rdname planner
#' @param title,descr,due,assigned properties of the new task
#' @importFrom httr POST add_headers stop_for_status
#' @export
planner_create_task <- function(title,
                                descr = "",
                                due = NULL,
                                assigned = NULL,
                                bucket_name = read_secret("planner.default.bucket"),
                                account = planner_connect()) {
  # does not work with Microsoft365R yet, so we do it manually
  request_add <- POST(url = "https://graph.microsoft.com/v1.0/planner/tasks",
                      encode = "json",
                      config = add_headers(Authorization = paste(account$token$credentials$token_type,
                                                                 account$token$credentials$access_token)),
                      body = list(title = title,
                                  planId = account$properties$id,
                                  bucketId  = planner_get_bucket(bucket_name = bucket_name, account = account)$properties$id))
  stop_for_status(request_add, task = paste("add task", title))
}
