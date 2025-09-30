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

#' Validation and Authorisation
#' 
#' These functions handle validation and authorisation of files stored on a SharePoint site via the Microsoft Graph API, updating corresponding metadata fields and optionally interacting with Microsoft Planner for workflow management.
#' @param drive_item a Drive Item object of a file
#' @param local_file a location of the local file. If set, this will allow a direct Positron link on the attachment of the Planner task.
#' @param user a Certe user, who is member of the SharePoint site where `file` is stored
#' @param authorise_request option to request for authorisation after validation
#' @param teams a Teams account, e.g. outcome of [connect_teams()]
#' @param planner a Planner account, e.g. outcome of [connect_planner()]
#' @details
#' The functions in this set operate on Microsoft 365 Drive Items (class `ms_drive_item`) and are designed to work within a controlled document workflow involving SharePoint and Planner:
#' 
#' - `validate_request_file()` creates a label on a Planner task requesting validation for a file.
#'
#' - `validate_file()` writes a timestamp and user ID to the file's metadata to record validation, using the authenticated Teams token. If `authorise_request = TRUE`, it also triggers a request for authorisation by labelling a task in Planner associated with the parent folder of the file.
#'
#' - `authorise_file()` writes a timestamp and user ID to the file's metadata to reflect authorisation of the file. It also labels the associated Planner task to indicate successful authorisation.
#'
#' All API calls use the Microsoft Graph API and require appropriate OAuth tokens via Teams or Planner accounts. The structure assumes SharePoint lists are configured with custom metadata columns for validation and authorisation tracking.
#' @importFrom httr PATCH add_headers stop_for_status
#' @importFrom jsonlite toJSON
#' @name validation_authorisation
#' @rdname validation_authorisation
#' @export
validate_request_file <- function(drive_item, local_file = NULL, planner = connect_planner()) {
  if (!inherits(drive_item, "ms_drive_item")) {
    stop("`drive_item` must be a Drive Item")
  }
  if (is.null(local_file)) {
    planner_task_request_validation(task = drive_item$get_parent_folder()$properties$name,
                                    attachment_urls = c("SharePoint-map" = drive_item$get_parent_folder()$properties$webUrl),
                                    account = planner)
  } else {
    planner_task_request_validation(task = drive_item$get_parent_folder()$properties$name,
                                    attachment_urls = c("SharePoint-map" = drive_item$get_parent_folder()$properties$webUrl),
                                    checklist_items = paste0("positron://", local_file),
                                    account = planner)
  }
  message("Verzoek tot validatie van '", drive_item$properties$name, "' aangemaakt.")
}

#' @rdname validation_authorisation
#' @export
validate_file <- function(drive_item,
                          user = Sys.info()["user"],
                          authorise_request = FALSE,
                          teams = connect_teams(),
                          planner = connect_planner()) {
  if (!inherits(drive_item, "ms_drive_item")) {
    stop("`drive_item` must be a Drive Item")
  }
  site_id <- drive_item$properties$parentReference$siteId
  drive_id <- drive_item$properties$parentReference$driveId
  item_id <- drive_item$properties$id
  user_lookup_id <- get_sharepoint_lookup_id(site_id, user, teams)
  
  fields <- list(
    Gevalideerd = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    GevalideerddoorLookupId = user_lookup_id
  )
  res <- PATCH(url = paste0("https://graph.microsoft.com/v1.0/sites/", site_id,
                            "/drives/", drive_id,
                            "/items/", item_id,
                            "/listItem/fields"),
               encode = "json",
               config = add_headers(
                 Authorization = paste(teams$token$credentials$token_type,
                                       teams$token$credentials$access_token),
                 `Content-type` = "application/json"
               ),
               body = toJSON(fields, auto_unbox = TRUE)
  )
  stop_for_status(res, task = paste0("validate '", drive_item$properties$name, "'"))
  message("Bestand '", drive_item$properties$name, "' gevalideerd in SharePoint.")
  if (authorise_request == TRUE) {
    planner_task_request_authorisation(task = drive_item$get_parent_folder()$properties$name, account = planner)
    message("Taak '", drive_item$get_parent_folder()$properties$name, "' gelabeld voor autorisatie.")
  } else {
    # remove from 'Valideren' bucket
    planner_task_update(task = drive_item$get_parent_folder()$properties$name,
                        bucket_name = read_secret("planner.default.bucket.consult"),
                        account = planner)
    message("Taak '", drive_item$get_parent_folder()$properties$name, "' verplaatst naar '", read_secret("planner.default.bucket.consult"), "'.")
  }
}

#' @rdname validation_authorisation
#' @export
authorise_file <- function(drive_item,
                           user = Sys.info()["user"],
                           teams = connect_teams(),
                           planner = connect_planner()) {
  if (!inherits(drive_item, "ms_drive_item")) {
    stop("`drive_item` must be a Drive Item")
  }
  if (!user %in% read_secret("sharepoint.authorisation.eligible")) {
    stop("Gebruiker '", user, "' heeft niet voldoende rechten. Gebruiker met rechten: ", toString(read_secret("sharepoint.authorisation.eligible")), ".")
  }
  site_id <- drive_item$properties$parentReference$siteId
  drive_id <- drive_item$properties$parentReference$driveId
  item_id <- drive_item$properties$id
  user_lookup_id <- get_sharepoint_lookup_id(site_id, user, teams)
  
  fields <- list(
    Autorisatie = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    AutorisatiedoorLookupId = user_lookup_id
  )
  res <- PATCH(url = paste0("https://graph.microsoft.com/v1.0/sites/", site_id,
                            "/drives/", drive_id,
                            "/items/", item_id,
                            "/listItem/fields"),
               encode = "json",
               config = add_headers(
                 Authorization = paste(teams$token$credentials$token_type,
                                       teams$token$credentials$access_token),
                 `Content-type` = "application/json"
               ),
               body = toJSON(fields, auto_unbox = TRUE)
  )
  stop_for_status(res, task = paste0("authorise '", drive_item$properties$name, "'"))
  message("Bestand '", drive_item$properties$name, "' geautoriseerd in SharePoint.")
  planner_task_authorise(task = drive_item$get_parent_folder()$properties$name,
                         bucket_name = read_secret("planner.default.bucket.consult"),
                         account = planner)
  message("Taak '", drive_item$get_parent_folder()$properties$name, "' gelabeld als geautoriseerd en verplaatst naar '", read_secret("planner.default.bucket.consult"), "'.")
}

#' @importFrom httr GET add_headers stop_for_status content
get_sharepoint_lookup_id <- function(site_id, certe_login_number, account) {
  user_principal <- paste0(certe_login_number, "@certe.nl")
  
  # Step 1: Get all lists, locate the 'users' list
  res_lists <- GET(
    url = paste0("https://graph.microsoft.com/v1.0/sites/", site_id, "/lists?select=id,name,system"),
    config = add_headers(
      Authorization = paste(account$token$credentials$token_type,
                            account$token$credentials$access_token),
      `Content-type` = "application/json"
    )
  )
  stop_for_status(res_lists)
  lists <- content(res_lists, as = "parsed")$value
  users_list <- Filter(function(l) tolower(l$name) == "users", lists)[[1]]
  users_list_id <- users_list$id
  
  # Step 2: Retrieve all user items from the users list
  res_users <- GET(
    url = paste0("https://graph.microsoft.com/v1.0/sites/", site_id,
                 "/lists/", users_list_id,
                 "/items?$select=Fields&$expand=Fields"),
    config = add_headers(
      Authorization = paste(account$token$credentials$token_type,
                            account$token$credentials$access_token),
      `Content-type` = "application/json"
    )
  )
  stop_for_status(res_users)
  users <- content(res_users, as = "parsed")$value
  
  # Step 3: Match UserName
  usernames <- vapply(FUN.VALUE = character(1), users, function(x) if (is.null(x$fields$UserName)) NA_character_ else x$fields$UserName[1])
  lookup_ids <- vapply(FUN.VALUE = character(1), users, function(x) if (is.null(x$fields$UserName)) NA_character_ else x$fields$id[1])
  lookup_ids <- lookup_ids[!is.na(usernames)]
  usernames <- usernames[!is.na(usernames)]
  if (!user_principal %in% usernames) stop("User '", user_principal, "'not found in SharePoint site '", account$get_sharepoint_site()$properties$displayName, "'")
  lookup_ids[usernames == user_principal][1]
}
