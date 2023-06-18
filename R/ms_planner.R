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
#' @param plan_name name of the team's plan
#' @param ... arguments passed on to [get_microsoft365_token()]
#' @param account a Microsoft 365 account to use for looking up properties. This has to be an object as returned by [planner_connect()] or via [AzureGraph::create_graph_login()].
#' @rdname planner
#' @name planner
#' @importFrom AzureGraph create_graph_login
#' @importFrom httr POST PATCH add_headers stop_for_status
#' @export
planner_connect <- function(team_name = read_secret("team.name"), plan_name = read_secret("planner.name"), ...) {
  if (is.null(pkg_env$m365_getplan)) {
    # not yet connected to Planner in Microsoft 365, so set it up
    pkg_env$m365_getplan <- create_graph_login(token = get_microsoft365_token(scope = "planner", ...))$
      get_group(name = team_name)$
      get_plan(plan_title = plan_name)
  }
  # this will auto-renew authorisation when due
  return(invisible(pkg_env$m365_getplan))
}

#' @rdname planner
#' @param bucket_name name of the bucket
#' @export
planner_bucket_create <- function(bucket_name, account = planner_connect()) {
  request_add <- POST(url = "https://graph.microsoft.com/v1.0/planner/buckets",
                      encode = "json",
                      config = add_headers(Authorization = paste(account$token$credentials$token_type,
                                                                 account$token$credentials$access_token),
                                           `Content-type` = "application/json"),
                      body = list(name = bucket_name,
                                  planId = account$properties$id,
                                  orderHint = " !"))
  stop_for_status(request_add, task = paste("add bucket", bucket_name))
}

#' @rdname planner
#' @param plain return as plain names, not as `Azure` objects
#' @export
planner_buckets_list <- function(account = planner_connect(), plain = FALSE) {
  if (plain == TRUE) {
    sort(vapply(FUN.VALUE = character(1), account$list_buckets(), function(x) x$properties$name))
  } else {
    account$list_buckets()
  }
}

#' @rdname planner
#' @export
planner_tasks_list <- function(account = planner_connect(), plain = FALSE) {
  if (plain == TRUE) {
    sort(vapply(FUN.VALUE = character(1), account$list_tasks(), function(x) x$properties$title))
  } else {
    account$list_tasks()
  }
}

#' @rdname planner
#' @export
planner_categories_list <- function(account = planner_connect()) {
  unlist(account$do_operation("details")$categoryDescriptions, use.names = TRUE)
}

#' @rdname planner
#' @param title title of the task
#' @param descr a description for the task. A vector will be add as one text separated by white lines.
#' @param duedate a date to use a due date, use `FALSE` to remove it
#' @param requested_by name of the person(s) who requested the task
#' @param priority a priority to set. Can be ranged between 0 (highest) and 10 (lowest), or: `"urgent"` or `"dringend"` for 1, `"important"` or `"belangrijk"` for 3, `"medium"` or `"gemiddeld"` or `FALSE` for 5, `"low"` or `"laag"` for 9. Priorities cannot be removed - the default setting is 5.
#' @param checklist_items character vector of checklist items
#' @param assigned names of members within the plan. Use `FALSE` to remove all assigned members.
#' @param categories names of categories to add
#' @export
planner_task_create <- function(title,
                                descr = NULL,
                                duedate = NULL,
                                requested_by = NULL,
                                priority = read_secret("planner.default.priority"),
                                checklist_items = NULL,
                                categories = NULL,
                                # comments = NULL,
                                assigned = NULL,
                                bucket_name = read_secret("planner.default.bucket"),
                                account = planner_connect()) {
  # see this for all the possible fields: https://learn.microsoft.com/en-us/graph/api/resources/plannertask?view=graph-rest-1.0
  
  # does not work with Microsoft365R yet, so we do it manually
  body <- list(title = title,
               planId = account$properties$id,
               bucketId  = planner_bucket_object(bucket_name = bucket_name, account = account)$properties$id,
               startDateTime = paste0(format(Sys.Date(), "%Y-%m-%d"), "T00:00:00Z"))
  
  if (!is.null(duedate)) {
    if (isFALSE(duedate)) {
      body <- c(body, list(dueDateTime = NULL))
    } else {
      if (!inherits(duedate, c("Date", "POSIXct"))) {
        stop("duedate is not a valid date")
      }
      body <- c(body, dueDateTime = paste0(format(duedate, "%Y-%m-%d"), "T00:00:00Z"))
    }
  }
  
  if (!is.null(priority)) {
    body <- c(body, list(priority = planner_priority_to_int(priority)))
  }
  
  if (is.null(requested_by)) {
    descr <- c(descr, paste0("Aangevraagd door: ", paste0(requested_by, collapse = " en ")))
  }
  
  # run request:
  body <- toJSON(body, auto_unbox = TRUE, null = "null", pretty = TRUE)
  request_add <- POST(url = "https://graph.microsoft.com/v1.0/planner/tasks",
                      encode = "raw",
                      config = add_headers(Authorization = paste(account$token$credentials$token_type,
                                                                 account$token$credentials$access_token),
                                           `Content-type` = "application/json"),
                      body = body)
  stop_for_status(request_add, task = paste("add task", title, ".\nBody of request:\n\n", body))
  
  # # just like Trello, some propertiescan only be added as update, https://learn.microsoft.com/en-us/graph/api/plannertaskdetails-update
  if (!is.null(descr) || !is.null(categories) || !is.null(checklist_items) || !is.null(assigned)) {
    planner_task_update(title, descr = descr, checklist_items = checklist_items, assigned = assigned, categories = categories)
  }
}

#' @rdname planner
#' @param old_title title of the task to update
#' @param assigned_keep add members that are set in `assigned` instead of replacing them, defaults to `FALSE`
#' @param categories_keep add categories that are set in `categories` instead of replacing them, defaults to `FALSE`
#' @importFrom jsonlite toJSON
#' @export
planner_task_update <- function(old_title,
                                title = NULL,
                                bucket_name = NULL,
                                descr = NULL,
                                duedate = NULL,
                                assigned = NULL,
                                assigned_keep = FALSE,
                                categories = NULL,
                                categories_keep = FALSE,
                                checklist_items = NULL,
                                priority = NULL,
                                # TODO comments = NULL, # these only work with Group.ReadWrite.All
                                account = planner_connect()) {
  # does not work with Microsoft365R yet, so we do it manually
  task <- planner_task_object(task_title = old_title)
  task_id <- task$properties$id
  
  # UPDATE TASK ITSELF ----
  
  body <- list()
  
  if (!is.null(title)) {
    # new title
    body <- c(body, title = title)
    # descr <- c(descr, create_log("Changed title to '", title, "'"))
  }
  
  if (!is.null(duedate)) {
    if (isFALSE(duedate)) {
      body <- c(body, list(dueDateTime = NULL))
      # descr <- c(descr, create_log("Removed due date"))
    } else {
      if (!inherits(duedate, c("Date", "POSIXct"))) {
        stop("duedate is not a valid date")
      }
      body <- c(body, dueDateTime = paste0(format(duedate, "%Y-%m-%d"), "T00:00:00Z"))
      # descr <- c(descr, create_log("Changed due date to ", format(duedate, "%a %d %b %Y")))
    }
  }
  
  if (!is.null(bucket_name)) {
    # new bucket
    body <- c(body, bucketId = planner_bucket_object(bucket_name = bucket_name, account = account)$properties$id)
    # descr <- c(descr, create_log("Moved to bucket '", bucket_name, "'"))
  }
  
  if (!is.null(categories)) {
    apply_categories <- list()
    # these are all existing categories
    categories_current <- names(task$properties$appliedCategories)
    # get internal name of given categories
    categories_new <- get_internal_category(categories)
    # descr <- c(descr, create_log("Added category: ", paste0("'", sort(categories), "'", collapse = " and ")))
    for (category in unique(c(categories_new, categories_current))) {
      if (category %in% categories_new) {
        # add it as defined here: https://learn.microsoft.com/en-us/graph/api/resources/plannerappliedcategories
        apply_categories <- c(apply_categories, stats::setNames(list(TRUE), category))
      } else if (!isTRUE(categories_keep)) {
        # remove it from the current categories
        apply_categories <- c(apply_categories, stats::setNames(list(FALSE), category))
      }
    }
    body <- c(body, list(appliedCategories = apply_categories))
    
  }
  
  if (!is.null(assigned)) {
    apply_assigned <- list()
    # these are all existing assigned
    assigned_current <- names(task$properties$assignments)
    # get internal name of given assigned
    assigned_new <- planner_user_property(assigned)
    # descr <- c(descr, create_log("Assigned user(s) ", paste0(sort(planner_user_property(assigned, property = "name")), collapse = " and ")))
    for (assign in unique(c(assigned_new, assigned_current))) {
      if (assign %in% assigned_new) {
        # add it as defined here: https://learn.microsoft.com/en-us/graph/api/resources/plannerassignments?view=graph-rest-1.0
        apply_assigned <- c(apply_assigned,
                            stats::setNames(list(list(`@odata.type` = "microsoft.graph.plannerAssignment",
                                                      orderHint = " !")),
                                            assign))
      } else if (!isTRUE(assigned_keep) || isFALSE(assigned)) {
        # remove it from the current assigned
        apply_assigned <- c(apply_assigned, stats::setNames(list(NULL), assign))
      }
    }
    body <- c(body, list(assignments = apply_assigned))
  }
  
  if (!is.null(priority)) {
    body <- c(body, list(priority = planner_priority_to_int(priority)))
    # descr <- c(descr, create_log("Changed priority to '", priority, "' (", planner_priority_to_int(priority), ")"))
  }
  
  if (length(body) > 0) {
    # in httr, NULLs will be removed, so we follow this solution: https://github.com/r-lib/httr/issues/561#issuecomment-451278328
    body <- toJSON(body, auto_unbox = TRUE, null = "null", pretty = TRUE)
    # run request:
    request_update <- PATCH(url = paste0("https://graph.microsoft.com/v1.0/planner/tasks/", task_id),
                            encode = "raw",
                            config = add_headers(Authorization = paste(account$token$credentials$token_type,
                                                                       account$token$credentials$access_token),
                                                 # this one is required for updating tasks:
                                                 `If-Match` = task$properties$`@odata.etag`,
                                                 Prefer = "return=representation",
                                                 `Content-type` = "application/json"),
                            body = body)
    stop_for_status(request_update, task = paste("update task", old_title, ".\nBody of request:\n\n", body))
  }
  
  
  # UPDATE TASK DETAILS -----
  
  # the following properties are considered task details and must be updated using .../id/details:
  # checklist, description, previewType, references
  # see https://learn.microsoft.com/en-us/graph/api/plannertaskdetails-update
  
  if (is.null(descr) && is.null(checklist_items)) {
    return(invisible())
  }
  
  body <- list()
  if (!is.null(descr)) {
    body <- c(body, description = paste(descr, collapse = "\n\n"), previewType = "description")
  } else {
    body <- c(body, previewType = "automatic")
  }
  
  if (!is.null(checklist_items)) {
    check_items <- list()
    for (check_item in checklist_items) {
      check_items <- c(check_items, 
                       stats::setNames(list(list(`@odata.type` = "microsoft.graph.plannerChecklistItem",
                                                 title = check_item,
                                                 isChecked = FALSE)),
                                       generate_guids(1)))
    }
    body <- c(body, list(checklist = check_items))
    if (body$previewType == "automatic") {
      body$previewType <- "checklist"
    }
  }
  
  # use a GET to get the correct etag, see https://stackoverflow.com/a/43380424/4575331
  get_task <- GET(url = paste0("https://graph.microsoft.com/v1.0/planner/tasks/", task_id, "/details"),
                  config = add_headers(Authorization = paste(account$token$credentials$token_type,
                                                             account$token$credentials$access_token)))
  # run request:
  body <- toJSON(body, auto_unbox = TRUE, null = "null", pretty = TRUE)
  request_updatedetails <- PATCH(url = paste0("https://graph.microsoft.com/v1.0/planner/tasks/", task_id, "/details"),
                                 encode = "raw",
                                 config = add_headers(Authorization = paste(account$token$credentials$token_type,
                                                                            account$token$credentials$access_token),
                                                      # this one is required for updating tasks:
                                                      `If-Match` = get_task$headers$etag,
                                                      Prefer = "return=representation",
                                                      `Content-type` = "application/json"),
                                 body = body)
  stop_for_status(request_updatedetails, task = paste("update details of task", old_title, ".\nBody of request:\n\n", body))
}


#' @rdname planner
#' @export
planner_task_id <- function(old_title, account = planner_connect()) {
  task <- planner_task_object(task_title = old_title)
  date <- format(as.Date(task$properties$createdDateTime), "%y-%m")
  id_index <- match(strsplit(task$properties$id, "")[[1]], c(0:9, letters[1:26], LETTERS[1:26]))
  paste0(substr(date, 2, 2), substr(date, 5, 5), "-", substr(id_index[1], 1, 1), substr(id_index[2], 1, 1), substr(id_index[3], 1, 1))
}

#' @rdname planner
#' @param category_text text of the category to use
#' @export
planner_task_request_validation <- function(title,
                                            category_text = read_secret("planner.label.authorise"),
                                            account = planner_connect()) {
  task <- planner_task_object(title)
  planner_task_update(title,
                      categories = category_text)
  #,
  # comments = paste0("Validatie aangevraagd\n",
  # "ID taak: ",  task$properties$id,
  # "ID bucket: ", task$properties$bucketId))
}

#' @rdname planner
#' @export
planner_task_validate <- function(title,
                                  category_text = read_secret("planner.label.authorised"),
                                  account = planner_connect()) {
  task <- planner_task_object(title)
  planner_task_update(title,
                      categories = category_text) #,
  # comments = paste0("Geautomatiseeerd gevalideerd\n",
  #                   "ID taak: ",  task$properties$id,
  #                   "ID bucket: ", task$properties$bucketId))
}

#' @rdname planner
#' @param user a user name, mail adress, or Certe login name
#' @param property property to return, can be "id", "name" or "mail"
#' @param as_list return the full list of members as [list], split into Eigenaars (Owners) / Leden (Members). This ignores `user`.
#' @importFrom AzureGraph create_graph_login
#' @importFrom dplyr bind_rows filter pull
#' @export
planner_user_property <- function(user,
                                  team_name = read_secret("team.name"),
                                  account = planner_connect(),
                                  property = "id",
                                  as_list = FALSE) {
  members <- create_graph_login(token = account$token)$get_group(name = team_name)$list_members()
  owners <- character(0)
  if (as_list == TRUE) {
    owners <- vapply(FUN.VALUE = character(1),
                     create_graph_login(token = account$token)$get_group(name = "Medische Epidemiologie")$list_owners(),
                     function(x) x$properties$displayName)
  }
  df <- lapply(members, function(m) {
    data.frame(certe_login = gsub("@certe.nl", "", m$properties$userPrincipalName),
               name = m$properties$displayName,
               mail = m$properties$mail,
               id = m$properties$id,
               role = ifelse(m$properties$displayName %in% owners, "Eigenaar", "Lid"))
  }) |>
    bind_rows() |> 
    filter(name != "Azure Connect")
  
  if (as_list == TRUE) {
    return(list(Eigenaars = sort(df[[property]][which(df$role == "Eigenaar")]),
                Leden = sort(df[[property]][which(df$role == "Lid")])))
  }
  
  users <- character(0)
  for (usr in user) {
    users <- c(users, df[[property]][which(df$certe_login %like% usr | df$name %like% usr | df$mail %like% usr)])
  }
  users
}


planner_bucket_object <- function(bucket_name = read_secret("planner.default.bucket"), account = planner_connect()) {
  # account$get_bucket() does not work yet in Microsoft365R, return error 'Invalid bucket name', so do it manually:
  buckets <- account$list_buckets()
  index <- which(vapply(FUN.VALUE = logical(1), buckets, function(b) b$properties$name == bucket_name))
  if (length(index) == 0) {
    stop("Bucket not found")
  } else {
    buckets[[index[1L]]]
  }
}

planner_task_object <- function(task_title, task_id = NULL, account = planner_connect()) {
  # account$get_bucket() does not work yet in Microsoft365R, return error 'Invalid bucket name', so do it manually:
  tasks <- account$list_tasks()
  index <- which(vapply(FUN.VALUE = logical(1), tasks, function(b) ifelse(!is.null(task_id), 
                                                                          b$properties$id == task_id,
                                                                          b$properties$title == task_title)))
  if (length(index) == 0) {
    stop("Task not found")
  } else {
    tasks[[index[1L]]]
  }
}

get_internal_category <- function(category_name, account = planner_connect()) {
  categories <- planner_categories_list(account = account)
  names(which(categories == category_name))
}

planner_priority_to_int <- function(priority) {
  # from https://learn.microsoft.com/nl-nl/graph/api/plannertask-update?view=graph-rest-1.0&tabs=http:
  # Planner sets the value 1 for "urgent", 3 for "important", 5 for "medium", and 9 for "low".
  # 0 has the highest priority and 10 has the lowest priority
  if (isFALSE(priority)) {
    return(5) # default setting
  } else if (is.numeric(priority) && priority >= 0 && priority <= 10) {
    return(priority)
  } else {
    priority <- trimws(tolower(priority))
    if (priority %in% c("urgent", "dringend")) {
      return(1)
    } else if (priority %in% c("important", "belangrijk", "hoog")) {
      return(3)
    } else if (priority %in% c("medium", "gemiddeld", "normaal")) {
      return(5)
    } else if (priority %in% c("low", "laag")) {
      return(9)
    } else {
      stop("invalid priority: ", priority, call. = FALSE)
    }
  }
}

generate_guids <- function(length) {
  vapply(FUN.VALUE = character(1), seq_len(length), function(x) {
    out <- paste0(sample(c(0:9, letters[1:6]), 30, replace = TRUE), collapse = "")
    paste0(substr(out, 1, 8), "-",
           substr(out, 9, 12), "-4",
           substr(out, 13, 15), "-",
           sample(c("8", "9", "a", "b"), 1),
           substr(out, 16, 18), "-",
           substr(out, 19, 30))
  })
}

create_log <- function(...) {
  paste0(format(Sys.time(), "%Y-%m-%d %H:%M"), ", ", get_current_user(), "/ ", paste0(c(...), collapse = ""))
}
