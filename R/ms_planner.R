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
#' @param plan name of the team's plan if `team_name` is not empty. Otherwise, a plan ID (for individual use).
#' @param ... arguments passed on to [get_microsoft365_token()]
#' @param account a Microsoft 365 account to use for looking up properties. This has to be an object as returned by [planner_connect()] or via [AzureGraph::create_graph_login()]`$get_group(name)$get_plan(plan_title)`.
#' @rdname planner
#' @name planner
#' @importFrom AzureGraph create_graph_login call_graph_endpoint
#' @importFrom Microsoft365R ms_plan
#' @export
planner_connect <- function(plan = read_secret("planner.name"), team_name = read_secret("team.name"), ...) {
  if (is.null(pkg_env$m365_getplan)) {
    # not yet connected to Planner in Microsoft 365, so set it up
    if (!is.null(team_name) && team_name != "") {
      suppressMessages(
        pkg_env$m365_getplan <- create_graph_login(token = get_microsoft365_token(scope = "planner", ...))$
          get_group(name = team_name)$
          get_plan(plan_title = plan)
      )
    } else {
      # team_name can be empty, but then plan must be a plan_id
      token <- get_microsoft365_token(scope = "tasks", ...)
      res <- call_graph_endpoint(token = token, file.path("planner/plans", plan))
      pkg_env$m365_getplan <- ms_plan$new(token, token$tenant, res)
    }
  }
  return(invisible(pkg_env$m365_getplan))
}

#' @rdname planner
#' @param bucket_name name of the bucket
#' @export
planner_browse <- function(account = planner_connect()) {
  utils::browseURL(paste0("https://tasks.office.com/", account$token$tenant, 
                          "/nl-NL/Home/Planner/#/plantaskboard?planId=", account$properties$id))
}

#' @rdname planner
#' @param bucket_name name of the bucket
#' @importFrom httr add_headers
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
#' @param description a description for the task. A vector will be add as one text separated by white lines.
#' @param startdate a date to use as start date, use `FALSE` to remove it. Defaults to today.
#' @param duedate a date to use as due date, use `FALSE` to remove it
#' @param requested_by name of the person(s) who requested the task
#' @param priority a priority to set. Can be ranged between 0 (highest) and 10 (lowest), or: `"urgent"` or `"dringend"` for 1, `"important"` or `"belangrijk"` for 3, `"medium"` or `"gemiddeld"` or `FALSE` for 5, `"low"` or `"laag"` for 9. Priorities cannot be removed - the default setting is 5.
#' @param checklist_items character vector of checklist items
#' @param assigned names of members within the plan - use `NULL` to not add members in [planner_task_create()], and use `FALSE` to remove all existing member in [planner_task_update()]
#' @param categories names of categories to add
#' @param attachment_urls URLs to add as attachment, can be named characters to give the URLs a title
#' @param project_number the new project number to assign. Use `NULL` or `FALSE` to not assign a project number. Defaults to the currently highest project ID + 1.
#' @importFrom httr add_headers stop_for_status POST
#' @importFrom jsonlite toJSON
#' @export
planner_task_create <- function(title,
                                description = NULL,
                                startdate = Sys.Date(),
                                duedate = NULL,
                                requested_by = NULL,
                                priority = read_secret("planner.default.priority"),
                                checklist_items = NULL,
                                categories = NULL,
                                # comments = NULL,
                                assigned = NULL,
                                bucket_name = read_secret("planner.default.bucket"),
                                attachment_urls = NULL,
                                account = planner_connect(),
                                project_number = planner_highest_project_id() + 1) {
  # see this for all the possible fields: https://learn.microsoft.com/en-us/graph/api/resources/plannertask?view=graph-rest-1.0
  
  # assign project ID
  if (!is.null(project_number) && !isFALSE(project_number)) {
    if (!is.numeric(project_number)) {
      stop("project_number must be numeric, or NULL or FALSE")
    }
    title <- paste0(title, " - p", project_number)
  } else {
    project_number <- NULL
  }
  
  # does not work with Microsoft365R yet, so we do it manually
  body <- list(title = title,
               planId = account$properties$id,
               bucketId  = planner_bucket_object(bucket_name = bucket_name, account = account)$properties$id,
               startDateTime = paste0(format(Sys.Date(), "%Y-%m-%d"), "T00:00:00Z"))
  
  if (!arg_is_empty(startdate)) {
    if (isFALSE(startdate)) {
      body <- c(body, list(startDateTime = NULL))
    } else {
      if (!inherits(startdate, c("Date", "POSIXct"))) {
        stop("startdate is not a valid date")
      }
      body <- c(body, startDateTime = paste0(format(startdate, "%Y-%m-%d"), "T00:00:00Z"))
    }
  }
  if (!arg_is_empty(duedate)) {
    if (isFALSE(duedate)) {
      body <- c(body, list(dueDateTime = NULL))
    } else {
      if (!inherits(duedate, c("Date", "POSIXct"))) {
        stop("duedate is not a valid date")
      }
      body <- c(body, dueDateTime = paste0(format(duedate, "%Y-%m-%d"), "T00:00:00Z"))
    }
  }
  
  if (!arg_is_empty(priority)) {
    body <- c(body, list(priority = planner_priority_to_int(priority)))
  }
  
  if (!arg_is_empty(requested_by)) {
    description <- c(paste0("Aangevraagd door: ", paste0(requested_by, collapse = " en "), "."), description)
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
  
  # went well, so increase the highest ID by 1
  if (!is.null(project_number)) {
    increase_highest_project_id(account = account)
  }
  
  # some properties can only be added as update (using PATCH)
  # see https://learn.microsoft.com/en-us/graph/api/plannertaskdetails-update
  if (!arg_is_empty(description) || !arg_is_empty(categories) || !arg_is_empty(checklist_items) ||
      !arg_is_empty(assigned) || !arg_is_empty(attachment_urls)) {
    tsk <- tryCatch(planner_task_find(title), error = function(e) NULL)
    if (is.null(tsk)) {
      # sync the fields
      try(pkg_env$m365_getplan$sync_fields(), silent = TRUE)
      tsk <- tryCatch(planner_task_find(title), error = function(e) NULL)
    }
    if (is.null(tsk)) {
      # still cannot be found yet, wait a sec
      Sys.sleep(1)
    } else {
      title <- tsk
    }
    planner_task_update(title, description = description, checklist_items = checklist_items,
                        assigned = assigned, categories = categories, attachment_urls = attachment_urls)
  }
  
  return(invisible(list(id = project_number, title = title)))
}

#' @rdname planner
#' @param task any task title, task ID, or [`ms_plan_task`][Microsoft365R::ms_plan_task] object (e.g. from [planner_task_find()])
#' @param percent_completed percentage of task completion between 0-100
#' @param assigned_keep add members that are set in `assigned` instead of replacing them, defaults to `FALSE`
#' @param categories_keep add categories that are set in `categories` instead of replacing them, defaults to `FALSE`
#' @importFrom jsonlite toJSON
#' @importFrom httr add_headers stop_for_status PATCH GET
#' @importFrom dplyr case_when
#' @export
planner_task_update <- function(task,
                                title = NULL,
                                description = NULL,
                                startdate = NULL,
                                duedate = NULL,
                                priority = NULL,
                                checklist_items = NULL,
                                categories = NULL,
                                categories_keep = FALSE,
                                assigned = NULL,
                                assigned_keep = FALSE,
                                bucket_name = NULL,
                                percent_completed = NULL,
                                attachment_urls = NULL,
                                # TODO comments = NULL, # these only work with Group.ReadWrite.All
                                account = planner_connect()) {
  # does not work with Microsoft365R yet, so we do it manually
  
  task <- planner_task_find(task)
  task_id <- task$properties$id
  task_title <- task$properties$title
  
  # UPDATE TASK ITSELF ----
  
  body <- list()
  
  if (!arg_is_empty(title)) {
    # new title
    body <- c(body, title = title)
  }
  
  if (!arg_is_empty(startdate)) {
    if (isFALSE(startdate)) {
      body <- c(body, list(startDateTime = NULL))
    } else {
      if (!inherits(startdate, c("Date", "POSIXct"))) {
        stop("startdate is not a valid date")
      }
      body <- c(body, startDateTime = paste0(format(startdate, "%Y-%m-%d"), "T00:00:00Z"))
    }
  }
  if (!arg_is_empty(duedate)) {
    if (isFALSE(duedate)) {
      body <- c(body, list(dueDateTime = NULL))
    } else {
      if (!inherits(duedate, c("Date", "POSIXct"))) {
        stop("duedate is not a valid date")
      }
      body <- c(body, dueDateTime = paste0(format(duedate, "%Y-%m-%d"), "T00:00:00Z"))
    }
  }
  
  if (!arg_is_empty(percent_completed)) {
    if (percent_completed < 1) {
      # if 0.5 was meant as 50%
      percent_completed <- percent_completed * 100
    }
    body <- c(body, percentComplete = as.integer(percent_completed))
  }
  
  if (!arg_is_empty(bucket_name)) {
    # new bucket
    body <- c(body, bucketId = planner_bucket_object(bucket_name = bucket_name, account = account)$properties$id)
  }
  
  if (!arg_is_empty(categories)) {
    apply_categories <- list()
    # these are all existing categories
    categories_current <- names(task$properties$appliedCategories)
    # get internal name of given categories
    categories_new <- get_internal_category(categories)
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
  
  if (!arg_is_empty(assigned)) {
    apply_assigned <- list()
    # these are all existing assigned
    assigned_current <- names(task$properties$assignments)
    # get internal name of given assigned
    assigned_new <- planner_user_property(assigned)
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
  
  if (!arg_is_empty(priority)) {
    body <- c(body, list(priority = planner_priority_to_int(priority)))
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
    stop_for_status(request_update, task = paste("update task", task_title, ".\nBody of request:\n\n", body))
  }
  
  
  # UPDATE TASK DETAILS -----
  
  # the following properties are considered task details and must be updated using .../id/details:
  # checklist, description, previewType, references
  # see https://learn.microsoft.com/en-us/graph/api/plannertaskdetails-update
  
  if (arg_is_empty(description) && arg_is_empty(checklist_items) && arg_is_empty(attachment_urls)) {
    return(invisible())
  }
  
  body <- list()
  if (!arg_is_empty(description)) {
    body <- c(body, description = paste(description, collapse = "\n\n"), previewType = "description")
  } else {
    body <- c(body, previewType = "automatic")
  }
  
  if (!arg_is_empty(checklist_items)) {
    check_items <- list()
    for (check_item in checklist_items) {
      check_items <- c(check_items, 
                       stats::setNames(list(list(`@odata.type` = "microsoft.graph.plannerChecklistItem",
                                                 title = check_item,
                                                 isChecked = FALSE)),
                                       generate_guids(1)))
    }
    if (length(check_items) > 0) {
      body <- c(body, list(checklist = check_items))
      if (body$previewType == "automatic") {
        body$previewType <- "checklist"
      }
    }
  }
  
  if (!arg_is_empty(attachment_urls)) {
    attachment_items <- list()
    for (i in seq_len(length(attachment_urls))) {
      attachment_item <- attachment_urls[i]
      names(attachment_item) <- names(attachment_urls[i])
      type <- case_when(attachment_item %like% "[.]xlsx?$" ~ "Excel",
                        attachment_item %like% "[.]docx?$" ~ "Word",
                        attachment_item %like% "[.]pptx?$" ~ "PowerPoint",
                        TRUE ~ "Other",
      )
      url <- unname(attachment_item)
      alias <- names(attachment_item)
      if (is.null(alias) || alias == "") {
        alias <- gsub("https?://", "", url)
      }
      # 5 characters need to be encoded, see
      # https://learn.microsoft.com/en-us/graph/api/resources/plannerexternalreferences?view=graph-rest-1.0
      url <- gsub("%", "%25", url, fixed = TRUE)
      url <- gsub(".", "%2E", url, fixed = TRUE)
      url <- gsub(":", "%3A", url, fixed = TRUE)
      url <- gsub("@", "%40", url, fixed = TRUE)
      url <- gsub("#", "%23", url, fixed = TRUE)
      attachment_items <- c(attachment_items, 
                            stats::setNames(list(list(`@odata.type` = "microsoft.graph.plannerExternalReference",
                                                      alias = alias,
                                                      type = type)),
                                            url))
    }
    if (length(attachment_items) > 0) {
      body <- c(body, list(references = attachment_items))
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
  stop_for_status(request_updatedetails, task = paste("update details of task", task_title, ".\nBody of request:\n\n", body))
}

#' @rdname planner
#' @param search_term search term, can contain regular expressions
#' @param limit maximum number of tasks to show
#' @details [planner_task_search()] searches the title and description using case-insensitive regular expressions and returns an [`ms_plan_task`][Microsoft365R::ms_plan_task] object. In interactive mode and with multiple hits, a menu will be shown to pick from.
#' @importFrom certestyle font_blue format2
#' @export
planner_task_search <- function(search_term = ".*", limit = Inf, account = planner_connect()) {
  tasks <- planner_tasks_list(account = account, plain = FALSE)
  hits <- vapply(FUN.VALUE = logical(1),
                 tasks,
                 function(x) any(c(x$properties$title, x$do_operation("details")$description) %like% search_term &
                                   x$properties$title != read_secret("planner.dummycard"),
                                 na.rm = TRUE))
  if (sum(hits) == 0) {
    message("No tasks found.")
    return(NA)
  } else if (sum(hits) == 1) {
    return(tasks[[which(hits)]])
  } else {
    tasks <- tasks[hits]
    dates <- as.POSIXct(vapply(FUN.VALUE = double(1), tasks,
                               function(x) if (is.null(x$properties$startDateTime)) {
                                 NA_real_ 
                               } else {
                                 as.POSIXct(gsub("[TZ]", " ", x$properties$startDateTime))
                               }))
    # order on dates
    tasks_sorted <- tasks[order(dates, decreasing = TRUE)]
    dates <- dates[order(dates, decreasing = TRUE)]
    if (!interactive()) {
      message("Multiple results found, assuming task '", tasks_sorted[[1]]$properties$title, "'")
      return(tasks_sorted[[1]])
    } else {
      if (!is.infinite(limit)) {
        if (sum(hits) <= limit) {
          limit <- Inf
        } else {
          tasks_sorted <- tasks_sorted[seq_len(limit)]
          dates <- dates[seq_len(limit)]
        }
      }
      # interactive and more than 1, pick from menu
      titles <- vapply(FUN.VALUE = character(1), tasks_sorted, function(x) x$properties$title)
      bucket_ids <- vapply(FUN.VALUE = character(1), tasks_sorted, function(x) x$properties$bucketId)
      buckets <- planner_buckets_list(account = account)
      buckets <- data.frame(id = vapply(FUN.VALUE = character(1), buckets, function(x) x$properties$id),
                            name = vapply(FUN.VALUE = character(1), buckets, function(x) x$properties$name))
      buckets <- buckets$name[match(bucket_ids, buckets$id)]
      texts <- paste0(font_blue(titles, collapse = NULL),
                      " (", ifelse(is.na(dates), "", paste0(format2(dates, "ddd d mmm yyyy"), ", ")), buckets, ")")
      choice <- utils::menu(texts, graphics = FALSE, 
                            title = paste0(ifelse(is.infinite(limit), "Tasks", paste("First", limit, "tasks")),
                                           " in '", account$properties$title, "' found with '", font_blue(search_term),
                                           "' (0 to Cancel):"))
      if (choice == 0) {
        NA
      } else {
        tasks_sorted[[choice]]
      }
    }
  }
}

#' @rdname planner
#' @param task_title title of the task, will be searched with [`%like%`][certetoolbox::like]
#' @param task_id exact id of the task
#' @details [planner_task_find()] searches task title or ID, and returns an [`ms_plan_task`][Microsoft365R::ms_plan_task] object. It is used internally b a lot of `planner_*` functions, very fast, and does not support interactive use.
#' @importFrom Microsoft365R ms_plan_task
#' @export
planner_task_find <- function(task_title = NULL, task_id = NULL, account = planner_connect()) {
  if (inherits(task_title, "ms_plan_task")) {
    return(task_title)
  }
  if (arg_is_empty(task_title) && arg_is_empty(task_id)) {
    stop("task_title or task_id must be given")
  }
  # account$get_bucket() does not work yet in Microsoft365R, returns error 'Invalid bucket name', so do it manually:
  tasks <- account$list_tasks()
  out <- which(vapply(FUN.VALUE = logical(1), tasks, function(b) ifelse(!arg_is_empty(task_id), # this prioritises ID over title
                                                                        b$properties$id == task_id,
                                                                        b$properties$title %like% task_title)))
  if (length(out) == 0) {
    stop("Task not found: '", task_title, "'", call. = FALSE)
  } else if (length(out) == 1) {
    return(tasks[[out[1]]])
  } else {
    titles <- vapply(FUN.VALUE = character(1), tasks[out], function(t) t$properties$title)
    if (any(titles == task_title)) {
      return(tasks[[out[which(titles == task_title)]]])
    } else {
      out <- tasks[[out[1]]]
      message("Assuming task '", out$properties$title,  "' for searching with text '", task_title, "'")
      return(out)
    }
  }
}

#' @rdname planner
#' @details [planner_retrieve_project_id()] retrieves the p-number from the task title and returns it as [integer].
#' @export
planner_retrieve_project_id <- function(task, account = planner_connect()) {
  task <- planner_task_find(task)
  title <- task$properties$title
  if (title %like% "p[0-9]+") {
    as.integer(gsub(".*p([0-9]+).*", "\\1", title))
  } else {
    NA_integer_
  }
}

#' @rdname planner
#' @param category_text text of the category to use
#' @export
planner_task_request_validation <- function(task,
                                            category_text = read_secret("planner.label.authorise"),
                                            account = planner_connect()) {
  planner_task_update(task, categories = category_text, account = account)
}

#' @rdname planner
#' @export
planner_task_validate <- function(task,
                                  category_text = read_secret("planner.label.authorised"),
                                  account = planner_connect()) {
  planner_task_update(task, categories = category_text, account = account)
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

#' @rdname planner
#' @details [planner_highest_project_id()] retrieves the currently highest project ID from the dummy card.
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET add_headers stop_for_status content
#' @export
planner_highest_project_id <- function(task = read_secret("planner.dummycard"),
                                       account = planner_connect()) {
  # this returns the currently highest project number, which is saved to the description
  task <- planner_task_find(task)
  task_id <- task$properties$id
  task_title <- task$properties$title
  
  get_task <- GET(url = paste0("https://graph.microsoft.com/v1.0/planner/tasks/", task_id, "/details"),
                  config = add_headers(Authorization = paste(account$token$credentials$token_type,
                                                             account$token$credentials$access_token)))
  stop_for_status(get_task, task = paste("getting task", task_title))
  response_body <- get_task |> 
    content(type = "text", encoding = "UTF-8") |>
    fromJSON(flatten = TRUE)
  
  as.integer(response_body$description)
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

increase_highest_project_id <- function(task = read_secret("planner.dummycard"),
                                        account = planner_connect()) {
  highest <- planner_highest_project_id(task = task, account = account)
  planner_task_update(task = task, description = highest + 1, account = account)
}

get_internal_category <- function(category_name, account = planner_connect()) {
  categories <- planner_categories_list(account = account)
  names(which(categories == category_name))
}

planner_priority_to_int <- function(priority) {
  # from https://learn.microsoft.com/nl-nl/graph/api/plannertask-update?view=graph-rest-1.0&tabs=http:
  # Planner sets the value 1 for "urgent", 3 for "important", 5 for "medium", and 9 for "low".
  # 0 has the highest priority and 10 has the lowest priority
  priority <- priority[1]
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
  # required for checklists, they must each have a unique ID in the format of valid GUIDs
  # didn't know the format was so strict, but luckily ChatGPT did - its script:
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

arg_is_empty <- function(x) {
  is.null(x) || all(is.na(x))
}
