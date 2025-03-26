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


# EXPORTED FUNCTIONS ------------------------------------------------------

#' Connect to Microsoft Planner via Microsoft 365
#'
#' These functions use the connection to Microsoft Planner set up with [connect_planner()].
#' @param team_name name of the team, can be left blank to connect to an individual planner
#' @param account a Microsoft 365 account to use for looking up properties. This has to be an object as returned by [connect_planner()] or via [AzureGraph::create_graph_login()]`$get_group(name)$get_plan(plan_title)`.
#' @param bucket_name name of the bucket
#' @rdname planner
#' @name planner
#' @export
planner_browse <- function(account = connect_planner()) {
  utils::browseURL(planner_url(account = account))
}

planner_url <- function(account = connect_planner()) {
  paste0("https://tasks.office.com/", account$token$tenant, 
         "/nl-NL/Home/Planner/#/plantaskboard?planId=",
         get_azure_property(account, "id"))
}

#' @rdname planner
#' @param bucket_name name of the bucket
#' @importFrom httr add_headers
#' @export
planner_bucket_create <- function(bucket_name, account = connect_planner()) {
  request_add <- POST(url = "https://graph.microsoft.com/v1.0/planner/buckets",
                      encode = "json",
                      config = add_headers(Authorization = paste(account$token$credentials$token_type,
                                                                 account$token$credentials$access_token),
                                           `Content-type` = "application/json"),
                      body = list(name = bucket_name,
                                  planId = get_azure_property(account, "id"),
                                  orderHint = " !"))
  stop_for_status(request_add, task = paste("add bucket", bucket_name))
}

#' @rdname planner
#' @param plain return as plain names, not as `Azure` objects
#' @export
planner_buckets_list <- function(account = connect_planner(), plain = FALSE) {
  if (plain == TRUE) {
    sort(get_azure_property(account$list_buckets(), "name"))
  } else {
    account$list_buckets()
  }
}


#' @rdname planner
#' @param title title of the task
#' @param description a description for the task. A vector of length > 1 will be added as one text separated by white lines.
#' @param startdate a date to use as start date, use `FALSE` to remove it
#' @param duedate a date to use as due date, use `FALSE` to remove it
#' @param requested_by name of the person(s) who requested the task, this will be added as first line to `description`
#' @param priority a priority to set. Can be ranged between 0 (highest) and 10 (lowest), or: `"urgent"` or `"dringend"` for 1, `"important"` or `"belangrijk"` for 3, `"medium"` or `"gemiddeld"` or `FALSE` for 5, `"low"` or `"laag"` for 9. Priorities cannot be removed - the default setting is 5.
#' @param checklist_items character vector of checklist items
#' @param assigned names of members within the plan - use `NULL` to not add members in [planner_task_create()], and use `FALSE` to remove all existing members in [planner_task_update()]
#' @param categories names of categories to add, can be multiple, but must exactly match existing category names
#' @param attachment_urls URLs to add as attachment, can be named characters to give the URLs a title. If they are Excel, PowerPoint or Word files, a preview will be shown on the task.
#' @param project_number the new project number to assign. Use `NULL` or `FALSE` to not assign a project number. Defaults to the currently highest project ID + 1.
#' @param consult_number the new consult number to assign. Use `NULL` or `FALSE` to not assign a consult number. Defaults to the currently highest consult ID + 1.
#' @importFrom httr add_headers stop_for_status POST
#' @importFrom jsonlite toJSON
#' @export
planner_task_create <- function(title,
                                description = NULL,
                                startdate = NULL,
                                duedate = NULL,
                                requested_by = NULL,
                                priority = read_secret("planner.default.priority"),
                                checklist_items = NULL,
                                categories = NULL,
                                # comments = NULL,
                                assigned = NULL,
                                bucket_name = read_secret("planner.default.bucket"),
                                attachment_urls = NULL,
                                account = connect_planner(),
                                project_number = planner_highest_project_id() + 1,
                                consult_number = planner_highest_consult_id() + 1) {
  # see this for all the possible fields: https://learn.microsoft.com/en-us/graph/api/resources/plannertask?view=graph-rest-1.0
  
  # assign project ID or consult ID
  if (!is.null(project_number) && !isFALSE(project_number)) {
    if (!is.numeric(project_number)) {
      stop("project_number must be numeric, or NULL or FALSE")
    }
    title <- paste0(title, " - p", project_number)
    consult_number <- NULL
  } else if (!is.null(consult_number) && !isFALSE(consult_number)) {
    if (!is.numeric(consult_number)) {
      stop("consult_number must be numeric, or NULL or FALSE")
    }
    title <- paste0(title, " - c", consult_number)
    project_number <- NULL
  } else {
    project_number <- NULL
    consult_number <- NULL
  }
  
  if (!arg_is_empty(categories) && isTRUE("Project" %in% categories)) {
    # projects must contain valid file names, since they have an accompanying folder
    invalid_characters <- c("/", "\\", ":", "*", "?", "\"", "<", ">", "|")
    old_title <- title
    title <- gsub(" +", " ", gsub(paste0("[" , paste0(invalid_characters, collapse = "") , "]"), " ", title))
    if (old_title != title) {
      warning("Since this will be a project that requires a valid project folder name, the title was renamed to '", title, "'", immediate. = TRUE, call. = FALSE)
    }
  }
  
  if (title %in% planner_tasks_list(plain = TRUE, account = account)) {
    stop("Task '", title, "' already exists. Make sure to keep task titles unique.", call. = FALSE)
  }
  
  # does not work with Microsoft365R yet, so we do it manually
  body <- list(title = title,
               planId = get_azure_property(account, "id"),
               bucketId  = planner_bucket_object(bucket_name = bucket_name, account = account) |> get_azure_property("id"))
  
  if (inherits(startdate, c("Date", "POSIXct")) && inherits(duedate, c("Date", "POSIXct"))) {
    if (startdate > duedate) {
      stop("'startdate' cannot be later than 'duedate'")
    }
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
  
  if (!arg_is_empty(priority)) {
    body <- c(body, list(priority = planner_priority_to_int(priority)))
  }
  
  if (!arg_is_empty(requested_by)) {
    if (!is.null(project_number)) {
      description <- c(paste0("Aangevraagd door: ", paste0(requested_by, collapse = " en "), "."), description)
    } else {
      description <- c(paste0("Van ", paste0(requested_by, collapse = " en "), "."), description)
    }
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
  } else if (!is.null(consult_number)) {
    increase_highest_consult_id(account = account)
  }
  
  # some properties can only be added as update (using PATCH)
  # see https://learn.microsoft.com/en-us/graph/api/plannertaskdetails-update
  if (!arg_is_empty(description) || !arg_is_empty(categories) || !arg_is_empty(checklist_items) ||
      !arg_is_empty(assigned) || !arg_is_empty(attachment_urls)) {
    tsk <- tryCatch(planner_task_find(title, account = account), error = function(e) NULL)
    if (is.null(tsk)) {
      # sync the fields
      try(account$sync_fields(), silent = TRUE)
      tsk <- tryCatch(planner_task_find(title, account = account), error = function(e) NULL)
    }
    if (is.null(tsk)) {
      # still cannot be found yet, wait a sec
      Sys.sleep(1)
    } else {
      title <- tsk
    }
    planner_task_update(title, description = description, checklist_items = checklist_items,
                        assigned = assigned, categories = categories, attachment_urls = attachment_urls,
                        account = account)
  }
  if (inherits(title, "ms_plan_task")) {
    title <- get_azure_property(title, property = "title")
  }
  
  return(invisible(list(id = if (!is.null(project_number)) project_number else consult_number, title = title)))
}


#' @rdname planner
#' @param ... arguments passed on to [planner_task_update()]
#' @export
planner_create_projecttask <- function(project_number,
                                       title,
                                       bucket_name = read_secret("planner.default.bucket.projecttask"),
                                       ...) {
  if (is.numeric(project_number) || !substr(project_number, 1, 1) == "p") {
    project_number <- paste0("p", project_number)
  }
  
  task <- planner_task_find(paste0(" - ", project_number, "$"))

  title <- paste0("[", project_number, "] ", title)
  
  due <- get_azure_property(task, "dueDateTime")
  if (!is.na(due)) {
    due <- as.Date(substr(due, 1, 10))
    if (due < Sys.Date()) {
      due <- Sys.Date()
    }
  } else {
    due <- NULL
  }
  
  planner_task_create(title = title,
                      startdate = Sys.Date(),
                      description = paste0("Project: ", task$properties$title),
                      duedate = due,
                      assigned = names(get_azure_property(task, "assignments")),
                      bucket_name = bucket_name,
                      categories = "Projecttaak",
                      project_number = NULL,
                      consult_number = NULL,
                      ...)
  
}

#' @rdname planner
#' @export
planner_checklist_to_projectasks <- function(task, 
                                             bucket_name = read_secret("planner.default.bucket.projecttask"),
                                             ...) {
  task <- planner_task_find(task)
  if (task$properties$activeChecklistItemCount == 0) {
    return(invisible)
  }

  project_id <- planner_retrieve_project_id(task)
  details <- task$do_operation("details")
  checklist <- details$checklist
  checklist <- checklist[which(vapply(FUN.VALUE = logical(1), checklist, function(x) !x$isChecked))]
  titles <- sort(unname(vapply(FUN.VALUE = character(1), checklist, function(x) x$title)))
  
  for (i in seq_along(titles)) {
    planner_create_projecttask(project_number = project_id,
                               title = titles[i],
                               bucket_name = bucket_name,
                               ...)
    
  }
}


#' @rdname planner
#' @param task any task title, task ID, or [`ms_plan_task`][Microsoft365R::ms_plan_task] object (e.g. from [planner_task_find()])
#' @param checklist_items_to_check names/titles of checklist items that need to become checked
#' @param percent_completed percentage of task completion between 0-100
#' @param assigned_keep add members that are set in `assigned` instead of replacing them, defaults to `FALSE`
#' @param categories_keep add categories that are set in `categories` instead of replacing them, defaults to `FALSE`
#' @param preview_type type of preview - the checkbox on a task card. Can be `"automatic"`, `"noPreview"`, `"checklist"`, `"description"`, `"reference"`. When set to `"automatic"` the displayed preview is chosen by the app viewing the task.
#' @param order_hint order of the task
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
                                checklist_items_to_check = NULL,
                                categories = NULL,
                                categories_keep = FALSE,
                                assigned = NULL,
                                assigned_keep = FALSE,
                                bucket_name = NULL,
                                percent_completed = NULL,
                                attachment_urls = NULL,
                                preview_type = NULL,
                                order_hint = NULL,
                                # TODO comments = NULL, # these only work with Group.ReadWrite.All
                                account = connect_planner()) {
  # does not work with Microsoft365R yet, so we do it manually
  
  task <- planner_task_find(task)
  task_id <- get_azure_property(task, "id")
  task_title <- get_azure_property(task, "title")
  task_details <- task$do_operation("details")
  
  # Update Task Itself ----
  
  body <- list()
  
  if (!arg_is_empty(title)) {
    # new title
    body <- c(body, title = title)
  }
  
  if (inherits(startdate, c("Date", "POSIXct")) && inherits(duedate, c("Date", "POSIXct"))) {
    if (startdate > duedate) {
      stop("'startdate' cannot be later than 'duedate'")
    }
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
    body <- c(body, bucketId = planner_bucket_object(bucket_name = bucket_name, account = account) |> get_azure_property("id"))
  }
  
  if (!arg_is_empty(order_hint)) {
    body <- c(body, orderHint = order_hint)
  }
  
  if (!arg_is_empty(categories)) {
    apply_categories <- list()
    # these are all existing categories
    categories_current <- names(get_azure_property(task, "appliedCategories"))
    # get internal name of given categories
    categories_new <- get_internal_category_name(categories)
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
    assigned_current <- names(get_azure_property(task, "assignments"))
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
                                                 `If-Match` = get_azure_property(task, "@odata.etag"),
                                                 Prefer = "return=representation",
                                                 `Content-type` = "application/json"),
                            body = body)
    stop_for_status(request_update, task = paste("update task", task_title, ".\nBody of request:\n\n", body))
  }
  
  
  # Update Task Details -----
  
  # the following properties are considered task details and must be updated using .../id/details:
  # checklist, description, previewType, references
  # see https://learn.microsoft.com/en-us/graph/api/plannertaskdetails-update
  
  if (arg_is_empty(description) && arg_is_empty(checklist_items) && arg_is_empty(checklist_items_to_check) && arg_is_empty(attachment_urls) && arg_is_empty(preview_type)) {
    return(invisible())
  }
  
  body <- list()
  if (!arg_is_empty(description)) {
    body <- c(body, description = paste(description, collapse = "\n"), previewType = "description")
  } else {
    body <- c(body, previewType = "automatic")
  }
  
  check_items <- NULL
  if (!arg_is_empty(checklist_items)) {
    # reverse the order, since the first item in the vector will otherwise become last on the task
    checklist_items <- rev(checklist_items)
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
  
  if (!arg_is_empty(checklist_items_to_check)) {
    if (is.null(check_items)) {
      check_items <- task_details$checklist
      for (i in seq_along(check_items)) {
        check_items[[i]] <- list(`@odata.type` = "microsoft.graph.plannerChecklistItem",
                                 title = check_items[[i]]$title,
                                 isChecked = check_items[[i]]$isChecked)
      }
    }
    for (i in seq_along(check_items)) {
      if (check_items[[i]]$title %in% checklist_items_to_check) {
        check_items[[i]]$isChecked <- TRUE
      }
    }
    body <- c(body, list(checklist = check_items))
  }
  
  if (!arg_is_empty(attachment_urls)) {
    attachment_items <- list()
    attachment_types <- case_when(attachment_urls %like% "[.]xlsx?$" ~ "Excel",
                                  attachment_urls %like% "[.]docx?$" ~ "Word",
                                  attachment_urls %like% "[.]pptx?$" ~ "PowerPoint",
                                  TRUE ~ "Other")
    for (i in seq_len(length(attachment_urls))) {
      attachment_item <- attachment_urls[i]
      names(attachment_item) <- names(attachment_urls[i])
      
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
                                                      type = attachment_types[i])),
                                            url))
    }
    if (length(attachment_items) > 0) {
      body <- c(body, list(references = attachment_items))
      if (any(attachment_types != "Other")) {
        # show the attachments, such as PowerPoint files
        body$previewType <- "reference"
      } else if (body$previewType == "automatic") {
        # if the preview is automatic and all are website, preview type will turn to "reference" to show the website, we don't want that
        body$previewType <- "noPreview"
      }
    }
  }
  
  if (!arg_is_empty(preview_type)) {
    # force preview type
    body$previewType <- preview_type
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
#' @export
planner_tasks_list <- function(account = connect_planner(),
                               plain = FALSE,
                               include_completed = TRUE) {
  tasks <- account$list_tasks()
  tasks <- tasks[get_azure_property(tasks, "title") != read_secret("planner.dummy.project")]
  if (isFALSE(include_completed)) {
    tasks <- tasks[get_azure_property(tasks, "percentComplete") < 100]
  }
  if (plain == TRUE) {
    sort(get_azure_property(tasks, "title"))
  } else {
    tasks
  }
}

#' @rdname planner
#' @param search_term search term, can contain a regular expression. When searching for project numbers (such as "p201 - Some text", or "p201" or "201"), only titles will be searched for the project number.
#' @param limit maximum number of tasks to show
#' @param include_completed also search completed tasks
#' @param include_description also search the description, which requires additional queries and lowers speed
#' @details [planner_task_search()] searches the title and description using case-insensitive regular expressions and returns an [`ms_plan_task`][Microsoft365R::ms_plan_task] object. In interactive mode and with multiple hits, a menu will be shown to pick from.
#' @importFrom certestyle font_blue format2
#' @importFrom dplyr bind_rows arrange desc
#' @importFrom rstudioapi isAvailable
#' @export
planner_task_search <- function(search_term = ".*",
                                limit = 50,
                                include_completed = TRUE,
                                include_description = FALSE,
                                account = connect_planner()) {
  if (inherits(search_term, "ms_plan_task")) {
    return(search_term)
  } else {
    search_term <- as.character(search_term)
  }
  if (is.null(search_term) || length(search_term) == 0 || search_term %in% c(NA, "")) {
    search_term <- ".*"
  }
  tasks <- planner_tasks_list(account = account,
                              plain = FALSE,
                              include_completed = include_completed)
  
  search_parts <- unlist(strsplit(as.character(search_term), "[^a-zA-Z0-9]"))
  if (any(search_parts %like% "^p[0-9]+$", na.rm = TRUE)) {
    # term contains project number, only keep that
    search_term <- search_parts[search_parts %like% "^p[0-9]+$"][1]
  }
  if (search_term %like% "^p?[0-9]+$") {
    # searching for a project number (such as "p100" or "100"), just check titles and return the hit
    task_titles <- tasks |> get_azure_property("title")
    title_hits <- which(task_titles %like% paste0("p", gsub("p", "", search_term), "$"))
    if (length(title_hits) == 1) {
      # found the project
      return(tasks[[title_hits]])
    }
  }
  
  tasks_df <- data.frame(id = tasks |> get_azure_property("id"),
                         title = tasks |> get_azure_property("title"),
                         createdDateTime = tasks |> get_azure_property("createdDateTime") |> as.Date(),
                         dueDateTime = tasks |> get_azure_property("dueDateTime") |> as.Date(),
                         bucketId = tasks |> get_azure_property("bucketId"))
  
  if (isTRUE(include_description) && search_term %unlike% "p?[0-9]") {
    message("Downloading task descriptions...", appendLF = FALSE)
    # also add descriptions
    tasks_df$description = vapply(FUN.VALUE = character(1),
                                  tasks,
                                  function(t) {
                                    desc <- t$do_operation("details")$description
                                    if (is.null(desc)) {
                                      NA_character_
                                    } else {
                                      desc
                                    }
                                  })
    message("OK")
  }
  
  tasks_df$search_col <- do.call(paste, tasks_df) # this includes description if it's in it
  tasks_df$project_number <- suppressWarnings(as.integer(gsub(".*p([0-9]+).*", "\\1", tasks_df$title)))
  tasks_df$is_like <- tasks_df$search_col %like% search_term
  tasks_df$levenshtein_distance <- as.double(utils::adist(tasks_df$search_col, search_term, fixed = TRUE))
  tasks_df$levenshtein_delta <- nchar(tasks_df$search_col) - tasks_df$levenshtein_distance
  tasks_df <- tasks_df |> arrange(desc(is_like), levenshtein_delta)
  
  if (sum(tasks_df$is_like) == 0) {
    message("No tasks found resembling '", search_term, "'.")
    return(NA)
  } else if (sum(tasks_df$is_like) == 1 && !interactive()) {
    return(tasks[[which(tasks_df$is_like)]])
  } else {
    if (!interactive()) {
      message("Multiple results found, assuming task '", tasks_df$title[1], "'")
      return(tasks[[which(get_azure_property(tasks, "id") == tasks_df$id[1])]])
    } else {
      if (!is.infinite(limit)) {
        tasks_df <- tasks_df[seq_len(limit), ]
      }
      # interactive and more than 1, pick from menu
      buckets <- planner_buckets_list(account = account) |> 
        lapply(as.data.frame, account = account) |> 
        bind_rows()
      tasks_df$bucket <- buckets$name[match(tasks_df$bucketId, buckets$id)]
      if (rstudioapi::isAvailable()) {
        # use RStudio and Shiny to pick a project
        duplicated_titles <- which(tasks_df$title %in% tasks_df$title[which(duplicated(tasks_df$title))])
        tasks_df$title[duplicated_titles] <- paste0(tasks_df$title[duplicated_titles], " (eind: ", format2(tasks_df$dueDateTime[duplicated_titles], "ddd d mmm yyyy"), ")")
        task_id <- NULL
        task_id <- shiny_item_picker(tasks_df$title |> stats::setNames(tasks_df$id),
                                     oversized = which(tasks_df$is_like),
                                     title = "Kies een taak uit de lijst:",
                                     subtitle = "(vetgedrukte taken hebben een tekstovereenkomst met de zoekterm)")
        if (is.null(task_id)) {
          return(NA)
        } else {
          return(tasks[[which(get_azure_property(tasks, "id") == task_id)]])
        }
      } else {
        texts <- paste0(font_blue(tasks_df$title, collapse = NULL),
                        " (", ifelse(is.na(tasks_df$createdDateTime),
                                     "",
                                     paste0(format2(tasks_df$createdDateTime, "ddd d mmm yyyy"), ", ")),
                        tasks_df$bucket, ")")
        choice <- utils::menu(texts, graphics = FALSE,
                              title = paste0(ifelse(is.infinite(limit), "Tasks", paste("First", limit, "tasks")),
                                             " in '", get_azure_property(account, "title"), "' found (0 to Cancel):"))
      }
      if (choice == 0) {
        NA
      } else {
        return(tasks[[which(get_azure_property(tasks, "id") == tasks_df$id[choice])]])
      }
    }
  }
}

#' @rdname planner
#' @param task exact task ID or title, will be searched with [`%like%`][certetoolbox::like]
#' @details [planner_task_find()] searches task title or ID, and returns an [`ms_plan_task`][Microsoft365R::ms_plan_task] object. It is used internally b a lot of `planner_*` functions, very fast, and does not support interactive use.
#' @importFrom Microsoft365R ms_plan_task
#' @export
planner_task_find <- function(task, account = connect_planner()) {
  if (inherits(task, "ms_plan_task")) {
    return(task)
  } else {
    task <- as.character(task)
  }
  tasks <- account$list_tasks() # this will also include the dummy project, which must be the case
  out <- which(get_azure_property(tasks, "id") == task)
  if (length(out) == 0) {
    out <- which(get_azure_property(tasks, "title") %like% task)
    if (length(out) == 0) {
      # perhaps regex made it fail, so:
      out <- which(grepl(task, get_azure_property(tasks, "title"), fixed = TRUE))
    }
  }
  
  if (length(out) == 0) {
    stop("Task not found: '", task, "'", call. = FALSE)
  } else if (length(out) == 1) {
    return(tasks[[out[1]]])
  } else {
    tasks <- tasks[out]
    titles <- get_azure_property(tasks, "title")
    if (any(titles == task)) {
      tasks <- tasks[which(titles == task)]
      # sort on creation date, highest first
      created <- get_azure_property(tasks, "createdDateTime")
      tasks <- tasks[order(created, decreasing = TRUE)]
      out <- tasks[[1]]
      if (length(tasks) > 1) {
        warning("There are ", length(tasks), " tasks with the title '", task, 
                "'. \nAssuming task ID: ", get_azure_property(out, "id"),
                ", created on ", format(as.POSIXct(gsub("T", " ", get_azure_property(out, "createdDateTime")))), ".",
                call. = FALSE)
      }
      return(out)
    } else {
      out <- tasks[[1]]
      message("Assuming task '", get_azure_property(out, "title"),  "' for searching with text '", task, "' out of ", length(tasks), " hits")
      return(out)
    }
  }
}

#' @rdname planner
#' @export
planner_categories_list <- function(account = connect_planner()) {
  unlist(account$do_operation("details")$categoryDescriptions, use.names = TRUE)
}

#' @rdname planner
#' @details [planner_retrieve_project_id()] retrieves the p-number from the task title and returns it as [integer].
#' @export
planner_retrieve_project_id <- function(task, account = connect_planner()) {
  task <- planner_task_search(task, include_completed = TRUE, include_description = FALSE, account = account)
  title <- get_azure_property(task, "title")
  if (title %like% "p[0-9]+") {
    structure(as.integer(gsub(".*p([0-9]+).*", "\\1", title)),
              task = task,
              class = c("certeprojects_planner_project_nr", "integer"))
  } else {
    structure(NA_integer_,
              task = NULL,
              class = c("certeprojects_planner_project_nr", "integer"))
  }
}

#' @noRd
#' @method print certeprojects_planner_project_nr
#' @export
print.certeprojects_planner_project_nr <- function(x, ...) {
  if (!is.null(attributes(x)$task)) {
    cat("MS Planner Project ID of: ", attributes(x)$task |> get_azure_property("title"), "\n", sep = "")
  } else {
    cat("MS Planner Project ID\n")
  }
  print(as.integer(x))
}

#' @rdname planner
#' @param category_text text of the category to use
#' @export
planner_task_request_validation <- function(task,
                                            category_text = read_secret("planner.label.authorise"),
                                            account = connect_planner()) {
  planner_task_update(task, categories = category_text, account = account)
}

#' @rdname planner
#' @export
planner_task_validate <- function(task,
                                  category_text = read_secret("planner.label.authorised"),
                                  account = connect_planner()) {
  planner_task_update(task, categories = category_text, account = account)
}

#' @rdname planner
#' @param path location of the folder that has to be converted to a project. This folder will be renamed to contain the new project number.
#' @param projects_path location of the folder that contains all department projects
#' @param ... arguments passed on to [planner_task_create()]
#' @details Use [planner_create_project_from_path()] to convert any folder (and any location) to a project folder, by (1) assigning a project number, (2) creating a Planner task and (3) moving the old folder to the department's projects folder.
#' @export
planner_create_project_from_path <- function(path,
                                             projects_path = read_secret("projects.path"),
                                             account = connect_planner(),
                                             title = basename(path),
                                             ...) {
  if (!dir.exists(path)) {
    stop("path not found: ", path, call. = FALSE)
  }
  path <- tools::file_path_as_absolute(path)
  if (!interactive()) {
    stop("planner_create_project_from_path() has to run in interactive mode.", call. = FALSE)
  }
  continue <- utils::askYesNo(paste0("Nieuwe Planner-taak en projectnummer aanmaken voor '", title, "'?"),
                              prompts = gettext(c("Ja", "Nee", "Annuleren")))
  if (!isTRUE(continue)) {
    return(invisible())
  }
  
  dots <- list(...)
  
  cat("\nPROJECT:             ", title, "\n", sep = "")
  if (!"requested_by" %in% names(dots)) {
    dots$requested_by <- readline("Aangevraagd door:    ")
  }
  if (!"description" %in% names(dots)) {
    dots$description  <- readline("Beschrijving [leeg]: ")
  }
  if (!"bucket_name" %in% names(dots)) {
    dots$bucket_name <- readline("Bucket [Bezig]:      ")
    if (dots$bucket_name == "") {
      dots$bucket_name <- "Bezig"
    }
  }
  ass <- utils::askYesNo(paste0("Project toewijzen aan ", Sys.info()["user"], "?"),
                         prompts = gettext(c("Ja", "Nee", "")))
  if (isTRUE(ass)) {
    dots$assigned <- paste0(Sys.info()["user"], "@certe.nl")
  } else {
    dots$assigned <- NULL
  }
  dots$title <- title
  dots$categories <- "Project"
  dots$account <- account
  
  task <- do.call(planner_task_create, dots)
  
  new_title <- task$title
  new_path <- paste0(projects_path, "/", new_title)
  cat("Oude mapnaam:   ", path, "\n", sep = "")
  cat("Nieuwe mapnaam: ", new_path, "\n", sep = "")
  move_folder <- FALSE
  while(!isTRUE(move_folder)) {
    move_folder <- utils::askYesNo(paste0("Druk op Enter om de oude map te hernoemen."),
                                   prompts = gettext(c("OK", "Annuleren", "")))
  }
  success <- FALSE
  while (!isTRUE(success)) {
    success <- tryCatch(file.rename(path, paste0(dirname(path), "/", new_title)),
                        error = function(e) {
                          message("De map kon niet verplaatst worden: ", e$message)
                          return(FALSE)
                        })
    if (!success) {
      readline("Druk op Enter om het opnieuw te proberen. ")
    }
  }
  message("Geslaagd.")
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
                                  account = connect_planner(),
                                  property = "id",
                                  as_list = FALSE) {
  if (is.null(pkg_env$planner_members)) {
    pkg_env$planner_members <- create_graph_login(token = account$token)$get_group(name = team_name)$list_members()
  }
  members <- pkg_env$planner_members
  if (is.null(pkg_env$planner_owners)) {
    pkg_env$planner_owners <- create_graph_login(token = account$token)$get_group(name = team_name)$list_owners()
  }
  owners <- pkg_env$planner_owners
  owners <- get_azure_property(owners, "displayName")
  df <- data.frame(certe_login = gsub("@certe.nl", "", get_azure_property(members, "userPrincipalName")),
                   name = get_azure_property(members, "displayName"),
                   mail = get_azure_property(members, "mail"),
                   id = get_azure_property(members, "id"),
                   role = ifelse(get_azure_property(members, "displayName") %in% owners, "Eigenaar", "Lid")) |>
    bind_rows() |> 
    filter(name != "Azure Connect")
  
  if (as_list == TRUE) {
    return(list(Eigenaars = sort(df[[property]][which(df$role == "Eigenaar")]),
                Leden = sort(df[[property]][which(df$role == "Lid")])))
  }
  
  users <- character(0)
  for (usr in user) {
    users <- c(users, df[[property]][which(df$id == usr | paste0(df$certe_login, "@certe.nl") == tolower(usr) | df$certe_login %like% usr | df$name %like% usr | df$mail %like% usr)])
  }
  users
}

#' @rdname planner
#' @details [planner_highest_project_id()] retrieves the currently highest project ID from the dummy project.
#' @export
planner_highest_project_id <- function(task = read_secret("planner.dummy.project.id"),
                                       account = connect_planner()) {
  # this returns the currently highest project number, which is saved to the description
  task <- account$get_task(task_id = task)
  as.integer(gsub("[^0-9]+", "", task$do_operation("details")$description))
}

#' @rdname planner
#' @details [planner_highest_project_id()] retrieves the currently highest project ID from the dummy project. [planner_highest_consult_id()] does this for consults.
#' @export
planner_highest_consult_id <- function(task = read_secret("planner.dummy.consult.id"),
                                       account = connect_planner()) {
  # this returns the currently highest project number, which is saved to the description
  task <- account$get_task(task_id = task)
  as.integer(gsub("[^0-9]+", "", task$do_operation("details")$description))
}

#' @rdname planner
#' @method as.data.frame ms_object
#' @param x an `ms_object`
#' @inheritParams base::as.data.frame
#' @details Using [as.data.frame()] or [as_tibble()] on an `ms_object`, such as `ms_plan_task`, will return the properties and details of the object as a [data.frame]. For transforming many `ms_object`s to a data.frame, use [as.data.frame()] or [as_tibble()] in [lapply()] and bind the list of objects together. For example, this retrieves a tibble with the properties and details of all tasks:
#' 
#' ```r
#' library(dplyr)
#' planner_tasks_list() |> 
#'   lapply(as_tibble) |> 
#'   bind_rows()
#' 
#' # also works for other 'ms_object's, such as 'ms_channel':
#' teams_channels_list(plain = FALSE) |> 
#'   lapply(as_tibble) |> 
#'   bind_rows()
#' ```
#' @importFrom dplyr bind_rows
#' @export
as.data.frame.ms_object <- function(x, row.names = NULL, optional = FALSE, account = connect_planner(), ...) {
  object <- x$properties
  details <- tryCatch(x$do_operation("details"), error = function(e) NULL)
  if (!is.null(details)) {
    object <- c(object, details[!names(details) %in% names(x$properties)])
  }
  names(object) <- gsub("@", "", names(object), fixed = TRUE)
  as.data.frame(lapply(object, function(y) {
    if (is.null(y) || length(y) == 0) {
      NA
    } else if (all(y %like% "[TZ]" & y %like% "^[0-9TZ.: -]+$", na.rm = TRUE)) {
      # dates
      as.POSIXct(gsub("T", " ", y, fixed = TRUE))
    } else if (!tryCatch(is.null(y$user$id), error = function(e) TRUE)) {
      # has one user, such as createdBy and completedBy
      tryCatch(paste0(planner_user_property(y$user$id, property = "name", account = account), collapse = ", "),
               error = function(e) paste0(y, collapse = ", "))
    } else if (!is.null(names(y)) && all(names(y) %like% "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{10}", na.rm = TRUE)) {
      tryCatch(paste0(planner_user_property(names(y), property = "name", account = account), collapse = ", "),
               error = function(e) paste0(y, collapse = ", "))
    } else {
      paste0(y, collapse = ", ")
    }}),
    row.names = row.names,
    optional = optional
  )
}

#' @rdname planner
#' @method as_tibble ms_object
#' @importFrom dplyr as_tibble
#' @export
as_tibble.ms_object <- function(x, account = connect_planner(), ...) {
  as_tibble(as.data.frame(x = x, account = account, ...))
}


# INTERNAL FUNCTIONS ------------------------------------------------------

planner_bucket_object <- function(bucket_name = read_secret("planner.default.bucket"), account = connect_planner()) {
  # account$get_bucket() does not work yet in Microsoft365R, return error 'Invalid bucket name', so do it manually:
  buckets <- account$list_buckets()
  index <- which(get_azure_property(buckets, "name") == bucket_name)
  if (length(index) == 0) {
    stop("Bucket not found")
  } else {
    buckets[[index[1L]]]
  }
}

increase_highest_project_id <- function(task = read_secret("planner.dummy.project.id"),
                                        account = connect_planner()) {
  highest <- planner_highest_project_id(task = task, account = account)
  planner_task_update(task = task, description = paste0("p", highest + 1), account = account)
}

increase_highest_consult_id <- function(task = read_secret("planner.dummy.consult.id"),
                                        account = connect_planner()) {
  highest <- planner_highest_consult_id(task = task, account = account)
  planner_task_update(task = task, description = paste0("c", highest + 1), account = account)
}


get_internal_category_name <- function(category_name, account = connect_planner()) {
  categories <- planner_categories_list(account = account)
  names(categories[categories %in% category_name])
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
