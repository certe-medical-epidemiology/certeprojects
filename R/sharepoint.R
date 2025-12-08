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

#' Upload to / Download from SharePoint Project Channel
#' 
#' These functions allow for fast and easy transfer between local files and SharePoint, using [teams_projects_channel()] to retrieve the Projects channel.
#' @param local_file_name File name of the local file.
#' @param remote_file_name Name of the file to search, will be used together with `project_number` and can be a regular expression. Will be passed on to [project_get_file()].
#' @inheritParams project_get_file
#' @param full_sharepoint_path Full path of the remote file, consisting of the project folder and the file name, such as `"Project - p123/my_file.R"`.
#' @param overwrite Whether to overwrite an existing file.
#' @details
#' You must either provide `remote_file_name` + `project_number`, or provide `full_sharepoint_path`.
#' 
#' * `download_from_sharepoint()` finds a SharePoint file in the [Projects channel][teams_projects_channel()] using [project_get_file()], and downloads it to a local temporary folder. The local filename is returned and can be used in other functions.
#' * `upload_to_sharepoint()` uploads any local file to SharePoint in the [Projects channel][teams_projects_channel()]. The remote path is returned and can be used in other functions.
#' * `source_sharepoint()` runs [source()] on a SharePoint project file, by downloading it to a temporary folder first using `download_from_sharepoint()`.
#' * `download_file_to_sharepoint()` downloads a remote URL and then uploads the downloaded file to SharePoint using `upload_to_sharepoint()`
#' @rdname sharepoint
#' @name sharepoint
#' @export
#' @examples
#' \dontrun{
#' 
#' source_sharepoint("my_file.R", 123)
#' 
#' "my_file.R" |>
#'   download_from_sharepoint(project_number = 123) |>
#'   source_sharepoint()
#'   
#' url <- "https://examplefile.com/file-download/26"
#' from_internet <- download_file_to_sharepoint(url,
#'                                              "test-download.pdf",
#'                                              project_number = 123)
#' from_internet
#' #> [1] "Testproject - p123/test-download.pdf"
#' 
#' local_file <- download_from_sharepoint(full_sharepoint_path = from_internet)
#' }
download_from_sharepoint <- function(remote_file_name = NULL,
                                     project_number = project_get_current_id(),
                                     full_sharepoint_path = NULL,
                                     account = connect_teams(),
                                     overwrite = TRUE) {
  if (!is.null(remote_file_name)) {
    if (!is.null(full_sharepoint_path)) {
      stop("Either `remote_file_name` or `full_sharepoint_path` must be set, not both")
    }
    full_sharepoint_path <- project_get_file(remote_file_name, project_number = project_number, account = account)
  }
  
  sharepoint_file <- teams_projects_channel(account = account)$get_item(full_sharepoint_path)
  
  # download to temporary file
  download_folder <- file.path(tempdir(), "certeprojects-sharepoint-downloads", dirname(full_sharepoint_path))
  if (!dir.exists(download_folder)) {
    dir.create(download_folder, showWarnings = FALSE, recursive = TRUE)
  }
  file_local <- file.path(download_folder, basename(full_sharepoint_path))
  try(unlink(file_local, force = TRUE), silent = TRUE)
  
  tryCatch({
    cli::cli_process_start("Downloading {.val {full_sharepoint_path}} from SharePoint...")
    sharepoint_file$download(dest = file_local, overwrite = overwrite)
    cli::cli_process_done(msg_done = "Downloaded {.val {full_sharepoint_path}} from SharePoint.")
  }, error = function(e) {
    cli::cli_process_failed(msg = "Downloading {.val {full_sharepoint_path}} from SharePoint...")
    stop(e)
  })
  
  file_local
}

#' @rdname sharepoint
#' @export
upload_to_sharepoint <- function(local_file_name = NULL,
                                 remote_file_name = NULL,
                                 project_number = project_get_current_id(),
                                 full_sharepoint_path = NULL,
                                 account = connect_teams()) {
  if (!file.exists(local_file_name)) {
    stop("Local file '", local_file_name, "' does not exists")
  }
  if (!is.null(remote_file_name)) {
    if (!is.null(full_sharepoint_path)) {
      stop("Either `remote_file_name` or `full_sharepoint_path` must be set, not both")
    }
    full_sharepoint_path <- project_set_file(remote_file_name, project_number = project_number, account = account)
  }
  channel <- teams_projects_channel(account = account)
  
  tryCatch({
    cli::cli_process_start("Uploading {.val {full_sharepoint_path}} to SharePoint...")
    channel$upload(src = local_file_name, dest = full_sharepoint_path)
    cli::cli_process_done(msg_done = "Uploaded {.val {full_sharepoint_path}} to SharePoint.")
  }, error = function(e) {
    cli::cli_process_failed(msg = "Uploading {.val {full_sharepoint_path}} to SharePoint...")
    stop(e)
  })
  
  invisible(full_sharepoint_path)
}

#' @param ... arguments passed on to [source()] or [download.file()]
#' @rdname sharepoint
#' @export
source_sharepoint <- function(remote_file_name = NULL,
                              project_number = project_get_current_id(),
                              full_sharepoint_path = NULL,
                              account = connect_teams(),
                              ...) {
  
  if (!is.null(remote_file_name)) {
    if (!is.null(full_sharepoint_path)) {
      stop("Either `remote_file_name` or `full_sharepoint_path` must be set, not both")
    }
    full_sharepoint_path <- project_get_file(remote_file_name, project_number = project_number, account = account)
  }
  file_local <- download_from_sharepoint(full_sharepoint_path = full_sharepoint_path, account = account)
  source(file_local, ...)
}

#' @rdname sharepoint
#' @param url URL to download and afterwards upload to SharePoint
#' @export
download_file_to_sharepoint <- function(url,
                                        local_file_name = NULL,
                                        project_number = project_get_current_id(),
                                        full_sharepoint_path = NULL,
                                        account = connect_teams(),
                                        ...) {
  if (!is.null(local_file_name)) {
    if (!is.null(full_sharepoint_path)) {
      stop("Either `local_file_name` or `full_sharepoint_path` must be set, not both")
    }
    full_sharepoint_path <- project_set_file(local_file_name, project_number = project_number, account = account)
  }
  
  # download to temporary file
  download_folder <- file.path(tempdir(), "certeprojects-sharepoint-downloads", dirname(full_sharepoint_path))
  if (!dir.exists(download_folder)) {
    dir.create(download_folder, showWarnings = FALSE, recursive = TRUE)
  }
  file_local <- file.path(download_folder, basename(full_sharepoint_path))
  try(unlink(file_local, force = TRUE), silent = TRUE)
  
  tryCatch({
    cli::cli_process_start("Downloading {.url {url}} as {.val {basename(file_local)}}...")
    utils::download.file(url = url, destfile = file_local, ..., quiet = TRUE)
    cli::cli_process_done(msg_done = "Downloaded {.url {url}} as {.val {basename(file_local)}}.")
  }, error = function(e) {
    cli::cli_process_failed(msg = "Downloading {.url {url}} as {.val {basename(file_local)}}...")
    stop(e)
  })
  
  if (file.exists(file_local)) {
    upload_to_sharepoint(local_file_name = file_local,
                         full_sharepoint_path = full_sharepoint_path,
                         account = account)
  }
}
