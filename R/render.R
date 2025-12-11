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

#' Knit R Markdown or Quarto Document
#' 
#' Runs input files through Quarto or RMarkdown, and allows Quarto to work with full paths (where Quarto itself requires relative paths). This function essentially replaces both [rmarkdown::render()] and [quarto::quarto_render()].
#' @param input_file file to be rendered, can be R Markdown (`.Rmd`) or Quarto (`.qmd`), or a lot of other formats such as `.md`, `.ipynb` and [many other formats that Quarto supports](https://quarto.org/docs/reference/).
#' @inheritParams quarto::quarto_render
#' @param ... other arguments passed on to [quarto::quarto_render()].
#' @details Functions [knit()] and [render()] are identical.
#' 
#' Any Quarto file (`.qmd`) will be rendered using [quarto::quarto_render()]. Any RMarkdown file (`.Rmd`) will be rendered using [rmarkdown::render()].
#' @return As opposed to [quarto::quarto_render()], which always returns `NULL`, this `render()` function always returns the output file name (invisibly).
#' @importFrom quarto quarto_path quarto_render
#' @importFrom rmarkdown render
#' @rdname render
#' @export
render <- function(input_file, output_file = NULL, quiet = FALSE, ...) {
  input_file <- tools::file_path_as_absolute(input_file)
  
  if (input_file %like% "Rmd$") {
    # RMarkdown
    render_fn <- rmarkdown::render
  } else {
    # Quarto
    if (!file.exists(quarto_path())) {
      stop("Quarto needs to be installed.")
    }
    render_fn <- quarto::quarto_render
    Sys.setenv(QUARTO_PROJECT_FILE = input_file)
    Sys.setenv(QUARTO_PRINT_STACK = "true") # will cause Quarto to print a stack trace when an error occurs
  }
  
  if (!is.null(output_file)) {
    output_file <- file.path(tools::file_path_as_absolute(dirname(output_file)), basename(output_file))
    output_dir  <- dirname(output_file)
  } else {
    output_dir  <- dirname(input_file)
  }
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(dirname(input_file))
  
  if (is.null(output_file)) {
    # in case of quarto::quarto_render(): checks if `output_file` is MISSING, not if it's NULL because its default is NULL already
    render_fn(input = basename(input_file),
              quiet = quiet,
              ...)
    output_file <- detect_output_file(tools::file_path_as_absolute(input_file))
  } else {
    render_fn(input = basename(input_file),
              output_file = basename(output_file),
              quiet = quiet,
              ...)
  }
  
  if (!is.null(output_file) && dirname(input_file) != output_dir) {
    file.rename(from = file.path(getwd(), basename(output_file)),
                to = file.path(output_dir, basename(output_file)))
    if (tools::file_ext(output_file) == "html") {
      # also move the libraries that were generated
      temp_folder_name <- paste0(gsub(paste0(".", tools::file_ext(input_file)),
                                      "",
                                      basename(input_file), fixed = TRUE),
                                 "_files")
      suppressWarnings(
        file.rename(from = file.path(getwd(), temp_folder_name),
                    to = file.path(output_dir, temp_folder_name))
      )
    }
    if (isFALSE(quiet)) {
      message("Rendered file was moved to '", file.path(output_dir, basename(output_file)), "'.")
    }
  }
  
  invisible(output_file)
}

#' @rdname render
#' @export
knit <- render

#' @rdname render
#' @inheritParams sharepoint
#' @param ... arguments passed on to [certeprojects::render()]
#' @param output_folder any output folder of the resulting output file, defaults to [get_output_folder()]. Can also be `"sharepoint"`, which sets `upload = TRUE`.
#' @param upload whether to upload the resulting output file back to SharePoint, default is `FALSE`.
#' @details
#' [render_sharepoint()] downloads a SharePoint project file to a local temporary folder, then renders it. Either `input_file` + `project_number` or `full_sharepoint_path` must be given.
#' @export
render_sharepoint <- function(input_file = NULL,
                              output_file = NULL,
                              project_number = project_get_current_id(),
                              full_sharepoint_path = NULL,
                              quiet = FALSE,
                              account = connect_teams(),
                              output_folder = get_output_folder(project_number = project_number, account = account),
                              upload = FALSE,
                              ...) {
  
  if (!is.null(input_file)) {
    if (!is.null(full_sharepoint_path)) {
      stop("Either `input_file` or `full_sharepoint_path` must be set, not both")
    }
    if (input_file %unlike% "[.](R|Rmd|qmd)$") {
      input_file <- paste0(input_file, ".*(R|Rmd|qmd)$")
    }
    full_sharepoint_path <- project_get_file(input_file, project_number = project_number, account = account)
  }
  
  if (full_sharepoint_path %unlike% "Rmd$") {
    # download quarto settings file first if file is not RMarkdown (could be `.qmd`, `.md`, `.ipynb`, etc.)
    try(download_from_sharepoint(full_sharepoint_path = "_quarto.yml"), silent = TRUE)
  }
  
  file_local <- download_from_sharepoint(full_sharepoint_path = full_sharepoint_path, account = account)
  
  if (is.null(output_file)) {
    output_file <- detect_output_file(tools::file_path_as_absolute(file_local))
  }

  cli::cli_process_start(paste0("Rendering file: {.val {basename(full_sharepoint_path)}} -> {.val {basename(output_file)}}"))
  if (file.exists(output_file)) {
    # remove output file before rendering
    try(unlink(output_file, force = TRUE), silent = TRUE)
  }
  out <- render(input_file = file_local, output_file = output_file, quiet = quiet, ...)
  cli::cli_process_done(msg_done = paste0("Rendered file: {.val {basename(full_sharepoint_path)}} -> {.val {basename(out)}}"))
  
  if (identical(tolower(output_folder), "sharepoint") || (!isTRUE(upload) && is.null(output_folder))) {
    upload <- TRUE
    output_folder <- NULL
  }
  
  if (isTRUE(upload)) {
    # upload the result
    upload_to_sharepoint(local_file_name = out,
                         full_sharepoint_path = file.path(dirname(full_sharepoint_path), basename(out)))
    return(invisible(file.path(dirname(full_sharepoint_path), basename(out))))
  } else {
    file.copy(out, file.path(output_folder, output_file), overwrite = TRUE, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)
    cli::cli_process_done(msg_done = paste0("Rendered file saved to {.val {output_folder}} folder"))
    return(file.path(output_folder, output_file))
  }
  
}

detect_output_file <- function(input_file) {
  yml <- rmarkdown::yaml_front_matter(input_file)
  ext <- NULL
  
  if (!is.null(yml$format)) {
    fmt <- yml$format
    if (is.character(fmt)) {
      ext <- fmt
    } else if (is.list(fmt) && length(fmt) > 0) {
      ext <- names(fmt)[1]
    }
  } else if (!is.null(yml$output)) {
    fmt <- yml$output
    if (is.character(fmt)) {
      ext <- fmt
    } else if (is.list(fmt) && length(fmt) > 0) {
      ext <- names(fmt)[1]
    }
  }
  
  # Normalize namespaced formats
  ext <- sub("^.*::", "", ext)
  
  # Map known output formats to file extensions
  ext_map <- list(
    html_document = "html",
    html_vignette = "html",
    pdf_document  = "pdf",
    word_document = "docx",
    beamer_presentation = "pdf",
    slidy_presentation = "html",
    ioslides_presentation = "html",
    `revealjs::revealjs_presentation` = "html",
    pdf = "pdf",
    html = "html",
    docx = "docx"
  )
  
  if (!is.null(ext) && length(ext) > 1) {
    ext <- ext_map[[ext]]
  } else {
    ext <- "pdf"
  }
  
  paste0(tools::file_path_sans_ext(input_file), ".", ext)
}
