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

#' Knit R Markdown or Quarto Document using Quarto
#' 
#' Runs input files through Quarto, and allows Quarto to work with full paths (where Quarto itself requires relative paths). This function essentially replaces both [rmarkdown::render()] and [quarto::quarto_render()].
#' @param input_file file to be rendered, can be R Markdown (`.Rmd`) or Quarto (`.qmd`), or a lot of other formats such as `.md`, `.ipynb` and [many other formats that Quarto supports](https://quarto.org/docs/reference/).
#' @inheritParams quarto::quarto_render
#' @param ... other arguments passed on to [quarto::quarto_render()].
#' @details Functions [knit()] and [render()] are identical.
#' @importFrom quarto quarto_path quarto_render
#' @rdname render
#' @export
render <- function(input_file, output_file = NULL, quiet = TRUE, as_job = "auto", ...) {
  if (!file.exists(quarto_path())) {
    stop("Quarto needs to be installed.")
  }
  input_file <- tools::file_path_as_absolute(input_file)
  if (!is.null(output_file)) {
    output_file <- file.path(tools::file_path_as_absolute(dirname(output_file)), basename(output_file))
    output_dir  <- dirname(output_file)
  } else {
    output_dir  <- dirname(input_file)
  }
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(dirname(input_file))
  
  Sys.setenv(QUARTO_PROJECT_FILE = input_file)
  Sys.setenv(QUARTO_PRINT_STACK = "true") # will cause Quarto to print a stack trace when an error occurs
  
  if (is.null(output_file)) {
    # because quarto::quarto_render() checks if `output_file` is MISSING, not if it's NULL because its default is NULL already
    quarto_render(input = basename(input_file),
                  quiet = quiet,
                  as_job = as_job,
                  ...)
  } else {
    quarto_render(input = basename(input_file),
                  output_file = basename(output_file),
                  quiet = quiet,
                  as_job = as_job,
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
