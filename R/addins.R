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

addin1_projects_new <- function() {
  project_add()
}

#' @importFrom miniUI miniPage miniContentPanel
#' @importFrom shiny uiOutput renderUI tagList actionButton h4 p HTML observe runGadget dialogViewer stopApp icon reactiveValuesToList
#' @importFrom rstudioapi showDialog
#' @importFrom certestyle colourpicker
#' @importFrom dplyr case_when
addin2_projects_status_update <- function() {
  current_task_id <- project_get_current_id()
  if (is.null(current_task_id)) {
    return(invisible())
  }
  
  ui <- miniPage(
    tags$style(paste0(
      "body {
        background-color: ", colourpicker("certeblauw6"), ";
     }",
      "* {
        text-align: center;
     }",
      ".move_btn {
        font-size: 14px;
        background-color: white;
        margin-bottom: 10px;
     }",
      "button .fa-solid {
      font-size: 20px;
      display: block;
    }")),
    miniContentPanel(
      uiOutput("task_ui")
    )
  )
  
  server <- function(input, output, session) {
    output$task_ui <- renderUI({
      buckets <- planner_buckets_list()
      current_task <- planner_task_find(paste0("p", current_task_id))
      bucket_id <- get_azure_property(current_task, "bucketId")
      buckets_df <- data.frame(id = get_azure_property(buckets, "id"),
                               name = get_azure_property(buckets, "name"))
      buckets_df$sorting <- case_when(buckets_df$name %like% "Idee" ~ 1,
                                      buckets_df$name %like% "Bezig" ~ 2,
                                      buckets_df$name %like% "Wachten" ~ 3,
                                      buckets_df$name %like% "Voltooid" ~ 4,
                                      TRUE ~ 5)
      buckets_df <- buckets_df[order(buckets_df$sorting), ]
      current_bucket <- buckets_df$name[match(bucket_id, buckets_df$id)]
      
      buttons <- tagList()
      for (bucket in buckets_df$name) {
        if (bucket == current_bucket || bucket %like% "nog mee beginnen") next
        icon <- case_when(bucket %like% "Bezig" ~ "screwdriver-wrench",
                          bucket %like% "Wachten" ~ "hourglass-start",
                          bucket %like% "Voltooid" ~ "square-check",
                          bucket %like% "Idee" ~ "lightbulb",
                          TRUE ~ "screwdriver-wrench")
        buttons <- c(buttons,
                     tagList(
                       actionButton(inputId = bucket,
                                    label = paste0("Verplaatsen naar '", bucket, "'"),
                                    icon = icon(icon, class = "fa-solid"),
                                    width = "100%",
                                    class = "move_btn")))
      }
      
      tagList(
        h4(get_azure_property(current_task, "title")),
        p(HTML(paste("Huidige bucket:", strong(current_bucket), 
                     ifelse(is.na(get_azure_property(current_task, "startDateTime")),
                            "",
                            paste0("<br><small>Gestart: ", format2(as.Date(as.POSIXct(gsub("[TZ]", " ", get_azure_property(current_task, "startDateTime"))), tz = "Europe/Amsterdam"), "dddd d mmmm yyyy"), "</small>"))))),
        buttons
      )  
    })
    observe({
      contents <- unlist(reactiveValuesToList(input))
      if (is.null(contents) || all(contents == 0)) return(invisible())
      move_to <- names(contents)[which(contents == 1)]
      current_task <- planner_task_find(paste0("p", current_task_id))
      planner_task_update(current_task, bucket_name = move_to)
      stopApp()
      showDialog(title = "Taak verplaatst",  message = paste0("Taak verplaatst naar '", move_to, "'."))
    })
  }
  suppressMessages(
    runGadget(app = ui, server = server,
              viewer = dialogViewer(dialogName = paste("Taak verplaatsen -", get_azure_property(pkg_env$planner, "title")), width = 550, height = 270))
  )
}
addin4_projects_save <- function() {
  project_save_file()
}
addin5_projects_open_file <- function() {
  project_open_analysis_file()
}
addin6_projects_open_folder <- function() {
  project_open_folder()
}

#' @importFrom rstudioapi showPrompt filesPaneNavigate showQuestion
set_wd <- function(new_dir = NULL) {
  if (is.null(new_dir)) {
    if ("clipr" %in% rownames(utils::installed.packages())) {
      default <- clipr::read_clip()
      if (!is.character(default) || length(default) != 1 || !dir.exists(as.character(default)[1L])) {
        default <- ""
      }
    } else {
      default <- ""
    }
    new_dir <- showPrompt(title = "Set Working Directory",
                          message = "New Working Directory:",
                          default = default)
  }
  if (is.null(new_dir) || !dir.exists(new_dir)) {
    return(invisible())
  }
  setwd(new_dir)
  filesPaneNavigate(new_dir)
  
  rdata <- paste0(new_dir, "/.Rdata")
  if (file.exists(rdata)) {
    if (isTRUE(showQuestion(title = "Load RData",
                            message = paste0("Also load RData from folder ", basename(new_dir), "?"),
                            ok = "Yes",
                            cancel = "No"))) {
      load(rdata, envir = globalenv())
    }
  }
}
