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

#' Add Project Using Shiny
#' 
#' This is a Shiny app to add a new project: it creates a project folder local [or in Teams][teams_connect()], generates the required Quarto or R Markdown or R files, and creates a [new task in Planner][planner_connect()]. These functions come with RStudio addins to quickly access existing projects.
#' @param planner Microsoft Planner account, as returned by e.g. [planner_connect()]
#' @param teams Microsoft Teams account, as returned by e.g. [teams_connect()]
#' @param channel Microsoft Teams Channel folder, as returned by e.g. [teams_projects_channel()]
#' @export
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel textInput textAreaInput uiOutput selectInput checkboxInput br p hr actionButton radioButtons renderUI tagList selectizeInput dateInput observeEvent updateTextInput runGadget stopApp dialogViewer incProgress withProgress tags icon mainPanel img a updateCheckboxInput updateRadioButtons HTML h5 strong
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom shinyWidgets searchInput awesomeRadio awesomeCheckbox
#' @importFrom dplyr select pull filter if_else
#' @importFrom certestyle colourpicker format2
#  certestyle for R Markdown:
#' @importFrom certestyle rmarkdown_author rmarkdown_date rmarkdown_template rmarkdown_logo
#' @importFrom rstudioapi initializeProject openProject navigateToFile getActiveProject showDialog showQuestion
#' @importFrom Microsoft365R ms_plan ms_team ms_drive_item
#' @rdname project_shiny
project_add <- function(planner = planner_connect(),
                        teams = teams_connect(),
                        channel = teams_projects_channel(account = teams)) {
  
  if (!inherits(planner, "ms_plan")) {
    planner <- NULL
  }
  if (!inherits(teams, "ms_team") || !inherits(channel, "ms_drive_item")) {
    teams <- NULL
    channel <- NULL
  }
  
  # ui ----
  ui <- fluidPage(
    useShinyjs(),
    tags$script('$(document).ready(function() {
                var textbox = document.getElementById("title");
                textbox.focus();
                });'),
    tags$style(paste0("* { font-family: Calibri; }
                      .container-fluid { margin-top: 15px; }
                      .well { background-color: ", colourpicker("certeblauw6"), "; }
                      .well label { color: ", colourpicker("certeblauw"), "; }
                      .form-group { margin-bottom: 5px; }
                      .well .form-group { margin-bottom: 14px; }
                      .h2, h2 { color: ", colourpicker("certeblauw"), "; }
                      h5 { font-size: 12px; }
                      a { color: ", colourpicker("certeblauw"), "; }
                      certeblauw, .certeblauw { color: ", colourpicker("certeblauw"), "; }
                      certeroze, .certeroze { color: ", colourpicker("certeroze"), "; }
                      label[for=title] { font-size: 16px; }
                      #create { background-color: ", colourpicker("certeblauw"), "; border-color: ", colourpicker("certeblauw"), "; }
                      #create:hover { background-color: ", colourpicker("certeblauw2"), "; border-color: ", colourpicker("certeblauw"), "; }
                      #cancel { background-color: white; color: ", colourpicker("certeblauw"), "; border-color: ", colourpicker("certeblauw"), ";}
                      #cancel:hover { background-color: ", colourpicker("certeblauw5"), "; color: ", colourpicker("certeblauw"), "; border-color: ", colourpicker("certeblauw"), ";}
                      .multi .selectize-input .item, .selectize-dropdown .active { background-color: ", colourpicker("certeblauw6"), " !important; }
                      .selectize-input .item.active { color: white; background-color: ", colourpicker("certeblauw"), " !important; }",
                      '.checkbox-primary input[type="checkbox"]:checked+label::before, .checkbox-primary input[type="radio"]:checked+label::before { background-color: ', colourpicker("certeblauw"), "; border-color: ", colourpicker("certeblauw"), " ;}",
                      ".datepicker .active { color: white !important; background-color: ", colourpicker("certeblauw"), " !important; }",
                      '.radio-primary input[type="radio"]:checked+label::before { border-color: ', colourpicker("certeblauw"), ";}",
                      '.radio-primary input[type="radio"]:checked+label::after { background-color: ', colourpicker("certeblauw"), ";}",
                      '.awesome-radio input[type="radio"]:focus+label::before, .awesome-checkbox input[type="checkbox"]:focus+label::before { outline: none; }',
                      "#files_teams_or_local .shiny-options-group .awesome-radio { margin-left: 0px; }")),
    
    sidebarLayout(
      sidebarPanel(
        textInput("title", "Titel", placeholder = ""),
        textAreaInput("description", "Omschrijving", cols = 1, rows = 2, resize = "vertical"),
        textAreaInput("checklist", "Taken", cols = 1, rows = 3, resize = "vertical", placeholder = "(1 taak per regel)"),
        uiOutput("requested_by"),
        selectInput("priority",
                    label = HTML(paste("Prioriteit", ifelse(as.integer(format(Sys.Date(), "%u")) %in% c(6:7), " <i>(standaard 'Dringend' in het weekend)</i>", ""))),
                    choices = c("Laag", "Gemiddeld", "Belangrijk", "Dringend"), 
                    selected = ifelse(as.integer(format(Sys.Date(), "%u")) %in% c(6:7),
                                      "Dringend", # default if card is created on weekend day
                                      "Gemiddeld")),
        tags$label("Deadline"),
        awesomeCheckbox("has_deadline", "Deadline instellen", TRUE),
        uiOutput("deadline"),
        br(),
        br(),
        actionButton("create", "Aanmaken", width = "49%", icon = icon("check"), class = "btn-success"),
        actionButton("cancel", "Annuleren", width = "49%", icon = icon("ban"), class = "btn-danger"),
        width = 6),
      
      mainPanel(
        img(src = img_rstudio(), height = "40px", style = "margin-bottom: 10px"),
        awesomeRadio("filetype",
                     label = "Bestandstype",
                     status = "primary",
                     choices = c(".qmd (Quarto)" = ".qmd",
                                 ".R" = ".R",
                                 ".Rmd (R Markdown)" = ".Rmd"),
                     selected = "",
                     inline = TRUE,
                     width = "100%"),
        awesomeCheckbox("rstudio_projectfile",
                        label = "RStudio-projectbestand aanmaken en openen",
                        status = "primary",
                        value = FALSE,
                        width = "100%"),
        hr(),
        
        # PLANNER
        img(src = img_planner(), height = "40px", style = "margin-bottom: 10px"),
        if (!is.null(planner)) {
          tagList(
            awesomeCheckbox("planner_upload", HTML(paste0("Taak aanmaken in '", a(get_azure_property(planner, "title"), href = planner_url(planner)), "'")), value = TRUE, width = "100%"),
            uiOutput("planner_settings")
          )
        } else {
          h5(HTML("Verbind eerst met MS Planner via <code>planner_connect()</code>."))
        },
        hr(),
        
        # TEAMS
        img(src = img_teams(), height = "40px", style = "margin-bottom: 10px"),
        if (!is.null(teams) && !is.null(channel) && !is.null(planner)) { # teams_* project functions rely on planner, so require that too
          awesomeRadio("files_teams_or_local",
                       label = "Projectmap en analysebestand:",
                       choices =  stats::setNames(c("teams",
                                                    "local"),
                                                  c(paste0("In '", get_azure_property(teams, "displayName"), "/", get_azure_property(channel, "name"),
                                                           "' aanmaken"),
                                                    paste0("In map '", shorten_folder(ifelse(suppressWarnings(read_secret("projects.path")) == "",
                                                                                             getwd(),
                                                                                             read_secret("projects.path"))),
                                                           "' aanmaken"))),
                       selected = "local",
                       inline = TRUE,
                       width = "100%")
        } else {
          uiOutput("teamconnect")
        },
        width = 6
      )
    )
  )
  
  # server ----
  server <- function(input, output, session) {
    
    is_active_project <- !is.null(getActiveProject())
    
    output$teamconnect <- renderUI({
      tagList(
        p(strong("Niet verbonden met Teams."), "Bestanden worden aangemaakt in de map:",
          ifelse(suppressWarnings(read_secret("projects.path")) == "",
                 getwd(),
                 read_secret("projects.path"))),
        actionButton("connect_to_teams", "Klik om te verbinden met Teams")  
      )
    })
    
    observeEvent(input$connect_to_teams, {
      if (is.null(teams)) {
        teams <<- tryCatch(teams_connect(), error = function(e) NULL)
      }
      if (inherits(teams, "ms_team")) {
        channel <- teams_projects_channel(account = teams)
      }
      if (!is.null(teams) && !is.null(planner)) { # teams_* project functions rely on planner, so require that too
        output$teamconnect <- renderUI({
          awesomeRadio("files_teams_or_local",
                       label = "Projectmap en analysebestand:",
                       # status = "primary",
                       choices =  stats::setNames(c("teams",
                                                    "local"),
                                                  c(paste0("In '",
                                                           get_azure_property(teams, "displayName"), "/", get_azure_property(channel, "name"),
                                                           "' aanmaken"),
                                                    paste0("In map '", shorten_folder(ifelse(suppressWarnings(read_secret("projects.path")) == "",
                                                                                             getwd(),
                                                                                             read_secret("projects.path"))),
                                                           "' aanmaken"))),
                       selected = "local",
                       inline = TRUE,
                       width = "100%")
        })
      }
    })
    
    output$requested_by <- renderUI({
      # retrieve user list with names and job titles
      users <- get_user()
      if (!is.null(users) && !identical(users, "")) {
        suppressWarnings(tagList(
          selectizeInput("requested_by",
                         label = "Aanvrager(s)",
                         choices = users,
                         multiple = TRUE,
                         options = list(
                           # support non-existing requesters
                           create = TRUE,
                           # creates new item on field leave
                           createOnBlur = TRUE,
                           closeAfterSelect = TRUE))
        ))
      } else {
        tagList(
          textInput("requested_by", "Aanvrager(s)")
        )
      }
    })
    
    output$deadline <- renderUI({
      if (input$has_deadline == TRUE) {
        tagList(
          dateInput("deadline", NULL,
                    value = Sys.Date() + 14,
                    min = Sys.Date(),
                    format = "DD d MM yyyy",
                    language = "nl",
                    startview = "month",
                    weekstart = 1), # Monday
        )
      }
    })
    
    output$planner_settings <- renderUI({
      if (input$planner_upload == TRUE) {
        members <- planner_user_property(property = "name", as_list = TRUE, account = planner)
        categories <- unname(planner_categories_list(account = planner))
        if (length(categories) > 1) {
          categories <- list(Labels = categories)
        }
        active_user <- planner_user_property(get_current_user(), property = "name", account = planner)
        if (length(active_user) == 0) {
          active_user <- NULL
        }
        tagList(
          selectInput("planner_bucket",
                      label = "Bucket",
                      choices = list(Buckets = planner_buckets_list(plain = TRUE, account = planner)),
                      selected = read_secret("planner.default.bucket"),
                      multiple = FALSE,
                      width = "100%"),
          selectizeInput("planner_members",
                         label = "Toegewezen aan",
                         choices = members,
                         selected = active_user,
                         multiple = TRUE,
                         width = "100%",
                         options = list(
                           # do not support non-existing members:
                           create = FALSE,
                           createOnBlur = FALSE,
                           closeAfterSelect = FALSE)),
          selectizeInput("planner_categories",
                         label = "Labels",
                         choices = categories,
                         selected = "Project",
                         multiple = TRUE,
                         width = "100%",
                         options = list(
                           # do not support non-existing members:
                           create = FALSE,
                           createOnBlur = FALSE,
                           closeAfterSelect = FALSE))
        )
      }
    })
    
    observeEvent(input$files_teams_or_local, {
      if (input$files_teams_or_local == "teams") {
        updateCheckboxInput(session = session, inputId = "rstudio_projectfile", value = FALSE)
        disable("rstudio_projectfile")
        on.exit(disable("rstudio_projectfile"))
      } else {
        enable("rstudio_projectfile")
        on.exit(enable("rstudio_projectfile"))
      }
    })
    
    # SAVE ----
    observeEvent(input$create, {
      
      empty_field <- function(field, value) {
        if (is.null(value) || length(value) == 0 || value == "") {
          showDialog(title = field,
                     message = paste("Het veld<b>", field, "</b>moet ingevuld zijn."))
          TRUE
        } else {
          FALSE
        }
      }
      invalid_title <- function(title) {
        if (title %like% "[/\\><*|?\"]") {
          q <- showQuestion(title = " Titel",
                            message = paste("De titel mag de volgende tekens niet bevatten: / \\ | < > * ? \". Vervangen door een streepje (-)?"),
                            ok = "OK",
                            cancel = "Annuleren")
          if (q == TRUE) {
            title <<- gsub("[/\\><*|?\"]+", "-", title)
            # is invalid?
            return(FALSE)
          } else {
            # is invalid?
            return(TRUE)
          }
        } else {
          # is invalid?
          FALSE
        }
      }
      
      title <- trimws(input$title)
      title <- gsub("^([a-z])", "\\U\\1", title, perl = TRUE)
      if (empty_field("Titel", title) || invalid_title(title)) return(invisible())
      requested_by <- input$requested_by
      if (empty_field("Aanvrager(s)", requested_by)) return(invisible())
      filetype <- input$filetype
      if (empty_field("Bestandstype", filetype)) return(invisible())
      
      disable("create")
      disable("cancel")
      on.exit(disable("create"))
      on.exit(disable("cancel"))
      
      description <- trimws(input$description)
      if (length(description) == 0) {
        description <- ""
      }
      checklist <- input$checklist
      if (is.null(checklist) || length(checklist) == 0) {
        checklist <- ""
      }
      
      withProgress(message = "Aanmaken...", value = 0, {
        progress_items <- input$rstudio_projectfile + input$planner_upload * 2 + 1 # (+ 1 for creating folders)
        card_id <- NULL
        new_title <- title
        # Planner ----
        if (input$planner_upload == TRUE) {
          incProgress(1 / progress_items, detail = "MS Planner: taak aanmaken")
          if (is.null(input$deadline) || input$has_deadline == FALSE) {
            deadline <- NULL
          } else {
            deadline <- input$deadline
          }
          new_task <- planner_task_create(account = planner,
                                          title = title,
                                          bucket_name = input$planner_bucket,
                                          assigned = input$planner_members,
                                          requested_by = requested_by,
                                          priority = input$priority,
                                          duedate = deadline,
                                          checklist_items = checklist |> strsplit("\n") |> unlist(),
                                          categories = input$planner_categories,
                                          description = description)
          card_id <- new_task$id
          new_title <- new_task$title
        }
        # Teams ----
        if (tryCatch(input$files_teams_or_local == "teams", error = function(e) FALSE)) {
          incProgress(1 / progress_items, detail = "MS Teams: map aanmaken")
          teams_new_project(task = new_title, channel = channel, planner = planner)
          projects_path <- tempdir()
        } else {
          # saved to local
          projects_path <- ifelse(read_secret("projects.path") == "",
                                  getwd(),
                                  read_secret("projects.path"))
        }

        fullpath <- paste0(projects_path, "/",
                           trimws(gsub("(\\|/|:|\\*|\\?|\"|\\|)+", " ", title)),
                           ifelse(is.null(card_id), "", paste0(" - p", card_id)))
        fullpath <- gsub("//", "/", fullpath, fixed = TRUE)
        
        desc <- unlist(strsplit(description, "\n", fixed = TRUE))
        if (requested_by != "") {
          request <- get_user(id == requested_by, property = "name")
          if (request %in% c("", NA)) {
            request <- requested_by
          }
          request <- paste0("# Aangevraagd door: ", request)
        } else {
          request <- NA_character_
        }
        
        header_text <- c(paste0("# Titel:            ", title),
                         if_else(!is.na(desc[1]), 
                                 paste0("# Omschrijving:     ", desc[1]),
                                 NA_character_),
                         if_else(length(desc) > 1,
                                 paste0("#                   ", desc[2:length(desc)], collapse = "\n"),
                                 NA_character_),
                         if_else(!is.null(card_id),
                                 paste0("# Projectnummer:    p", card_id),
                                 NA_character_),
                         request,
                         paste0(        "# Aangemaakt op:    ", format2(Sys.time(), "d mmmm yyyy H:MM")))
        incProgress(1 / progress_items, detail = "Map aanmaken")
        # create folder
        if (!dir.exists(fullpath)) {
          dir.create(fullpath, recursive = TRUE, showWarnings = FALSE)
          message("N.B.: map '", fullpath, "' aangemaakt")
        }
        
        # create file(s)
        if (filetype %in% c(".Rmd", ".qmd")) {
          filecontent <- c(
            "---",
            paste0('title: "', title, '" # laat leeg voor geen voorblad bij PDF'),
            'subtitle: ""',
            'subtitle2: ""',
            'author: "`r certestyle::rmarkdown_author()`" # vervang evt. door certestyle::rmarkdown_department()',
            ifelse(filetype == ".qmd",
                   'date: "`r Sys.Date()`" # moet in YYYY-MM-DD',
                   'date: "`r certestyle::rmarkdown_date()`"'),
            ifelse(filetype == ".qmd", 'date-format: "D MMMM YYYY" # zie Quarto website', NA_character_),
            'identifier: "`r certeprojects::project_identifier()`"',
            "toc: true",
            "toc_depth: 2",
            "fig_width: 6.5 # in inch",
            "fig_height: 5  # in inch",
            "output:",
            "  # word_document:",
            ifelse(filetype == ".qmd", "  #   fig-dpi: 600", NA_character_),
            ifelse(filetype == ".qmd",
                   paste0("  #   reference-doc: \"", rmarkdown_template("word"), "\""),
                   '  #   reference_docx: !expr certestyle::rmarkdown_template("word")'),
            "  pdf_document:",
            ifelse(filetype == ".qmd", "    execute:", NA_character_),
            ifelse(filetype == ".qmd", "      echo: false", NA_character_),
            ifelse(filetype == ".qmd", "      warning: false", NA_character_),
            '    latex_engine: "xelatex"',
            "    df_print: !expr certestyle::rmarkdown_table",
            ifelse(filetype == ".qmd",
                   paste0("    template: \"", rmarkdown_template("latex"), "\""),
                   '    template: !expr certestyle::rmarkdown_template("latex")'),
            'logofront: "`r certestyle::rmarkdown_logo(\'front\')`"   # max 16x7 cm',
            'logofooter: "`r certestyle::rmarkdown_logo(\'footer\')`" # max 16x0.7 cm',
            "editor: visual",
            ifelse(filetype == ".qmd", 'lang: "nl"', NA_character_),
            "---",
            "")
          if (filetype == ".Rmd") {
            # R Markdown
            filecontent <- c(filecontent,
                             "```{r Setup, include = FALSE, message = FALSE}",
                             header_text,
                             "",
                             "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,",
                             '                      results = "asis", comment = NA, dpi = 600)')
          } else if (filetype == ".qmd") {
            # Quarto
            filecontent <- c(filecontent,
                             "```{r}",
                             "#| label: Setup",
                             "#| include: false",
                             "#| message: false",
                             "",
                             header_text)
          }
          filecontent <- c(filecontent,
                           "",
                           "library(certedata)",
                           "",
                           "data_download <- FALSE",
                           paste0('if (!is.na(project_get_file(".*rds$", ', card_id, ")) & !data_download) {"),
                           paste0("  data_", card_id, ' <- import_rds(project_get_file(".*rds$", ', card_id, "))"),
                           "} else {",
                           paste0("  data_", card_id, " <- certedb_getmmb(dates = c(start, stop),"),
                           "                             where  = where(db))",
                           paste0("  export_rds(data_", card_id, ', "data_', card_id, '", card_number = ', card_id, ')'),
                           "}",
                           "```",
                           "",
                           "# Inleiding",
                           "",
                           "```{r}",
                           "",
                           "```",
                           "")
          if (filetype == ".qmd") {
            # conversion for R Markdown -> Quarto
            filecontent <- gsub("output:", "format:", filecontent, fixed = TRUE)
            filecontent <- gsub("word_document", "docx", filecontent, fixed = TRUE)
            filecontent <- gsub("_document", "", filecontent, fixed = TRUE)
            filecontent <- gsub("(toc|fig)_", "\\1-", filecontent)
            filecontent <- gsub("latex_engine", "pdf-engine", filecontent, fixed = TRUE)
            filecontent[filecontent %like% "df_print: "] <- NA_character_
            filecontent <- gsub("reference-docx", "reference-doc", filecontent, fixed = TRUE)
          }
        }
        if (filetype == ".R") {
          filecontent <- c(header_text,
                           "",
                           "library(certedata)",
                           paste0("data_", card_id, " <- certedb_getmmb(dates = c(start, stop),"),
                           paste0(strrep(" ", nchar(card_id)), "                        where = where(db))"),
                           paste0("export_rds(data_", card_id, ', "data_', card_id, '.rds", card_number = ', card_id, ")"),
                           paste0("# data_", card_id, ' <- import_rds(project_get_file(".*rds$", ', card_id, '))'),
                           ""
          )
        }
        filename <- paste0(fullpath, "/Analyse",
                           ifelse(!is.null(card_id),
                                  paste0(" p", card_id),
                                  ""),
                           filetype)
        writeLines(text = paste(filecontent[!is.na(filecontent)], collapse = "\n"),
                   con = file.path(filename))
        if (input$files_teams_or_local == "teams") {
          teams_upload_project_file(files = file.path(filename), task = new_title, channel = channel, planner = planner)
          # unlink(file.path(filename), force = TRUE)
          unlink(fullpath, recursive = TRUE, force = TRUE)
        }
        
        incProgress(1 / progress_items, detail = ifelse(isTRUE(input$rstudio_projectfile), "R-bestanden schrijven", ""))
        Sys.sleep(0.1)
      })
      
      message("Project p", card_id, " aangemaakt.")
      
      if (isTRUE(input$rstudio_projectfile)) {
        # create project and open it
        initializeProject(fullpath)
        openProject(fullpath, newSession = TRUE)
      } else {
        if (input$files_teams_or_local == "teams") {
          teams_browse_project(task = new_title, channel = channel, planner = planner)
        } else {
          # open file
          navigateToFile(filename)
        }
      }
      
      stopApp()
    })
    
    # CANCEL ----
    observeEvent(input$cancel, {
      stopApp()
    })
  }
  
  viewer <- dialogViewer(dialogName = paste("Nieuw project -", ifelse(!is.null(teams),
                                                                      get_azure_property(teams, "displayName"),
                                                                      read_secret("department.name"))),
                         width = 800,
                         height = 680)
  
  suppressMessages(
    runGadget(app = ui,
              server = server,
              viewer = viewer,
              stopOnCancel = FALSE))
}

shorten_folder <- function(folder) {
  if (nchar(folder) < 40) {
    return(folder)
  }
  folder |>
    strsplit("/", fixed = TRUE) |>
    unlist() |>
    vapply(FUN.VALUE = character(1),
           function(x) ifelse(nchar(x) <= 6,
                              x,
                              paste0(substr(x, 1, 5),
                                     ifelse(isTRUE(base::l10n_info()$`UTF-8`),
                                            "\u2026",
                                            "...")))) |>
    paste(collapse = "/")
}

img_rstudio <- function() {
  "data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0idXRmLTgiPz4KPCEtLSBHZW5lcmF0b3I6IEFkb2JlIElsbHVzdHJhdG9yIDE5LjIuMSwgU1ZHIEV4cG9ydCBQbHVnLUluIC4gU1ZHIFZlcnNpb246IDYuMDAgQnVpbGQgMCkgIC0tPgo8c3ZnIHZlcnNpb249IjEuMSIgaWQ9IkxheWVyXzEiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgeG1sbnM6eGxpbms9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGxpbmsiIHg9IjBweCIgeT0iMHB4IgoJIHdpZHRoPSIzMDBweCIgaGVpZ2h0PSIxMDBweCIgdmlld0JveD0iMCAwIDMwMCAxMDAiIHN0eWxlPSJlbmFibGUtYmFja2dyb3VuZDpuZXcgMCAwIDMwMCAxMDA7IiB4bWw6c3BhY2U9InByZXNlcnZlIj4KPHN0eWxlIHR5cGU9InRleHQvY3NzIj4KCS5zdDB7ZmlsbDojNzRBQURCO30KCS5zdDF7ZmlsbDojNEU0RTRFO30KCS5zdDJ7ZmlsbDojRkZGRkZGO30KPC9zdHlsZT4KPGc+Cgk8Y2lyY2xlIGNsYXNzPSJzdDAiIGN4PSI1MSIgY3k9IjQ5LjkiIHI9IjUwIi8+Cgk8Zz4KCQk8cGF0aCBjbGFzcz0ic3QxIiBkPSJNMTExLjcsNjQuOGMyLjYsMS43LDYuMywyLjksMTAuMywyLjljNS45LDAsOS40LTMuMSw5LjQtNy42YzAtNC4xLTIuNC02LjYtOC40LTguOAoJCQljLTcuMy0yLjctMTEuOC02LjUtMTEuOC0xMi44YzAtNyw1LjgtMTIuMiwxNC41LTEyLjJjNC41LDAsNy45LDEuMSw5LjgsMi4ybC0xLjYsNC43Yy0xLjQtMC45LTQuNC0yLjEtOC40LTIuMQoJCQljLTYuMSwwLTguNCwzLjctOC40LDYuN2MwLDQuMiwyLjcsNi4zLDguOSw4LjZjNy42LDIuOSwxMS40LDYuNiwxMS40LDEzLjJjMCw2LjktNS4xLDEzLTE1LjYsMTNjLTQuMywwLTktMS4zLTExLjQtMi45CgkJCUwxMTEuNyw2NC44eiIvPgoJCTxwYXRoIGNsYXNzPSJzdDEiIGQ9Ik0xNTEuOCwzMS45djcuN2g4LjR2NC41aC04LjR2MTcuNGMwLDQsMS4xLDYuMyw0LjQsNi4zYzEuNiwwLDIuNS0wLjEsMy40LTAuNGwwLjMsNC41CgkJCWMtMS4xLDAuNC0yLjksMC44LTUuMiwwLjhjLTIuNywwLTQuOS0wLjktNi4zLTIuNWMtMS42LTEuOC0yLjMtNC43LTIuMy04LjRWNDQuMWgtNXYtNC41aDV2LTZMMTUxLjgsMzEuOXoiLz4KCQk8cGF0aCBjbGFzcz0ic3QxIiBkPSJNMTkzLjcsNjNjMCwzLjQsMC4xLDYuMywwLjMsOC44aC01LjJsLTAuMy01LjNoLTAuMWMtMS41LDIuNi00LjksNi0xMC42LDZjLTUuMSwwLTExLjEtMi45LTExLjEtMTQuMVYzOS42CgkJCWg1Ljl2MTcuOGMwLDYuMSwxLjksMTAuMyw3LjIsMTAuM2MzLjksMCw2LjYtMi43LDcuNy01LjRjMC4zLTAuOCwwLjUtMS45LDAuNS0zVjM5LjZoNS45VjYzSDE5My43eiIvPgoJCTxwYXRoIGNsYXNzPSJzdDEiIGQ9Ik0yMzEuMSwyNC42djM4LjljMCwyLjksMC4xLDYuMSwwLjMsOC4zaC01LjJsLTAuMy01LjZoLTAuMmMtMS43LDMuNi01LjYsNi4zLTEwLjgsNi4zCgkJCWMtNy44LDAtMTMuOC02LjYtMTMuOC0xNi40Yy0wLjEtMTAuNyw2LjYtMTcuMiwxNC40LTE3LjJjNSwwLDguMiwyLjMsOS43LDQuOWgwLjFWMjQuNkgyMzEuMXogTTIyNS40LDUyLjdjMC0wLjctMC4xLTEuNy0wLjMtMi41CgkJCWMtMC45LTMuNy00LjEtNi43LTguNC02LjdjLTYuMSwwLTkuNiw1LjMtOS42LDEyLjRjMCw2LjUsMy4zLDExLjksOS41LDExLjljMy45LDAsNy41LTIuNyw4LjYtN2MwLjItMC44LDAuMy0xLjYsMC4zLTIuNXYtNS42CgkJCUMyMjUuNSw1Mi43LDIyNS40LDUyLjcsMjI1LjQsNTIuN3oiLz4KCQk8cGF0aCBjbGFzcz0ic3QxIiBkPSJNMjQ3LjQsMzAuNmMwLDItMS40LDMuNi0zLjcsMy42Yy0yLjEsMC0zLjUtMS42LTMuNS0zLjZzMS41LTMuNywzLjctMy43QzI0NiwyNi45LDI0Ny40LDI4LjUsMjQ3LjQsMzAuNnoKCQkJIE0yNDAuOSw3MS44VjM5LjZoNS45djMyLjJIMjQwLjl6Ii8+CgkJPHBhdGggY2xhc3M9InN0MSIgZD0iTTI4NS42LDU1LjVjMCwxMS45LTguMywxNy4xLTE2LDE3LjFjLTguNiwwLTE1LjQtNi40LTE1LjQtMTYuNmMwLTEwLjcsNy4xLTE3LDE2LTE3CgkJCUMyNzkuNCwzOSwyODUuNiw0NS43LDI4NS42LDU1LjV6IE0yNjAuMSw1NS44YzAsNyw0LDEyLjQsOS43LDEyLjRjNS42LDAsOS44LTUuMyw5LjgtMTIuNWMwLTUuNS0yLjctMTIuMy05LjYtMTIuMwoJCQlDMjYzLjEsNDMuNCwyNjAuMSw0OS43LDI2MC4xLDU1Ljh6Ii8+Cgk8L2c+Cgk8Zz4KCQk8cGF0aCBjbGFzcz0ic3QyIiBkPSJNNjguMSw2NS45aDUuNHY0LjJoLTguM0w1MS42LDQ5LjVoLTcuM3YxNi4zaDcuMlY3MGgtMTh2LTQuMmg2LjFWMjkuN2wtNi4yLTAuOHYtNGMyLjMsMC41LDQuNCwwLjksNi45LDAuOQoJCQljMy44LDAsNy44LTAuOSwxMS42LTAuOWM3LjUsMCwxNC40LDMuNCwxNC40LDExLjdjMCw2LjQtMy44LDEwLjUtOS44LDEyLjJMNjguMSw2NS45eiBNNDQuMiw0NS41bDMuOSwwLjEKCQkJYzkuNiwwLjIsMTMuMy0zLjUsMTMuMy04LjRjMC01LjctNC4xLTgtOS40LThjLTIuNSwwLTUsMC4yLTcuOCwwLjVDNDQuMiwyOS43LDQ0LjIsNDUuNSw0NC4yLDQ1LjV6Ii8+Cgk8L2c+CjwvZz4KPC9zdmc+Cg=="
}

img_planner <- function() {
  "data:image/png;base64,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"
}

img_teams <- function() {
  "data:image/png;base64,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"
}
