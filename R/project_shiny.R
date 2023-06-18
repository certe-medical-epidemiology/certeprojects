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
#' This is a Shiny app to add a new project: it creates a project folder, generates the required Quarto or R Markdown or R files, and creates a new card in Trello. These functions come with RStudio addins to quickly access existing projects.
#' @inheritParams trello
#' @param account Microsoft Planner 'Plan', as returned by e.g. [planner_connect()]
#' @export
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel textInput textAreaInput uiOutput selectInput checkboxInput br p hr actionButton radioButtons renderUI tagList selectizeInput dateInput observeEvent updateTextInput runGadget stopApp dialogViewer incProgress withProgress tags icon mainPanel img a
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom shinyWidgets searchInput awesomeRadio awesomeCheckbox
#' @importFrom dplyr select pull filter if_else
#' @importFrom certestyle colourpicker format2
#  certestyle for R Markdown:
#' @importFrom certestyle rmarkdown_author rmarkdown_date rmarkdown_template rmarkdown_logo
#' @importFrom rstudioapi initializeProject openProject navigateToFile getActiveProject showDialog showQuestion
#' @rdname project_shiny
project_add <- function(board = read_secret("trello.default.board"),
                        username = trello_credentials("member"),
                        key = trello_credentials("key"),
                        token = trello_credentials("token"),
                        account = planner_connect()) {
  
  trello_set <- tryCatch(is.data.frame(trello_get_boards(username = username, key = key, token = token)),
                         error = function(e) FALSE)
  
  # ui ----
  ui <- fluidPage(
    useShinyjs(),
    
    # keys: 112-123 = F1-F12, date ('new Date()') needed to trigger again:
    # https://stackoverflow.com/a/44500961/4575331
    # F4 = accept, F8 = cancel
    tags$script('$(document).on("keydown", function (e) {
                if (e.which == 115) {
                Shiny.onInputChange("create", new Date());
                } else if (e.which == 119) {
                Shiny.onInputChange("cancel", new Date());
                }});
                $(document).ready(function() {
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
                      certeblauw, .certeblauw { color: ", colourpicker("certeblauw"), "; }
                      certeroze, .certeroze { color: ", colourpicker("certeroze"), "; }
                      .results_count { margin-left: 10px; }
                      label[for=title] { font-size: 16px; }
                      #create { background-color: ", colourpicker("certegroen"), "; border-color: ", colourpicker("certegroen"), "; }
                      #create:hover { background-color: ", colourpicker("certegroen2"), "; border-color: ", colourpicker("certegroen"), "; }
                      #cancel { background-color: ", colourpicker("certeroze"), "; border-color: ", colourpicker("certeroze"), ";}
                      #cancel:hover { background-color: ", colourpicker("certeroze2"), "; border-color: ", colourpicker("certeroze"), ";}
                      .multi .selectize-input .item, .selectize-dropdown .active { background-color: ", colourpicker("certeblauw6"), " !important; }
                      .selectize-input .item.active { color: white; background-color: ", colourpicker("certeblauw"), " !important; }",
                      '.checkbox-primary input[type="checkbox"]:checked+label::before, .checkbox-primary input[type="radio"]:checked+label::before { background-color: ', colourpicker("certeblauw"), "; border-color: ", colourpicker("certeblauw"), " ;}",
                      '.radio-primary input[type="radio"]:checked+label::before { border-color: ', colourpicker("certeblauw"), ";}",
                      '.radio-primary input[type="radio"]:checked+label::after { background-color: ', colourpicker("certeblauw"), ";}",
                      '.awesome-radio input[type="radio"]:focus+label::before, .awesome-checkbox input[type="checkbox"]:focus+label::before { outline: none; }')),
    
    sidebarLayout(
      sidebarPanel(
        textInput("title", "Titel", placeholder = ""),
        textAreaInput("description", "Omschrijving", cols = 1, rows = 2, resize = "vertical"),
        textAreaInput("checklist", "Taken", cols = 1, rows = 3, resize = "vertical", placeholder = "(1 taak per regel)"),
        uiOutput("requested_by"),
        selectInput("priority", "Prioriteit", c("Laag", "Normaal", "Hoog"),
                    ifelse(as.integer(format(Sys.Date(), "%u")) %in% c(6:7),
                           "Hoog", # default if card is created on weekend day
                           "Normaal")),
        if (as.integer(format(Sys.Date(), "%u")) %in% c(6:7)) {
          p(HTML("<i>De prioriteit is standaard 'Hoog' in het weekend</i>."))
        },
        tags$label("Deadline"),
        awesomeCheckbox("has_deadline", "Deadline instellen", TRUE),
        uiOutput("deadline"),
        textInput("topdesk", "Meldingsnummer TOPDesk", placeholder = ""),
        br(),
        br(),
        actionButton("create", "Aanmaken (F4)", width = "49%", icon = icon("check"), class = "btn-success"),
        actionButton("cancel", "Annuleren (F8)", width = "49%", icon = icon("ban"), class = "btn-danger"),
        width = 6),
      
      mainPanel(
        img(src = img_rstudio(), height = "45px"),
        br(),
        br(),
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
        img(src = img_planner(), height = "45px", style = "margin-bottom: 10px"),
        awesomeCheckbox("planner_upload", "Taak aanmaken in Microsoft Planner", FALSE),
        uiOutput("planner_settings"),
        hr(),
        if (isTRUE(trello_set)) img(src = img_trello(), height = "40px", style = "margin-top: 10px; margin-bottom: 10px"),
        if (isTRUE(trello_set)) awesomeCheckbox("trello_upload", "Kaart aanmaken op Trello.com", TRUE),
        if (isTRUE(trello_set)) uiOutput("trello_boards"),
        if (isTRUE(trello_set)) uiOutput("trello_settings"),
        if (isTRUE(trello_set)) uiOutput("trello_search_select"),
        width = 6
      )
    )
  )
  
  # server ----
  server <- function(input, output, session) {
    
    is_active_project <- !is.null(getActiveProject())
    
    output$requested_by <- renderUI({
      # retrieve user list with names and job titles
      users <- get_user()
      if (!is.null(users)) {
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
                    language = "nl")
        )
      }
    })
    
    output$planner_settings <- renderUI({
      if (input$planner_upload == TRUE) {
        members <- planner_user_property(property = "name", as_list = TRUE, account = account)
        active_user <- planner_user_property(get_current_user(), property = "name", account = account)
        if (length(active_user) == 0) {
          active_user <- NULL
        }
        url <- paste0("https://tasks.office.com/", account$tenant, ".onmicrosoft.com/Home/PlanViews/", account$properties$id)
        title <- account$properties$title
        tagList(
          h5(HTML(paste0("Verbonden met ", a(title, href =url), "."))),
          selectInput("planner_bucket",
                      label = "Bucket",
                      choices = list(Buckets = planner_buckets_list(plain = TRUE, account = account)),
                      selected = read_secret("planner.default.bucket"),
                      multiple = FALSE,
                      width = "100%"),
          selectizeInput("planner_members",
                         label = "Uitgevoerd door",
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
                         choices = unname(planner_categories_list(account = account)),
                         selected = NULL,
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
    
    output$trello_boards <- renderUI({
      if (input$trello_upload == TRUE) {
        boards_df <- trello_get_boards(username = username, key = key, token = token) |>
          filter(closed == FALSE) |>
          select(id, name, shortLink)
        boards_shortLink <- boards_df |> pull(shortLink)
        names(boards_shortLink) <- boards_df |> pull(name)
        
        slct <- selectInput("trello_boards",
                            label = "Bord",
                            choices = boards_shortLink,
                            selected = read_secret("trello.default.board"),
                            multiple = FALSE,
                            width = "100%")
        
        # hide others if only one Trello board is available
        if (length(boards_shortLink) == 1) {
          slct <- hidden(slct)
        }
        
        tagList(slct)
      }
    })
    
    output$trello_settings <- renderUI({
      if (input$trello_upload == TRUE) {
        
        board_selected <- input$trello_boards
        
        if (!is.null(board_selected)) {
          
          if (length(board_selected) > 1) {
            board_selected <- board_selected[1]
          }
          
          lists <- trello_get_lists(board = board_selected, key = key, token = token)$id
          names(lists) <- trello_get_lists(board = board_selected, key = key, token = token)$name
          
          members <- trello_get_members(board = board_selected, key = key, token = token)$fullName
          user <- trello_credentials("membername")
          if (user == "") {
            active <- NULL
          } else {
            active <- user
          }
          
          tagList(
            selectInput("trello_list",
                        label = "Lijst",
                        choices = list(Lijsten = lists),
                        selected = lists[3], # 3 = 'bezig'
                        multiple = FALSE,
                        width = "100%"),
            selectizeInput("trello_members",
                           label = "Uitgevoerd door",
                           choices = list(Leden = members),
                           selected = active,
                           multiple = TRUE,
                           width = "100%",
                           options = list(
                             # do not support non-existing members:
                             create = FALSE,
                             createOnBlur = FALSE,
                             closeAfterSelect = FALSE)),
            textAreaInput("trello_comments",
                          label = "Opmerkingen",
                          width = "395px",
                          cols = 1,
                          rows = 2,
                          resize = "vertical",
                          placeholder = ""),
            searchInput("trello_search",
                        label = "Gerelateerde project(en)",
                        value = "",
                        placeholder = "Zoeken in titel/beschrijving/taken/namen...",
                        btnSearch = icon("search"),
                        btnReset = icon("remove"),
                        width = "100%")
          )
        }
      }
    })
    
    
    output$trello_search_select <- renderUI({
      if (input$trello_upload == TRUE) {
        board_selected <- input$trello_boards
        searchterm <- input$trello_search
        if (!is.null(searchterm)) { # not at start up, then it's NULL. After this will be "".
          if (!is.null(board_selected)) {
            if (length(board_selected) > 1) {
              board_selected <- board_selected[1]
            }
            disable("trello_cards")
            found_cards <- trello_search_card(x = searchterm,
                                              return_all = TRUE,
                                              board = board_selected,
                                              key = key,
                                              token = token)
            enable("trello_cards")
            if (length(found_cards) > 0) {
              tagList(
                selectizeInput("trello_cards",
                               label = NULL,
                               choices = found_cards,
                               multiple = TRUE,
                               width = "100%",
                               options = list(
                                 # do not support non-existing cards:
                                 create = FALSE,
                                 createOnBlur = FALSE,
                                 closeAfterSelect = TRUE)),
                p(paste(length(found_cards),
                        if_else(length(found_cards) == 1,
                                "resultaat",
                                "resultaten.")),
                  class = "certeblauw results_count")
              )
            } else if (!identical(searchterm, "")) {
              tagList(
                p("Geen resultaten.",
                  class = "certeroze results_count")
              )
            }
          }
        }
      }
    })
    
    # SAVE ----
    observeEvent(input$create, {
      
      empty_field <- function(field, value) {
        if (all(is.null(value)) || length(value) == 0 || value == "") {
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
      if (!is.null(input$topdesk)) {
        topdesk <- trimws(input$topdesk)
        if (topdesk != "") {
          description <- paste0("TOPDesk-nummer: [",
                                topdesk,
                                "](https://topdesk.in.certe.nl/tas/public/ssp/content/search?q=",
                                topdesk, ")\n\n",
                                description)
        }
      }
      if (length(description) == 0) {
        description <- ""
      }
      checklist <- input$checklist
      if (all(is.null(checklist)) | length(checklist) == 0) {
        checklist <- ""
      }
      trello_cards <- input$trello_cards
      if (all(is.null(trello_cards)) | length(trello_cards) == 0) {
        trello_cards <- ""
      }
      
      withProgress(message = "Aanmaken...", value = 0, {
        progress_items <- input$rstudio_projectfile + input$planner_upload + input$trello_upload + 1 # (+ 1 for creating folders)
        # Planner ----
        if (input$planner_upload == TRUE) {
          incProgress(1 / progress_items, detail = "MS Planner taak aanmaken")
          if (is.null(input$deadline) || input$has_deadline == FALSE) {
            deadline <- NULL
          } else {
            deadline <- input$deadline
          }
          planner_task_create(account = account,
                              title = title,
                              bucket_name = input$planner_bucket,
                              assigned = input$planner_members,
                              requested_by = requested_by,
                              priority = input$priority,
                              duedate = deadline,
                              checklist_items = checklist |> strsplit("\n") |> unlist(),
                              categories = input$planner_categories,
                              descr = description)
        }
        # Trello ----
        trello_card_id <- NULL
        if (input$trello_upload == TRUE) {
          incProgress(1 / progress_items, detail = "Trello-kaart aanmaken")
          if (is.null(input$deadline) || input$has_deadline == FALSE) {
            deadline <- ""
          } else {
            deadline <- input$deadline
          }
          trello_card_id <- trello_upload(board = input$trello_boards,
                                          title = title,
                                          member = input$trello_members,
                                          requested_by = requested_by,
                                          project_path = "",
                                          list = input$trello_list,
                                          prio = input$priority,
                                          duedate = deadline,
                                          attachments = trello_cards,
                                          checklist = checklist |> strsplit("\n") |> unlist(),
                                          desc = description,
                                          comments = input$trello_comments,
                                          key = key,
                                          token = token)
        }
        projects_path <- read_secret("projects.path")
        if (projects_path == "") {
          warning("NOTE: Projects path not set, using current working directory: ", getwd(), call. = FALSE)
          projects_path <- getwd()
        }
        fullpath <- paste0(projects_path, "/",
                           trimws(gsub("(\\|/|:|\\*|\\?|\"|\\|)+", " ", title)),
                           ifelse(is.null(trello_card_id), "", paste0(" - p", trello_card_id)))
        fullpath <- gsub("//", "/", fullpath, fixed = TRUE)
        
        desc <- unlist(strsplit(input$description, "\n", fixed = TRUE))
        if (requested_by != "") {
          request <- get_user(id == requested_by, property = "name")
          if (request == "") {
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
                         if_else(!is.null(trello_card_id),
                                 paste0("# Projectnummer:    p", trello_card_id),
                                 NA_character_),
                         request,
                         if_else(!identical(trello_cards, ""),
                                 paste0("# Gerelateerd:      ", paste0("p",
                                                                       trello_get_cards()$idShort[trello_get_cards()$shortUrl %in% trello_cards],
                                                                       collapse = ", ")),
                                 NA_character_),
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
                           paste0('if (!is.na(project_get_file(".*rds$", ', trello_card_id, ")) & !data_download) {"),
                           paste0("  data_", trello_card_id, ' <- import_rds(project_get_file(".*rds$", ', trello_card_id, "))"),
                           "} else {",
                           paste0("  data_", trello_card_id, " <- certedb_getmmb(dates = c(start, stop),"),
                           "                             where  = where(db))",
                           paste0("  export_rds(data_", trello_card_id, ', "data_', trello_card_id, '", card_number = ', trello_card_id, ')'),
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
                           paste0("data_", trello_card_id, " <- certedb_getmmb(dates = c(start, stop),"),
                           paste0(strrep(" ", nchar(trello_card_id)), "                        where = where(db))"),
                           paste0("export_rds(data_", trello_card_id, ', "data_', trello_card_id, '.rds", card_number = ', trello_card_id, ")"),
                           paste0("# data_", trello_card_id, ' <- import_rds(project_get_file(".*rds$", ', trello_card_id, '))'),
                           ""
          )
        }
        filename <- paste0(fullpath, "/Analyse",
                           ifelse(!is.null(trello_card_id),
                                  paste0(" p", trello_card_id),
                                  ""),
                           filetype)
        writeLines(text = paste(filecontent[!is.na(filecontent)], collapse = "\n"),
                   con = file.path(filename))
        
        incProgress(1 / progress_items, detail = ifelse(isTRUE(input$rstudio_projectfile), "R-bestanden schrijven", ""))
        Sys.sleep(0.1)
      })
      
      message("Project p", trello_card_id, " aangemaakt.")
      
      if (isTRUE(input$rstudio_projectfile)) {
        # create project and open it
        initializeProject(fullpath)
        openProject(fullpath, newSession = TRUE)
      } else {
        # open file
        navigateToFile(filename)
      }
      
      stopApp()
    })
    
    # CANCEL ----
    observeEvent(input$cancel, {
      stopApp()
    })
  }
  
  viewer <- dialogViewer(dialogName = paste("Nieuw project -", read_secret("department.name")),
                         width = 850,
                         height = 780)
  
  suppressMessages(
    runGadget(app = ui,
              server = server,
              viewer = viewer,
              stopOnCancel = FALSE))
}

#' @rdname project_shiny
#' @param card_number Trello card number
#' @inheritParams trello
#' @importFrom shiny HTML h4 div h5
#' @importFrom shinyWidgets awesomeCheckbox
#' @importFrom dplyr filter pull case_when transmute
#' @importFrom shinyjs useShinyjs enable disable hidden
#' @importFrom rstudioapi showDialog
#' @importFrom cleaner clean_Date clean_logical
#' @export
project_edit <- function(card_number = project_get_current_id(ask = TRUE),
                         board = read_secret("trello.default.board"),
                         key = trello_credentials("key"),
                         token = trello_credentials("token")) {
  card_number <- gsub("[^0-9]", "", card_number)
  
  if (is.null(card_number) | all(is.na(card_number))) {
    return(invisible())
  }
  
  card_info <- trello_get_cards(board = board, key = key, token = token) |>
    filter(idShort == card_number) |>
    as.list()
  if (length(card_info$id) == 0) {
    stop(paste0("Project p", card_number, " not found on Trello"), call. = FALSE)
  }
  lists <- trello_get_lists(board = board, key = key, token = token)
  card_status <- lists |> filter(id == card_info$idList) |> pull(name)
  card_comments <- trello_get_comments(card_id = card_info$id, key = key, token = token)
  if (NROW(card_comments) > 0) {
    card_comments <- card_comments |>
      transmute(by = memberCreator.fullName,
                date = date,
                text = data.text)
  }
  
  card_members <- trello_get_members(board = board, key = key, token = token) |>
    filter(id %in% unlist(trello_get_card_property(card_number, "idMembers", board = board, key = key, token = token))) |>
    pull(fullName) |>
    paste(collapse = ", ")
  
  card_checklist <- trello_get_checklists(board = board, key = key, token = token) |>
    filter(idCard == card_info$id) |>
    pull(checkItems)
  card_checklist <- card_checklist[[1]]
  
  # ui ----
  ui <- fluidPage(
    useShinyjs(),
    
    # keys: 112-123 = F1-F12, datum ('new Date()') nodig om steeds opnieuw te kunnen triggeren:
    # https://stackoverflow.com/a/44500961/4575331
    # F4 = accept, F8 = cancel
    tags$script('$(document).on("keydown", function (e) {
                if (e.which == 115) {
                Shiny.onInputChange("save", new Date());
                } else if (e.which == 119) {
                Shiny.onInputChange("cancel", new Date());
                }});
                $(document).ready(function() {
                var textbox = document.getElementById("comment");
                textbox.focus();
                });'),
    tags$style(paste0("* { font-family: Calibri; }
                      .container-fluid { margin-top: 15px; }
                      .form-group { margin-bottom: 5px; }
                      .well .form-group { margin-bottom: 14px; }
                      .h2, h2 { color: ", colourpicker("certeblauw"), "; }
                      h5 { font-weight: bold }
                      certeblauw, .certeblauw { color: ", colourpicker("certeblauw"), "; }
                      certeroze { color: ", colourpicker("certeroze"), "; }
                      .results_count { margin-left: 10px; }
                      label[for=title] { font-size: 16px; }
                      .task input:checked ~ span { text-decoration: line-through; }
                      .comment { font-style: italic; margin: 0 }
                      .code { font-family: 'Fira Code', 'Courier New'; font-size: 13px; font-style: normal; color: ", colourpicker("certeblauw"), "; }
                      p { cursor: default; }
                      .comment_box { border: 1px solid #dddddd; width: fit-content; border-radius: 10px; padding: 5px; margin-bottom: 10px; background-color: ", colourpicker("certeblauw6"), " }
                      #save { background-color: ", colourpicker("certegroen"), "; border-color: ", colourpicker("certegroen"), "; }
                      #save:hover { background-color: ", colourpicker("certegroen2"), "; border-color: ", colourpicker("certegroen"), "; }
                      #cancel { background-color: ", colourpicker("certeroze"), "; border-color: ", colourpicker("certeroze"), ";}
                      #cancel:hover { background-color: ", colourpicker("certeroze2"), "; border-color: ", colourpicker("certeroze"), ";}
                      .multi .selectize-input .item, .selectize-dropdown .active { background-color: ", colourpicker("certeblauw6"), " !important; }
                      .selectize-input .item.active { color: white; background-color: ", colourpicker("certeblauw"), " !important; }",
                      '.checkbox-primary input[type="checkbox"]:checked+label::before, .checkbox-primary input[type="radio"]:checked+label::before { background-color: ', colourpicker("certeblauw"), "; border-color: ", colourpicker("certeblauw"), " ;}",
                      '.radio-primary input[type="radio"]:checked+label::before { border-color: ', colourpicker("certeblauw"), ";}",
                      '.radio-primary input[type="radio"]:checked+label::after { background-color: ', colourpicker("certeblauw"), ";}",
                      '.awesome-radio input[type="radio"]:focus+label::before, .awesome-checkbox input[type="checkbox"]:focus+label::before { outline: none; }')),
    sidebarLayout(
      sidebarPanel(
        uiOutput("style_tag"),
        tags$label("Titel", style = "font-size: 16px;"),
        HTML(paste0('<p class="last_update">', card_info$name, '</p>')),
        tags$label("Laatste update"),
        HTML(paste0('<p class="last_update">',
                    format2(as.Date(card_info$dateLastActivity), "dddd d mmmm yyyy"),
                    " (", as.integer(difftime(Sys.Date(), as.Date(card_info$dateLastActivity), units = "days")), " dgn ~=",
                    " ", as.integer(difftime(Sys.Date(), as.Date(card_info$dateLastActivity), units = "weeks")),
                    " wkn geleden)</p>")),
        selectInput("status", "Status", choices = lists$name, selected = card_status),
        tags$label("Deadline"),
        awesomeCheckbox("has_deadline", "Deadline instellen", value = ifelse(is.na(card_info$due), FALSE, TRUE)),
        uiOutput("deadline"),
        textAreaInput("comment", "Nieuwe opmerking", cols = 1, rows = 3, resize = "vertical"),
        br(),
        actionButton("save", "Opslaan (F4)", width = "43%", icon = icon("check"), class = "btn-success"),
        actionButton("cancel", "Annuleren (F8)", width = "43%", icon = icon("ban"), class = "btn-danger"),
        actionButton("trello_open", NULL, width = "11%", icon = icon("trello"), class = "btn-primary"),
        width = 7),
      
      mainPanel(
        uiOutput("tasks_and_comments"),
        width = 5)
    )
  )
  
  # server ----
  server <- function(input, output, session) {
    disable("title")
    
    output$deadline <- renderUI({
      val <- case_when(isTRUE(input$has_deadline) & !is.na(as.Date(card_info$due)) ~ as.Date(card_info$due),
                       isTRUE(input$has_deadline) ~  Sys.Date() + 14,
                       TRUE ~ as.Date(NA_character_))
      if (is.na(val)) {
        tagList()
      } else {
        tagList(
          dateInput("deadline", NULL,
                    value = val,
                    format = "DD d MM yyyy",
                    language = "nl"),
          awesomeCheckbox("deadline_finished",
                          "Deadline voltooid",
                          value = card_info$dueComplete & !is.na(card_info$due))
        )
      }
    })
    
    output$tasks_and_comments <- renderUI({
      desc <- unlist(strsplit(card_info$desc,  "\n\n"))
      desc <- desc[desc %unlike% "[*].*[*]"] # not those starting and ending with *, such as Aangevraagd door' and 'Maplocatie'
      desc <- paste(gsub("\n", "<br>", desc, fixed = TRUE), collapse = "\n")
      while (desc %like% "\\[.*\\]\\(.*\\)") {
        # URLs: markdown -> HTML
        desc <- sub("(.*)\\[(.*?)\\]\\((.*?)\\)(.*)", '\\1<a href="\\3">\\2</a>\\4', desc)
      }
      
      l <- tagList()
      
      if (length(desc) > 0 & desc != "") {
        l <- tagList(l, h4("Omschrijving"),
                     HTML(paste0("<p>", desc, "</p>")))
      }
      
      l <- tagList(l, HTML(paste0("<p><b>Uitgevoerd door: </b>", card_members, "</p>")))
      
      if (NROW(card_checklist) > 0) {
        l <- tagList(l, h4("Taken"))
        for (i in seq_len(nrow(card_checklist))) {
          l <- tagList(l,
                       div(awesomeCheckbox(inputId = card_checklist$id[i],
                                           label = card_checklist$name[i],
                                           value = isTRUE(card_checklist$state[i] == "complete"),
                                           width = "100%"),
                           class = "task"))
        }
      } else {
        l <- tagList(l, p("Nog geen taken opgegeven."))
      }
      l <- tagList(l, textAreaInput("newtasks",
                                    label = NULL,
                                    width = "100%",
                                    cols = 1,
                                    rows = 1,
                                    resize = "vertical",
                                    placeholder = "Nieuwe taak (1 per regel)"))
      
      l <- tagList(l, hr())
      
      if (NROW(card_comments) > 0) {
        l <- tagList(l, h4("Opmerkingen"))
        for (i in seq_len(nrow(card_comments))) {
          # create code from all '```' parts; which thus supports:
          #
          # ```test```
          #
          # and
          #
          # ```r
          # test
          # ```
          comment_text <- gsub("```([a-z]+\\n)?(.*?)(\\n)?```", '<div class="code">\\2</div>', card_comments$text[i])
          l <- tagList(l,
                       div(HTML(paste0('<p style="font-weight: bold; font-size: 11px;">', card_comments$by[i], " op ", format2(as.Date(card_comments$date[i]), "ddd d mmm yyyy"), ":</p>")),
                           HTML(paste0('<p class="comment">', comment_text, "</p>")),
                           class = "comment_box"))
        }
      } else {
        l <- tagList(l, h5("Geen opmerkingen."))
      }
      
      div(l, style = "height: 580px; overflow: auto;")
    })
    
    output$style_tag <- renderUI({
      if (input$status == "Bezig") {
        col1 <- colourpicker("certeblauw")
        col2 <- colourpicker("certeblauw4")
        col3 <- colourpicker("certeblauw6")
      } else if (input$status == "Voltooid") {
        col1 <- colourpicker("certegroen")
        col2 <- colourpicker("certegroen4")
        col3 <- colourpicker("certegroen6")
      } else if (input$status == "Wachten op een ander") {
        col1 <- colourpicker("certeroze")
        col2 <- colourpicker("certeroze4")
        col3 <- colourpicker("certeroze6")
      } else {
        col1 <- colourpicker("certeblauw")
        col2 <- "#f5f5f5"
        col3 <- "#f5f5f5"
      }
      return(tags$head(tags$style(HTML(paste0(".well { background-color:", col3, "; }
                                               .well label { color: ", col1, "; }
                                               .last_update { margin: 0 10px 10px; font-size: 13px; color: ", col1, "; }")))))
    })
    
    # SAVE ----
    observeEvent(input$save, {
      disable("save")
      disable("cancel")
      
      if (input$status == card_status & trimws(input$comment) == "" & input$status %unlike% "(wachten|voltooid)") {
        showDialog("Status gewijzigd", "Als de status gewijzigd is naar 'wachten op een ander' of 'voltooid', moet een opmerking ingevuld worden.")
      } else {
        
        # status
        if (input$status != card_status) {
          trello_move_card(card_id = card_info$id,
                           list_id = lists |> filter(name == input$status) |> pull(id), 
                           key = key,
                           token = token)
        }
        
        # deadline
        if (input$has_deadline == FALSE) {
          trello_set_deadline(card_id = card_info$id,
                              duedate = NULL,
                              duecomplete = TRUE, 
                              key = key,
                              token = token)
        } else {
          trello_set_deadline(card_id = card_info$id,
                              duedate = clean_Date(input$deadline),
                              duecomplete = clean_logical(input$deadline_finished), 
                              key = key,
                              token = token)
        }
        
        # comment
        if (trimws(input$comment) != "") {
          trello_set_comment(card_id = card_info$id,
                             comment = input$comment,
                             key = key,
                             token = token)
        }
        
        # members
        trello_set_members(card_id = card_info$id,
                           member = input$trello_members, 
                           board = board,
                           key = key,
                           token = token)
        
        # checklist
        if (NROW(card_checklist) > 0) {
          for (i in seq_len(nrow(card_checklist))) {
            trello_set_task_state(card_id = card_info$id,
                                  checkitem_id = card_checklist$id[i],
                                  new_value = input[[card_checklist$id[i]]],
                                  key = key,
                                  token = token)
          }
        }
        newtasks <- input$newtasks
        if (all(is.null(newtasks)) | length(newtasks) == 0) {
          newtasks <- ""
        }
        if (newtasks != "") {
          trello_add_task(card_id = card_info$id,
                          new_items = newtasks |> strsplit("\n") |> unlist(),
                          board = board,
                          key = key,
                          token = token)
        }
        
        stopApp()
      }
    })
    
    # CANCEL ----
    observeEvent(input$cancel, {
      stopApp()
    })
    
    # OPEN TRELLO ----
    observeEvent(input$trello_open, {
      utils::browseURL(card_info$url)
    })
    
  }
  
  viewer <- dialogViewer(dialogName = paste0("Project aanpassen, projectnummer p", card_number),
                         width = 800,
                         height = 620)
  
  suppressMessages(
    runGadget(app = ui,
              server = server,
              viewer = viewer,
              stopOnCancel = FALSE))
}

img_rstudio <- function() {
  concat(
    c("data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0idXRmLTgiPz4KPCEtLSBHZW5lcmF0b3I6IEFkb2JlIE",
      "lsbHVzdHJhdG9yIDE5LjIuMSwgU1ZHIEV4cG9ydCBQbHVnLUluIC4gU1ZHIFZlcnNpb246IDYuMDAgQnVpbGQgMCkgIC0tPgo8c3ZnIHZlcn",
      "Npb249IjEuMSIgaWQ9IkxheWVyXzEiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgeG1sbnM6eGxpbms9Imh0dHA6Ly93d3",
      "cudzMub3JnLzE5OTkveGxpbmsiIHg9IjBweCIgeT0iMHB4IgoJIHdpZHRoPSIzMDBweCIgaGVpZ2h0PSIxMDBweCIgdmlld0JveD0iMCAwID",
      "MwMCAxMDAiIHN0eWxlPSJlbmFibGUtYmFja2dyb3VuZDpuZXcgMCAwIDMwMCAxMDA7IiB4bWw6c3BhY2U9InByZXNlcnZlIj4KPHN0eWxlIH",
      "R5cGU9InRleHQvY3NzIj4KCS5zdDB7ZmlsbDojNzRBQURCO30KCS5zdDF7ZmlsbDojNEU0RTRFO30KCS5zdDJ7ZmlsbDojRkZGRkZGO30KPC",
      "9zdHlsZT4KPGc+Cgk8Y2lyY2xlIGNsYXNzPSJzdDAiIGN4PSI1MSIgY3k9IjQ5LjkiIHI9IjUwIi8+Cgk8Zz4KCQk8cGF0aCBjbGFzcz0ic3",
      "QxIiBkPSJNMTExLjcsNjQuOGMyLjYsMS43LDYuMywyLjksMTAuMywyLjljNS45LDAsOS40LTMuMSw5LjQtNy42YzAtNC4xLTIuNC02LjYtOC",
      "40LTguOAoJCQljLTcuMy0yLjctMTEuOC02LjUtMTEuOC0xMi44YzAtNyw1LjgtMTIuMiwxNC41LTEyLjJjNC41LDAsNy45LDEuMSw5LjgsMi",
      "4ybC0xLjYsNC43Yy0xLjQtMC45LTQuNC0yLjEtOC40LTIuMQoJCQljLTYuMSwwLTguNCwzLjctOC40LDYuN2MwLDQuMiwyLjcsNi4zLDguOS",
      "w4LjZjNy42LDIuOSwxMS40LDYuNiwxMS40LDEzLjJjMCw2LjktNS4xLDEzLTE1LjYsMTNjLTQuMywwLTktMS4zLTExLjQtMi45CgkJCUwxMT",
      "EuNyw2NC44eiIvPgoJCTxwYXRoIGNsYXNzPSJzdDEiIGQ9Ik0xNTEuOCwzMS45djcuN2g4LjR2NC41aC04LjR2MTcuNGMwLDQsMS4xLDYuMy",
      "w0LjQsNi4zYzEuNiwwLDIuNS0wLjEsMy40LTAuNGwwLjMsNC41CgkJCWMtMS4xLDAuNC0yLjksMC44LTUuMiwwLjhjLTIuNywwLTQuOS0wLj",
      "ktNi4zLTIuNWMtMS42LTEuOC0yLjMtNC43LTIuMy04LjRWNDQuMWgtNXYtNC41aDV2LTZMMTUxLjgsMzEuOXoiLz4KCQk8cGF0aCBjbGFzcz",
      "0ic3QxIiBkPSJNMTkzLjcsNjNjMCwzLjQsMC4xLDYuMywwLjMsOC44aC01LjJsLTAuMy01LjNoLTAuMWMtMS41LDIuNi00LjksNi0xMC42LD",
      "ZjLTUuMSwwLTExLjEtMi45LTExLjEtMTQuMVYzOS42CgkJCWg1Ljl2MTcuOGMwLDYuMSwxLjksMTAuMyw3LjIsMTAuM2MzLjksMCw2LjYtMi",
      "43LDcuNy01LjRjMC4zLTAuOCwwLjUtMS45LDAuNS0zVjM5LjZoNS45VjYzSDE5My43eiIvPgoJCTxwYXRoIGNsYXNzPSJzdDEiIGQ9Ik0yMz",
      "EuMSwyNC42djM4LjljMCwyLjksMC4xLDYuMSwwLjMsOC4zaC01LjJsLTAuMy01LjZoLTAuMmMtMS43LDMuNi01LjYsNi4zLTEwLjgsNi4zCg",
      "kJCWMtNy44LDAtMTMuOC02LjYtMTMuOC0xNi40Yy0wLjEtMTAuNyw2LjYtMTcuMiwxNC40LTE3LjJjNSwwLDguMiwyLjMsOS43LDQuOWgwLj",
      "FWMjQuNkgyMzEuMXogTTIyNS40LDUyLjdjMC0wLjctMC4xLTEuNy0wLjMtMi41CgkJCWMtMC45LTMuNy00LjEtNi43LTguNC02LjdjLTYuMS",
      "wwLTkuNiw1LjMtOS42LDEyLjRjMCw2LjUsMy4zLDExLjksOS41LDExLjljMy45LDAsNy41LTIuNyw4LjYtN2MwLjItMC44LDAuMy0xLjYsMC",
      "4zLTIuNXYtNS42CgkJCUMyMjUuNSw1Mi43LDIyNS40LDUyLjcsMjI1LjQsNTIuN3oiLz4KCQk8cGF0aCBjbGFzcz0ic3QxIiBkPSJNMjQ3Lj",
      "QsMzAuNmMwLDItMS40LDMuNi0zLjcsMy42Yy0yLjEsMC0zLjUtMS42LTMuNS0zLjZzMS41LTMuNywzLjctMy43QzI0NiwyNi45LDI0Ny40LD",
      "I4LjUsMjQ3LjQsMzAuNnoKCQkJIE0yNDAuOSw3MS44VjM5LjZoNS45djMyLjJIMjQwLjl6Ii8+CgkJPHBhdGggY2xhc3M9InN0MSIgZD0iTT",
      "I4NS42LDU1LjVjMCwxMS45LTguMywxNy4xLTE2LDE3LjFjLTguNiwwLTE1LjQtNi40LTE1LjQtMTYuNmMwLTEwLjcsNy4xLTE3LDE2LTE3Cg",
      "kJCUMyNzkuNCwzOSwyODUuNiw0NS43LDI4NS42LDU1LjV6IE0yNjAuMSw1NS44YzAsNyw0LDEyLjQsOS43LDEyLjRjNS42LDAsOS44LTUuMy",
      "w5LjgtMTIuNWMwLTUuNS0yLjctMTIuMy05LjYtMTIuMwoJCQlDMjYzLjEsNDMuNCwyNjAuMSw0OS43LDI2MC4xLDU1Ljh6Ii8+Cgk8L2c+Cg",
      "k8Zz4KCQk8cGF0aCBjbGFzcz0ic3QyIiBkPSJNNjguMSw2NS45aDUuNHY0LjJoLTguM0w1MS42LDQ5LjVoLTcuM3YxNi4zaDcuMlY3MGgtMT",
      "h2LTQuMmg2LjFWMjkuN2wtNi4yLTAuOHYtNGMyLjMsMC41LDQuNCwwLjksNi45LDAuOQoJCQljMy44LDAsNy44LTAuOSwxMS42LTAuOWM3Lj",
      "UsMCwxNC40LDMuNCwxNC40LDExLjdjMCw2LjQtMy44LDEwLjUtOS44LDEyLjJMNjguMSw2NS45eiBNNDQuMiw0NS41bDMuOSwwLjEKCQkJYz",
      "kuNiwwLjIsMTMuMy0zLjUsMTMuMy04LjRjMC01LjctNC4xLTgtOS40LThjLTIuNSwwLTUsMC4yLTcuOCwwLjVDNDQuMiwyOS43LDQ0LjIsND",
      "UuNSw0NC4yLDQ1LjV6Ii8+Cgk8L2c+CjwvZz4KPC9zdmc+Cg=="))
}

img_trello <- function() {
  concat(
    c("data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHN2ZyB3aWR0aD0iODg1cHgiIGhlaW",
      "dodD0iMjcycHgiIHZpZXdCb3g9IjAgMCA4ODUgMjcyIiB2ZXJzaW9uPSIxLjEiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2Zy",
      "IgeG1sbnM6eGxpbms9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGxpbmsiPgogICAgPCEtLSBHZW5lcmF0b3I6IFNrZXRjaCA0MSAoMzUzMj",
      "YpIC0gaHR0cDovL3d3dy5ib2hlbWlhbmNvZGluZy5jb20vc2tldGNoIC0tPgogICAgPHRpdGxlPnRyZWxsby1sb2dvLWJsdWUtZmxhdDwvdG",
      "l0bGU+CiAgICA8ZGVzYz5DcmVhdGVkIHdpdGggU2tldGNoLjwvZGVzYz4KICAgIDxkZWZzPgogICAgICAgIDxsaW5lYXJHcmFkaWVudCB4MT",
      "0iNTAlIiB5MT0iMCUiIHgyPSI1MCUiIHkyPSIxMDAlIiBpZD0ibGluZWFyR3JhZGllbnQtMSI+CiAgICAgICAgICAgIDxzdG9wIHN0b3AtY2",
      "9sb3I9IiMwMDc5QkYiIG9mZnNldD0iMCUiPjwvc3RvcD4KICAgICAgICAgICAgPHN0b3Agc3RvcC1jb2xvcj0iIzAwNzlCRiIgb2Zmc2V0PS",
      "IxMDAlIj48L3N0b3A+CiAgICAgICAgPC9saW5lYXJHcmFkaWVudD4KICAgIDwvZGVmcz4KICAgIDxnIGlkPSJQYWdlLTEiIHN0cm9rZT0ibm",
      "9uZSIgc3Ryb2tlLXdpZHRoPSIxIiBmaWxsPSJub25lIiBmaWxsLXJ1bGU9ImV2ZW5vZGQiPgogICAgICAgIDxnIGlkPSJMb2dvcyIgdHJhbn",
      "Nmb3JtPSJ0cmFuc2xhdGUoLTUwOS4wMDAwMDAsIC00ODUuMDAwMDAwKSI+CiAgICAgICAgICAgIDxnIGlkPSJHcm91cCIgdHJhbnNmb3JtPS",
      "J0cmFuc2xhdGUoLTkuMDAwMDAwLCAxLjAwMDAwMCkiPgogICAgICAgICAgICAgICAgPGcgaWQ9IlRyZWxsby1Mb2dvIiB0cmFuc2Zvcm09In",
      "RyYW5zbGF0ZSg0NjguMDAwMDAwLCAwLjAwMDAwMCkiPgogICAgICAgICAgICAgICAgICAgIDxnIGlkPSJUcmVsbG8tTG9nby0tLUJsdWUtLS",
      "1GbGF0IiB0cmFuc2Zvcm09InRyYW5zbGF0ZSgwLjAwMDAwMCwgNDIwLjAwMDAwMCkiPgogICAgICAgICAgICAgICAgICAgICAgICA8ZyBpZD",
      "0iTG9nbyIgdHJhbnNmb3JtPSJ0cmFuc2xhdGUoNTAuMDAwMDAwLCA2NC4wMDAwMDApIj4KICAgICAgICAgICAgICAgICAgICAgICAgICAgID",
      "xnIHRyYW5zZm9ybT0idHJhbnNsYXRlKDUwLjAwMDAwMCwgMS4wMDAwMDApIj4KICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA8cG",
      "F0aCBkPSJNNjczLjI5MzU0LDE3Ny41ODk1MjUgQzY2MC41MjI4NjgsMTgyLjc5MzE0NyA2NTAuNDQ1Mjc4LDIwMC43NzA4ODEgNjM1LjExOD",
      "QsMjEwLjk4ODggQzYzNC4xNjgsMjExLjYyMjQgNjMzLjIxNzYsMjExLjkzOTIgNjMyLjU4NCwyMTEuOTM5MiBDNjMxLjMxNjgsMjExLjkzOT",
      "IgNjI5LjczMjgsMjEwLjY3MiA2MjkuNzMyOCwyMDQuOTY5NiBDNjI5LjczMjgsMTg1LjMyOCA2MzYuMDY4OCwxNzUuODI0IDY0MS40NTQ0LD",
      "E2MS44ODQ4IEM2NjAuMTQ1NiwxMTMuNDE0NCA2OTIuMTQyNCw3MS45MTM2IDcyMC42NTQ0LDI2LjYxMTIgQzcyMi44NzIsMjMuMTI2NCA3Mj",
      "QuMTM5MiwxOS4zMjQ4IDcyNC4xMzkyLDE1LjIwNjQgQzcyNC4xMzkyLDExLjcyMTYgNzIyLjg3Miw4Ljg3MDQgNzIxLjI4OCw1LjM4NTYgQz",
      "cyMC4wMjA4LDIuNTM0NCA3MTQuMzE4NCwwIDcwNy45ODI0LDAgQzcwNC40OTc2LDAgNzAxLjMyOTYsMC4zMTY4IDY5Ny4yMTEyLDAuMzE2OC",
      "BDNjgyLjMyMTYsMC4zMTY4IDY3OS4xNTM2LDIyLjE3NiA2NzUuNjY4OCwyNy44Nzg0IEM2NTEuOTA4OCw2OC4xMTIgNjI0LjY2NCwxMTcuOD",
      "Q5NiA2MTAuNzI0OCwxNTMuMDE0NCBDNjA3LjY2ODEwMiwxNjAuODk3NDY0IDYwNC4zNjYzMDksMTY4Ljc4MDUyOCA2MDEuODU2NjMsMTc2Lj",
      "g3MTAzNCBDNTg4LjA4MTg4MywxODAuODU5ODI2IDU3Ny43NjI0NTQsMjAwLjIyNzY5NyA1NjEuNjIwOCwyMTAuOTg4OCBDNTYwLjY3MDQsMj",
      "ExLjYyMjQgNTU5LjcyLDIxMS45MzkyIDU1OS4wODY0LDIxMS45MzkyIEM1NTcuODE5MiwyMTEuOTM5MiA1NTYuMjM1MiwyMTAuNjcyIDU1Ni",
      "4yMzUyLDIwNC45Njk2IEM1NTYuMjM1MiwxODUuMzI4IDU2Mi41NzEyLDE3NS44MjQgNTY3Ljk1NjgsMTYxLjg4NDggQzU4Ni42NDgsMTEzLj",
      "QxNDQgNjE4LjY0NDgsNzEuOTEzNiA2NDcuMTU2OCwyNi42MTEyIEM2NDkuMzc0NCwyMy4xMjY0IDY1MC42NDE2LDE5LjMyNDggNjUwLjY0MT",
      "YsMTUuMjA2NCBDNjUwLjY0MTYsMTEuNzIxNiA2NDkuMzc0NCw4Ljg3MDQgNjQ3Ljc5MDQsNS4zODU2IEM2NDYuNTIzMiwyLjUzNDQgNjQwLj",
      "gyMDgsMCA2MzQuNDg0OCwwIEM2MzEsMCA2MjcuODMyLDAuMzE2OCA2MjMuNzEzNiwwLjMxNjggQzYwOC44MjQsMC4zMTY4IDYwNS42NTYsMj",
      "IuMTc2IDYwMi4xNzEyLDI3Ljg3ODQgQzU3OC40MTEyLDY4LjExMiA1NTEuMTY2NCwxMTcuODQ5NiA1MzcuMjI3MiwxNTMuMDE0NCBDNTM2Lj",
      "ExOTAzOCwxNTUuODcyMjkyIDUzNC45Nzg2NjIsMTU4LjczMDE4NSA1MzMuODU1NDk1LDE2MS41OTc5NjIgQzUzMy41MDg2NTksMTYxLjc4Mz",
      "IxOCA1MzMuMTU0NDc3LDE2MS45ODQxMTQgNTMyLjc5MiwxNjIuMjAxNiBDNTE2LjYzNTIsMTcxLjcwNTYgNTAzLjAxMjgsMTg4LjQ5NiA0OD",
      "MuMzcxMiwxOTkuOTAwOCBDNDc5LjU2OTYsMjAyLjExODQgNDY3Ljg0OCwyMTAuMzU1MiA0NTguMzQ0LDIxMC4zNTUyIEM0NTYuMTI2NCwyMT",
      "AuMzU1MiA0NTQuMjI1NiwyMDkuNzIxNiA0NTIuMzI0OCwyMDguNzcxMiBDNDQ5LjQ3MzYsMjA3LjUwNCA0NDYuNjIyNCwyMDEuNDg0OCA0ND",
      "YuNjIyNCwxOTkuNTg0IEM0NDYuNjIyNCwxOTggNDQ2LjkzOTIsMTk3LjY4MzIgNDUwLjEwNzIsMTk1Ljc4MjQgQzQ3Ny45ODU2LDE3OC45OT",
      "IgNTAwLjc5NTIsMTU0LjkxNTIgNTIwLjQzNjgsMTMwLjgzODQgQzUyNy43MjMyLDEyMS45NjggNTM3LjIyNzIsMTA0LjU0NCA1MzcuMjI3Mi",
      "w5MS44NzIgQzUzNy4yMjcyLDgzLjYzNTIgNTM0LjM3Niw3NC4xMzEyIDUyNC41NTUyLDcwLjY0NjQgQzUxNy41ODU2LDY4LjExMiA1MDkuNj",
      "Y1Niw2Ni44NDQ4IDUwMy4zMjk2LDY2Ljg0NDggQzQ4Ni41MzkyLDY2Ljg0NDggNDc0LjE4NCw3NC40NDggNDY3LjIxNDQsODEuNzM0NCBDND",
      "YwLjExODk2Miw4OS4zMDkyNTk2IDQ1My4yNTU2OSw5Ny4wODcyNjUgNDQ2Ljg3OTMxLDEwNS4xMzg2ODUgQzQ0MC4yMTcwMDQsOTkuMzg2OD",
      "Q5NiA0MzAuODE5ODk3LDk2Ljk0MDggNDIxLjkxMiw5Ni45NDA4IEM0MTEuNDU3Niw5Ni45NDA4IDM5MS44MTYsMTA5LjkyOTYgMzgxLjY3OD",
      "QsMTE3LjUzMjggQzM4MC4wOTQ0LDExOC44IDM3OS4xNDQsMTE5LjQzMzYgMzc4LjUxMDQsMTE5LjQzMzYgQzM3OC4xOTM2LDExOS40MzM2ID",
      "M3Ny44NzY4LDExOS4xMTY4IDM3Ny44NzY4LDExOC40ODMyIEMzNzcuODc2OCwxMTguMTY2NCAzNzguNTEwNCwxMTUuMzE1MiAzNzguNTEwNC",
      "wxMTAuMjQ2NCBDMzc4LjUxMDQsMTA1LjgxMTIgMzc3LjU2LDEwMC4xMDg4IDM3My40NDE2LDkzLjEzOTIgQzM3Mi40OTEyLDkxLjU1NTIgMz",
      "Y2Ljc4ODgsODguMDcwNCAzNTkuODE5Miw4OC4wNzA0IEMzNTEuMjY1Niw4OC4wNzA0IDM0My4zNDU2LDkyLjE4ODggMzQzLjM0NTYsOTYuNj",
      "I0IEMzNDMuMzQ1Niw5OS43OTIgMzQ2LjE5NjgsMTAxLjA1OTIgMzQ2LjE5NjgsMTAzLjkxMDQgQzM0Ni4xOTY4LDEwNS40OTQ0IDM0NC45Mj",
      "k2LDExMi43ODA4IDM0My4wMjg4LDExOS43NTA0IEMzMzcuNjQzMiwxNDAuMDI1NiAzMzAuOTkwNCwxNTkuOTg0IDMyNC4wMjA4LDE3OS45ND",
      "I0IEMzMjAuMjE5MiwxOTEuMDMwNCAzMDcuNTQ3MiwyMDAuODUxMiAzMDcuNTQ3MiwyMTIuODg5NiBDMzA3LjU0NzIsMjE2LjY5MTIgMzEwLj",
      "A4MTYsMjIxLjc2IDMxMy41NjY0LDIyNS41NjE2IEMzMTkuMjY4OCwyMzEuODk3NiAzMjIuNzUzNiwyMzQuMTE1MiAzMjcuODIyNCwyMzQuMT",
      "E1MiBDMzMwLjA0LDIzNC4xMTUyIDMzMi41NzQ0LDIzMy40ODE2IDMzNC43OTIsMjMxLjU4MDggQzMzOS41NDQsMjI3LjQ2MjQgMzQyLjA3OD",
      "QsMjIzLjM0NCAzNDMuMzQ1NiwyMTcuNjQxNiBDMzUxLjU4MjQsMTgwLjI1OTIgMzc1LjM0MjQsMTU0LjkxNTIgNDAxLjYzNjgsMTM4LjEyND",
      "ggQzQxMC41MDcyLDEzMi40MjI0IDQyMi41NDU2LDEyNi40MDMyIDQyNC4xMjk2LDEyNi40MDMyIEM0MjUuNzUzODM5LDEyNi40MDMyIDQyOC",
      "4yOTQxMDMsMTI3LjE1MjY3NSA0MzAuODUzNzkzLDEyNy44ODMxMTEgQzQyMy41MDM1MjIsMTM5Ljg3MTk2OSA0MTcuNTQ3OTM5LDE1Mi41MT",
      "UzODYgNDEzLjY3NTIsMTY2LjAwMzIgQzQxMi40MDgsMTcwLjQzODQgNDExLjc3NDQsMTc0LjU1NjggNDExLjc3NDQsMTc4Ljk5MiBDNDExLj",
      "c3NDQsMTg2LjI3ODQgNDEzLjM1ODQsMTkzLjg4MTYgNDE1Ljg5MjgsMjAxLjE2OCBDNDE5LjM3NzYsMjExLjMwNTYgNDI1LjA4LDIyMC4xNz",
      "YgNDMzLDIyNC4yOTQ0IEM0NDcuNTcyOCwyMzEuODk3NiA0NTguMDI3MiwyMzYuMDE2IDQ2Ny41MzEyLDIzNi4wMTYgQzQ3Mi45MTY4LDIzNi",
      "4wMTYgNDc3LjAzNTIsMjM1LjA2NTYgNDgxLjQ3MDQsMjMyLjUzMTIgQzUwMi4yOTI2NzMsMjIwLjc1MDE3NyA1MTUuMDU4ODg0LDIxMS44MT",
      "I0NyA1MjQuMjc3MTU3LDIwMy44NzM4NDYgQzUyNC41Mjk5MTgsMjExLjkyMjkyMyA1MjYuMTE2NTY2LDIxNi4zODIzMTQgNTMxLjg0MTYsMj",
      "IyLjM5MzYgQzUzNy41NDQsMjI4LjQxMjggNTQ2LjQxNDQsMjMzLjc5ODQgNTU4LjEzNiwyMzYuMzMyOCBDNTYwLjAzNjgsMjM2LjY0OTYgNT",
      "YxLjkzNzYsMjM2Ljk2NjQgNTYzLjgzODQsMjM2Ljk2NjQgQzU3Ny4yNDk0OTksMjM2Ljk2NjQgNTkwLjE1NTk0NywyMjYuMDY1OTQxIDYwMC",
      "4xMDgwNzUsMjE1LjI2MDA0OSBDNjAxLjI2MDM0MiwyMTcuNjE4MDg4IDYwMi45MzQ4LDIxOS44Njg5OCA2MDUuMzM5MiwyMjIuMzkzNiBDNj",
      "ExLjA0MTYsMjI4LjQxMjggNjE5LjkxMiwyMzMuNzk4NCA2MzEuNjMzNiwyMzYuMzMyOCBDNjMzLjUzNDQsMjM2LjY0OTYgNjM1LjQzNTIsMj",
      "M2Ljk2NjQgNjM3LjMzNiwyMzYuOTY2NCBDNjUzLjA0NTc2NywyMzYuOTY2NCA2NjguMDYzMDYyLDIyMi4wMDkwMiA2NzguNDUwMzYsMjA5Lj",
      "c2NzM0NSBDNjgxLjkxMjI4MiwyMTYuOTI3ODQ4IDY5MS41ODY1NDMsMjI0LjAwNzM1MSA3MDMuMjMwNCwyMjkuNjggQzcwNi4zOTg0LDIzMS",
      "4yNjQgNzEwLjIsMjMyLjIxNDQgNzEzLjY4NDgsMjMyLjIxNDQgQzcyNy4zMDcyLDIzMi4yMTQ0IDczNy4xMjgsMjIxLjEyNjQgNzQ0LjczMT",
      "IsMjEyLjg4OTYgQzc2NC4zNzI4LDE5MS4zNDcyIDc3NS4xNDQsMTY0LjczNiA3ODMuMzgwOCwxMzEuMTU1MiBDNzg0LjAxNDQsMTI4LjYyMD",
      "ggNzg0Ljk2NDgsMTI3LjY3MDQgNzg2LjIzMiwxMjcuNjcwNCBDNzg5LjQsMTI3LjY3MDQgNzkzLjUxODQsMTI3LjY3MDQgNzk3Ljk1MzYsMT",
      "I3LjAzNjggQzgwNi41MDcyLDEyNS43Njk2IDgxMy4xNiwxMjIuOTE4NCA4MjEuMDgsMTIxLjY1MTIgQzgyNS44MzIsMTIwLjcwMDggODI1Lj",
      "E5ODQsMTE5LjExNjggODI5LjYzMzYsMTE3LjUzMjggQzgzMi44MDE2LDExNi4yNjU2IDgzNS4zMzYsMTE0Ljk5ODQgODM1LjMzNiwxMTEuOD",
      "MwNCBDODM1LjMzNiwxMDYuNDQ0OCA4MjUuNTE1MiwxMDAuNzQyNCA4MTAuNjI1NiwxMDAuNzQyNCBDNzk4LjkwNCwxMDAuNzQyNCA3OTEuOT",
      "M0NCwxMDEuNjkyOCA3ODcuMTgyNCwxMDEuNjkyOCBDNzc5Ljg5NiwxMDEuNjkyOCA3NzcuOTk1Miw5OS40NzUyIDc3My4yNDMyLDg3LjEyIE",
      "M3NzMuMjQzMiw4Ny4xMiA3NzcuOTk1Miw5OS40NzUyIDc3My4yNDMyLDg3LjEyIEM3NzIuMjkyOCw4NC41ODU2IDc3MS4zNDI0LDgzLjMxOD",
      "QgNzY4LjQ5MTIsODAuNzg0IEM3NjEuODM4NCw3NS4wODE2IDc1My42MDE2LDczLjE4MDggNzQ2Ljk0ODgsNzMuMTgwOCBDNzMxLjEwODgsNz",
      "MuMTgwOCA3MTcuNDg2NCw4OS4zMzc2IDcwNy45ODI0LDEwMi45NiBDNzA1Ljc2NDgsMTA2LjEyOCA3MDIuNTk2OCwxMDguNjYyNCA3MDAuMz",
      "c5MiwxMTIuMTQ3MiBDNjg3LjMyOTE4NSwxMzEuNzIyMjIyIDY3NC44MzQ1MTMsMTUzLjc5NjI5IDY3My4yOTM1NCwxNzcuNTg5NTI1IFogTT",
      "I2OC4yNjQsNjkuNjk2IEMyNzYuMTg0LDY5LjY5NiAyODAuNjE5Miw2OS4wNjI0IDI4MS41Njk2LDY5LjA2MjQgQzI4Mi41Miw2OS4wNjI0ID",
      "I4My4xNTM2LDY5LjM3OTIgMjgzLjE1MzYsNzAuMzI5NiBDMjgzLjE1MzYsNzAuOTYzMiAyODIuODM2OCw3MS45MTM2IDI4MC42MTkyLDc2Lj",
      "Y2NTYgQzI2MS4yOTQ0LDExOC4xNjY0IDI0Ny45ODg4LDE2MC4zMDA4IDIzNi4yNjcyLDIwNi44NzA0IEMyMzUuOTUwNCwyMDguMTM3NiAyMz",
      "UsMjEyLjg4OTYgMjM1LDIxNy42NDE2IEMyMzUsMjIyLjA3NjggMjM2LjI2NzIsMjI2LjgyODggMjQwLjcwMjQsMjI5LjM2MzIgQzI0OS4yNT",
      "YsMjM0LjQzMiAyNTYuMjI1NiwyMzYuNjQ5NiAyNjAuOTc3NiwyMzYuNjQ5NiBDMjY3Ljk0NzIsMjM2LjY0OTYgMjcxLjQzMiwyMzIuMjE0NC",
      "AyNzEuNDMyLDIyNC45MjggQzI3MS40MzIsMjE4LjU5MiAyNzEuNzQ4OCwyMTEuOTM5MiAyNzIuNjk5MiwyMDYuODcwNCBDMjgxLjU2OTYsMT",
      "YwLjMwMDggMjk0Ljg3NTIsMTIzLjg2ODggMzE1LjQ2NzIsODUuODUyOCBDMzI1LjI4OCw2Ny43OTUyIDMyNi41NTUyLDY2Ljg0NDggMzI2Lj",
      "U1NTIsNjUuMjYwOCBDMzI2LjU1NTIsNjQuNjI3MiAzMjYuMjM4NCw2My42NzY4IDMyNS42MDQ4LDYyLjcyNjQgQzMzOC4yNzY4LDU5Ljg3NT",
      "IgMzUzLjQ4MzIsNTcuOTc0NCAzNjcuNDIyNCw1Ny45NzQ0IEMzNjkuMDA2NCw1Ny45NzQ0IDM3NS4zNDI0LDYwLjE5MiAzNzYuMjkyOCw2MS",
      "4xNDI0IEMzNzguNTEwNCw2My4zNiAzODAuNzI4LDY1LjU3NzYgMzg0LjUyOTYsNjUuNTc3NiBDMzg3LjY5NzYsNjUuNTc3NiAzOTMuMDgzMi",
      "w2My4zNiAzOTQuOTg0LDYyLjA5MjggQzM5OS40MTkyLDU4LjkyNDggNDAxLjYzNjgsNTUuNDQgNDAxLjYzNjgsNDkuNzM3NiBDNDAxLjYzNj",
      "gsNDQuMzUyIDM4MS42Nzg0LDMxLjA0NjQgMzY3LjczOTIsMzEuMDQ2NCBDMzUwLjYzMiwzMS4wNDY0IDMzNS4xMDg4LDMzLjI2NCAzMjAuNT",
      "M2LDM2LjExNTIgQzMxNC4yLDM3LjM4MjQgMjkxLjM5MDQsNDAuODY3MiAyNzQuOTE2OCw0MC44NjcyIEMyNTkuMDc2OCw0MC44NjcyIDI2MC",
      "4wMjcyLDM4LjY0OTYgMjUzLjM3NDQsMzguNjQ5NiBDMjQ5LjU3MjgsMzguNjQ5NiAyNDcuNjcyLDQwLjU1MDQgMjQ2LjA4OCw0Mi4xMzQ0IE",
      "MyNDQuNTA0LDQzLjcxODQgMjQzLjU1MzYsNTAuMDU0NCAyNDMuNTUzNiw1NS43NTY4IEMyNDMuNTUzNiw1OC42MDggMjQzLjU1MzYsNjAuOD",
      "I1NiAyNDUuMTM3Niw2Mi43MjY0IEMyNDkuODg5Niw2OC40Mjg4IDI1OS4wNzY4LDY5LjY5NiAyNjguMjY0LDY5LjY5NiBDMjY4LjI2NCw2OS",
      "42OTYgMjU5LjA3NjgsNjkuNjk2IDI2OC4yNjQsNjkuNjk2IFogTTUwMy4wMTI4LDk1LjM1NjggQzUwMy4wMTI4LDk4LjIwOCA1MDIuMDYyNC",
      "wxMDAuMTA4OCA0OTguMjYwOCwxMDYuMTI4IEM0OTQuNDU5MiwxMTIuMTQ3MiA0OTYuMDQzMiwxMTIuMTQ3MiA0OTEuMjkxMiwxMTguNDgzMi",
      "BDNDgyLjEwNCwxMzAuODM4NCA0NzAuNjk5MiwxNDMuMTkzNiA0NTUuODA5NiwxNTYuODE2IEM0NTEuNjkxMiwxNjAuNjE3NiA0NTEuMDU3Ni",
      "wxNjAuNjE3NiA0NTAuNDI0LDE2MC42MTc2IEM0NTAuMTA3MiwxNjAuNjE3NiA0NDkuNDczNiwxNjAuMzAwOCA0NDkuNDczNiwxNTkuNjY3Mi",
      "BDNDQ5LjQ3MzYsMTU4LjcxNjggNDQ5Ljc5MDQsMTU3LjQ0OTYgNDUyLjk1ODQsMTUwLjc5NjggQzQ2Mi43NzkyLDEzMC4yMDQ4IDQ3NC44MT",
      "c2LDExNi41ODI0IDQ4OC4xMjMyLDEwMy4yNzY4IEM0OTUuNDA5Niw5NS45OTA0IDQ5OS44NDQ4LDkzLjQ1NiA1MDEuNDI4OCw5My40NTYgQz",
      "UwMi4zNzkyLDkzLjQ1NiA1MDMuMDEyOCw5My43NzI4IDUwMy4wMTI4LDk1LjM1NjggQzUwMy4wMTI4LDk1LjM1NjggNTAzLjAxMjgsOTMuNz",
      "cyOCA1MDMuMDEyOCw5NS4zNTY4IFogTTc0Ni42MzIsMTAzLjU5MzYgQzc0Ni45NDg4LDEwMy41OTM2IDc0Ny4yNjU2LDEwMy45MTA0IDc0Ny",
      "41ODI0LDEwNC41NDQgQzc0Ny44OTkyLDEwNS40OTQ0IDc0OC41MzI4LDEwNi40NDQ4IDc1MC4xMTY4LDEwNy43MTIgQzc1MS4zODQsMTA4Lj",
      "Y2MjQgNzUxLjM4NCwxMTMuMDk3NiA3NTEuMzg0LDExNi41ODI0IEM3NTEuMzg0LDE0Ny4zMTIgNzMzLjk2LDE3Mi42NTYgNzE2Ljg1MjgsMT",
      "k4LjMxNjggQzcxMy4wNTEyLDIwNC4wMTkyIDcxMC44MzM2LDIwNC45Njk2IDcwOC45MzI4LDIwNC45Njk2IEM3MDcuMzQ4OCwyMDQuOTY5Ni",
      "A3MDQuODE0NCwyMDAuODUxMiA3MDMuODY0LDE5OC4zMTY4IEM3MDIuNTk2OCwxOTQuODMyIDcwMi4yOCwxOTAuNzEzNiA3MDIuMjgsMTg5Lj",
      "EyOTYgQzcwMi4yOCwxNjQuNDE5MiA3MjUuNzIzMiwxMjcuNjcwNCA3NDEuMjQ2NCwxMDcuMzk1MiBDNzQzLjc4MDgsMTAzLjkxMDQgNzQ1Lj",
      "Y4MTYsMTAzLjU5MzYgNzQ2LjYzMiwxMDMuNTkzNiBDNzQ2LjYzMiwxMDMuNTkzNiA3NDUuNjgxNiwxMDMuNTkzNiA3NDYuNjMyLDEwMy41OT",
      "M2IFoiIGlkPSJUeXBlIiBmaWxsPSIjMDA3OUJGIj48L3BhdGg+CiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgPGcgaWQ9Ik1hcm",
      "siIHRyYW5zZm9ybT0idHJhbnNsYXRlKDAuMDAwMDAwLCAzNS4wMDAwMDApIj4KICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC",
      "AgPHJlY3QgaWQ9IkJvYXJkIiBmaWxsPSJ1cmwoI2xpbmVhckdyYWRpZW50LTEpIiB4PSIwIiB5PSIwIiB3aWR0aD0iMjAwIiBoZWlnaHQ9Ij",
      "IwMCIgcng9IjI1Ij48L3JlY3Q+CiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIDxyZWN0IGlkPSJSaWdodC1MaXN0IiBmaW",
      "xsPSIjRkZGRkZGIiB4PSIxMTMiIHk9IjI2IiB3aWR0aD0iNjEiIGhlaWdodD0iODcuNSIgcng9IjEyIj48L3JlY3Q+CiAgICAgICAgICAgIC",
      "AgICAgICAgICAgICAgICAgICAgICAgIDxyZWN0IGlkPSJMZWZ0LUxpc3QiIGZpbGw9IiNGRkZGRkYiIHg9IjI2IiB5PSIyNiIgd2lkdGg9Ij",
      "YxIiBoZWlnaHQ9IjEzNy41IiByeD0iMTIiPjwvcmVjdD4KICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA8L2c+CiAgICAgICAgIC",
      "AgICAgICAgICAgICAgICAgICA8L2c+CiAgICAgICAgICAgICAgICAgICAgICAgIDwvZz4KICAgICAgICAgICAgICAgICAgICA8L2c+CiAgIC",
      "AgICAgICAgICAgICA8L2c+CiAgICAgICAgICAgIDwvZz4KICAgICAgICA8L2c+CiAgICA8L2c+Cjwvc3ZnPg=="))
}

img_planner <- function() {
  concat(
    c("data:image/png;base64,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"
    ))
    }
