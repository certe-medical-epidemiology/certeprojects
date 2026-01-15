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

#' Add Project Or Consult Using Shiny
#' 
#' This is a Shiny app to add a new project: it creates a project folder locally [or in Teams][connect_teams()], generates the required Quarto or R Markdown or R file, and creates a [new task in Planner][connect_planner()]. These functions come with RStudio addins to quickly access existing projects. For consults, it only adds a Planner task and creates the card in the background.
#' @param planner Microsoft Planner account, as returned by e.g. [connect_planner()]
#' @param teams Microsoft Teams account, as returned by e.g. [connect_teams()]
#' @param channel Microsoft Teams Channel folder, as returned by e.g. [teams_projects_channel()]
#' @param outlook MIcrosoft Outlook account, as returned by e.g. [connect_outlook()]
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel textInput textAreaInput uiOutput selectInput checkboxInput br p hr actionButton radioButtons renderUI tagList selectizeInput dateInput observeEvent updateTextInput runGadget stopApp dialogViewer incProgress withProgress tags icon mainPanel img a updateCheckboxInput updateRadioButtons HTML h5 strong updateSelectInput updateTextAreaInput fileInput reactive updateSelectizeInput req
#' @importFrom shinyjs useShinyjs enable disable show hide
#' @importFrom shinyWidgets searchInput awesomeRadio updateAwesomeRadio
#' @importFrom dplyr select pull filter if_else
#' @importFrom certestyle colourpicker format2
#  certestyle for R Markdown:
#' @importFrom certestyle rmarkdown_author rmarkdown_date rmarkdown_template rmarkdown_logo
#' @importFrom rstudioapi initializeProject openProject navigateToFile showDialog showQuestion
#' @importFrom Microsoft365R ms_plan ms_team ms_drive_item
#' @rdname planner_add
#' @export
project_consult_add <- function(planner = connect_planner(),
                                teams = if (in_positron()) connect_teams() else NULL,
                                channel = if (in_positron()) teams_projects_channel() else NULL,
                                outlook = connect_outlook()) {
  
  if (!inherits(planner, "ms_plan")) {
    stop("Maak eerst verbinding met Planner via connect_planner().")
  }
  if (!inherits(teams, "ms_team") || !inherits(channel, "ms_drive_item")) {
    teams <- NULL
    channel <- NULL
  }
  
  # ui ----
  ui <- fluidPage(
    useShinyjs(),
    tags$script(HTML('$(document).ready(function() {
                        var textbox = document.getElementById("title");
                        textbox.focus();
                
                        $(document).keydown(function(event) {
                          // Add event listener for Ctrl + Enter
                          if (event.ctrlKey && event.key === "Enter") {
                            $("#create").click();
                          }
                          // Add event listener for Esc key
                          if (event.key === "Escape") {
                            $("#cancel").click();
                          }
                        });
    });')),
    tags$script(HTML("Shiny.addCustomMessageHandler('set-textarea-rows', function(message) {
        var el = document.getElementById(message.id);
        if (el) {
          el.rows = message.rows;
        }
      });
    ")),
    uiOutput("custom_css"),
    tags$style(paste0("* { font-family: Source Sans Pro, Helvetica Neue, Arial; }
                      .container-fluid { margin-top: 15px; }
                      .form-group { margin-bottom: 5px; }
                      .well .form-group { margin-bottom: 14px; }
                      h5 { font-size: 12px; }
                      .type_select { text-align: center; }
                      label[for=title] { font-size: 16px; }",
                      '.awesome-radio input[type="radio"]:focus+label::before, .awesome-checkbox input[type="checkbox"]:focus+label::before { outline: none; }',
                      "#files_teams_or_local .shiny-options-group .awesome-radio { margin-left: 0px; }",
                      "#file_upload_progress, input.form-control[placeholder=\"No file selected\"] { display: none; }",
                      "label.input-group-btn.input-group-prepend { width: 0px; }",
                      ".mails-div { display: flex; align-items: flex-start; }")),
    
    sidebarLayout(
      sidebarPanel(
        div(class = "type_select",
            awesomeRadio("type", NULL, choices = c("Project", "Consult"), selected = "Project", inline = TRUE, width = "100%")
        ),
        textInput("title", "Titel", placeholder = ""),
        uiOutput("requested_by_ui"),
        textAreaInput("description", "Notities", cols = 1, rows = 2, resize = "vertical"),
        textAreaInput("checklist", "Controlelijst", cols = 1, rows = 3, resize = "vertical", placeholder = "(1 item per regel)"),
        selectInput("priority",
                    label = HTML(paste("Prioriteit", ifelse(as.integer(format(Sys.Date(), "%u")) %in% c(6:7), " <i style='font-weight:normal !important;'>(standaard 'Dringend' in het weekend)</i>", ""))),
                    choices = c("Laag", "Gemiddeld", "Belangrijk", "Dringend"), 
                    selected = ifelse(as.integer(format(Sys.Date(), "%u")) %in% c(6:7),
                                      "Dringend", # default if project is created on weekend day
                                      "Gemiddeld")),
        dateInput("duedate",
                  label = HTML("Einddatum <i style='font-weight:normal !important;'>(standaard volgende week donderdag)</i>"),
                  value = Sys.Date() - as.numeric(format(Sys.Date(), format = "%u")) + 11,
                  min = Sys.Date(),
                  format = "DD d MM yyyy",
                  language = "nl",
                  startview = "month",
                  weekstart = 1), # Monday
        br(),
        div(class = "mails-div",
            actionButton("search_mail", "Info uit e-mail ophalen...", width = "90%", icon = icon("envelope")),
            fileInput("file_upload", label = NULL, width = "86px"),
        ),
        uiOutput("mail_list"),
        actionButton("create", "Aanmaken (Ctrl+Ent)", width = "49%", icon = icon("check"), class = "btn-success"),
        actionButton("cancel", "Annuleren (Esc)", width = "49%", icon = icon("ban"), class = "btn-danger"),
        width = 6),
      
      mainPanel(
        img(src = img_rstudio(), height = "40px", style = "margin-bottom: 10px"),
        img(src = img_certe(), height = "80px", style = "margin-left: 58%; margin-top: -15px;"),
        awesomeRadio("filetype",
                     label = "Bestandstype",
                     status = "primary",
                     choices = c(".qmd (Quarto)" = ".qmd",
                                 ".R" = ".R",
                                 ".Rmd (R Markdown)" = ".Rmd"),
                     selected = ".qmd",
                     inline = TRUE,
                     width = "100%"),
        hr(),
        
        # PLANNER
        img(src = img_planner(), height = "40px", style = "margin-bottom: 10px"),
        uiOutput("planner_settings"),
        hr(),
        
        # TEAMS
        img(src = img_teams(), height = "40px", style = "margin-bottom: 10px"),
        if (!is.null(teams) && !is.null(channel) && !is.null(planner)) { # teams_* project functions rely on planner, so require that too
          awesomeRadio("files_teams_or_local",
                       label = "Projectmap en analysebestand:",
                       choices =  stats::setNames(c("teams",
                                                    "local"),
                                                  c(paste0("(Teams) In '", get_azure_property(teams, "displayName"), "/", get_azure_property(channel, "name"),
                                                           "' aanmaken"),
                                                    paste0("(OneDrive) In map '", shorten_folder(ifelse(suppressWarnings(get_projects_path()) == "",
                                                                                                        getwd(),
                                                                                                        get_projects_path())),
                                                           "' aanmaken"))),
                       selected = "local",
                       inline = TRUE,
                       width = "100%")
        } else {
          uiOutput("teamconnect")
        },
        # br(),
        uiOutput("consult_inform"),
        width = 6
      )
    )
  )
  
  # server ----
  server <- function(input, output, session) {
    
    output$custom_css <- renderUI({
      colour <- ifelse(input$type == "Consult", "certegroen", "certeblauw")
      padding_desc <- ifelse(input$type == "Consult", "padding-bottom: 0;", "")
      
      tags$style(paste0(".well { background-color: ", colourpicker(paste0(colour, 5)), "; }
                      #description { ", padding_desc, " }
                      .well label { color: ", colourpicker(colour), "; }
                      .h2, h2 { color: ", colourpicker(colour), "; }
                      a { color: ", colourpicker(colour), "; }
                      .form-control:focus, .selectize-input.focus { border-color: ", colourpicker(colour), "; box-shadow: none; }
                      certeblauw, .certeblauw { color: ", colourpicker(colour), "; }
                      certeroze, .certeroze { color: ", colourpicker("certeroze"), "; }
                      .btn-default.focus, .btn-default:focus, .btn-default.active, .btn-default:active { background-color: ", colourpicker(paste0(colour, 5)), "; border-color: ", colourpicker(colour), "; }
                      #create { background-color: ", colourpicker(colour), "; border-color: ", colourpicker(colour), "; }
                      #create:hover { background-color: ", colourpicker(paste0(colour, 0)), "; border-color: ", colourpicker(colour), "; }
                      #cancel { background-color: white; color: ", colourpicker(colour), "; border-color: ", colourpicker(colour), ";}
                      #cancel:hover { color: ", colourpicker("certeroze"), "; border-color: ", colourpicker("certeroze"), ";}
                      .multi .selectize-input .item, .selectize-dropdown .active { background-color: ", colourpicker(paste0(colour, 5)), " !important; }
                      .selectize-input .item.active { color: white; background-color: ", colourpicker(colour), " !important; }",
                        '.checkbox-primary input[type="checkbox"]:checked+label::before, .checkbox-primary input[type="radio"]:checked+label::before { background-color: ', colourpicker(colour), "; border-color: ", colourpicker(colour), " ;}",
                        ".datepicker .active { color: white !important; background-color: ", colourpicker(colour), " !important; }",
                        '.radio-primary input[type="radio"]:checked+label::before { border-color: ', colourpicker(colour), ";}",
                        '.radio-primary input[type="radio"]:checked+label::after { background-color: ', colourpicker(colour), ";}"))
      
    })
    
    is_consult <- reactive(input$type == "Consult")
    
    observe({
      if (is_consult()) {
        updateSelectInput("planner_bucket", selected = read_secret("planner.default.bucket.consult"), session = session)
        current_labels <- input$planner_categories[input$planner_categories != "Project"]
        updateSelectizeInput("planner_categories", selected = unique(c("Consult", current_labels)), session = session)
        session$sendCustomMessage("set-textarea-rows", list(id = "description", rows = 8))
        updateTextAreaInput("description", label = "Vraag / verzoek", session = session)
        updateTextAreaInput("checklist", value = "", session = session)
        updateSelectizeInput("requested_by", label = "Consult aan", session = session)
        hide("checklist")
        updateAwesomeRadio("filetype", selected = ".R", session = session)
        disable("filetype")
        output$consult_inform <- renderUI(p("Voor een consult wordt het R-bestand opgeslagen in projectenmap/Consulten/."))
      } else {
        # project
        updateSelectInput("planner_bucket", selected = read_secret("planner.default.bucket"), session = session)
        current_labels <- input$planner_categories[input$planner_categories != "Consult"]
        updateSelectizeInput("planner_categories", selected = unique(c("Project", current_labels)), session = session)
        session$sendCustomMessage("set-textarea-rows", list(id = "description", rows = 2))
        updateTextAreaInput("description", label = "Notities", session = session)
        show("checklist")
        updateSelectizeInput("requested_by", label = "Aangevraagd door", session = session)
        updateAwesomeRadio("filetype", selected = ".qmd", session = session)
        enable("filetype")
        output$consult_inform <- renderUI(NULL)
      }
    })
    
    output$teamconnect <- renderUI({
      tagList(
        p(strong("Niet verbonden met Teams."), "Bestanden worden aangemaakt in de map:",
          ifelse(suppressWarnings(get_projects_path()) == "",
                 getwd(),
                 get_projects_path())),
        actionButton("connect_to_teams", "Klik om te verbinden met Teams")  
      )
    })
    
    observeEvent(input$connect_to_teams, {
      if (is.null(teams)) {
        teams <<- tryCatch(connect_teams(), error = function(e) NULL)
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
                                                    paste0("In map '", shorten_folder(ifelse(suppressWarnings(get_projects_path()) == "",
                                                                                             getwd(),
                                                                                             get_projects_path())),
                                                           "' aanmaken"))),
                       selected = "local",
                       inline = TRUE,
                       width = "100%")
        })
      }
    })
    
    output$requested_by_ui <- renderUI({
      # retrieve user list with names and job titles
      users <- get_user()
      if (!is.null(users) && !identical(users, "")) {
        suppressWarnings(tagList(
          selectizeInput("requested_by",
                         label = "Aangevraagd door",
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
    
    output$planner_settings <- renderUI({
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
                         closeAfterSelect = FALSE)),
        actionButton("add_doelmatig", "Label Doelmatigheid toevoegen", width = "100%")
      )
    })
    
    observeEvent(input$add_doelmatig, {
      updateSelectizeInput("planner_categories", selected = unique(c(input$planner_categories, "Doelmatigheid")), session = session)
    })
    
    # Mail import ----
    
    output$mail_list <- renderUI(tagList())
    
    observeEvent(input$search_mail, {
      pkg_env$mails <- tryCatch(
        connect_outlook()$get_inbox()$list_emails(n = 50),
        error = function(e) {
          showDialog(title = "! Fout bij ophalen van mails", message = e$message)
          NULL
        })
      if (is.null(pkg_env$mails)) {
        return(tagList())
      }
      mails <- pkg_env$mails
      mail_ids <- vapply(FUN.VALUE = character(1), mails, function(mail) mail$properties$id)
      mail_names <- vapply(FUN.VALUE = character(1),
                           mails, function(mail) paste0(
                             paste(unlist(mail$properties$sender), collapse = ", "),
                             " (", format2(as.POSIXct(gsub("T", " ", mail$properties$sentDateTime), tz = "Europe/Amsterdam"), "d mmm H:MM"), "u):\n ",
                             mail$properties$subject
                           ))
      
      output$mail_list <- renderUI({
        tagList(
          br(),
          selectInput("mail",
                      label = "Selecteer e-mail",
                      choices = c("", stats::setNames(mail_ids, mail_names)),
                      selected = FALSE,
                      width = "100%")
        )
      })
    })
    
    observeEvent(input$mail, {
      ## Read from selected mail object (when using select input) ----
      mails <- pkg_env$mails
      mail_ids <- vapply(FUN.VALUE = character(1), mails, function(mail) mail$properties$id)
      mail <- mails[mail_ids == input$mail]
      if (length(mail) > 0) {
        mail <- mail[[1]]
        
        title <- trimws(gsub("^.*(RE|FWD?|Antw?d?): ", "", mail$properties$subject, ignore.case = TRUE))
        title <- gsub("^([a-z])", "\\U\\1", title, perl = TRUE)
        updateTextInput(session = session, inputId = "title", value = title)
        
        users <- get_user()
        from <- mail$properties$sender$emailAddress$name
        if (identical(users, "")) {
          updateTextInput(session = session, inputId = "requested_by", value = from)
        } else if (!from %in% users) {
          suppressWarnings(
            updateSelectizeInput(session = session, inputId = "requested_by", selected = from, choices = c(from, users))
          )
        } else {
          updateSelectizeInput(session = session, inputId = "requested_by", selected = from)
        }
        
        mail_txt <- mail$properties$body$content
        if (identical(mail$properties$body$contentType, "html")) {
          mail_txt <- mail_txt |> rvest::read_html() |> rvest::html_text2()
        }
        # remove emojis and greetings etc
        mail_txt <- clean_text(mail_txt)
        updateTextAreaInput(session = session, inputId = "description", value = mail_txt)
      }
    })
    
    observeEvent(input$file_upload, {
      ## Read EML file (created when using drag/drop) ----
      req(input$file_upload)
      pkg_env$internetMessageId <- NULL
      
      if (input$file_upload$datapath %like% "[.](msg|eml)$") {
        
        if (input$file_upload$datapath %like% "[.]msg$") {
          # MSG file
          if (!"msgxtractr" %in% rownames(utils::installed.packages())) {
            showDialog(title = "Missing Package",
                       message = "Dropping msg files requires the 'msgxtractr' package.",
                       url = "https://github.com/certe-medical-epidemiology/msgxtractr")
            return(invisible())
          }
          msg <- msgxtractr::read_msg(input$file_upload$datapath)
          
          subject <- msg$subject
          from <- msg$sender$sender_name
          body <- msg$body$text
          body <- paste0(body[nchar(body) != 2], collapse = " ")
          body <- gsub("\r\n", "\n", body)
          sent_by_us <- from == read_secret("department.name") || certemail::get_certe_name() %like% from
          pkg_env$internetMessageId <- msg$headers$`Message-ID`
          
        } else if (input$file_upload$datapath %like% "[.]eml$") {
          # EML file
          msg <- readLines(input$file_upload$datapath)
          txt_body <- which(msg %like% "^--_000")
          subject <- trimws(gsub("^Subject: ", "", msg[msg %like% "^Subject: "]))
          from <- trimws(gsub('"', "", gsub("^From: ", "", msg[msg %like% "^From: "])))
          from <- gsub(" <.*>", "", from)
          sent_by_us <- from == read_secret("department.name")
          if (sent_by_us) {
            # sent by us, so get To field, and take upper part as our response, lower part as their question/request
            from <- trimws(gsub('"', "", gsub("^To: ", "", msg[msg %like% "^To: "])))
            from <- gsub(" <.*>", "", from)
          }
          body <- tryCatch(msg[seq(txt_body[1] + 1, txt_body[2] - 1)], error = function(e) "")
          body <- body[body %unlike% "^Content-" & body != ""]
          body[nchar(body) == 76 & substr(body, 76, 76) == "="] <- gsub("=$", "####", body[nchar(body) == 76 & substr(body, 76, 76) == "="])
          body <- paste(body, collapse = "\n")
          body <- gsub("####\n", "", body)
          # translate accents
          body <- decode_quoted_printable_encoding(body)
        }
        
        # for both MSG and EML
        if (sent_by_us) {
          from <- gsub(".*\n(Van|From): (.*?)\n.*", "\\2", body)
          from <- gsub(" <.*>", "", from)
          our_txt <- gsub("(.*)\n(Van|From): .*", "\\1", body)
          our_txt <- clean_text(our_txt)
          updateTextAreaInput(session = session, inputId = "description2", value = our_txt)
          body <- gsub(".*\n(Onderwerp|Subject): .*?(\n)+(.*)", "\\3", body)
        }
        
        # remove emojis and greetings etc
        body <- clean_text(body)
        
      } else {
        # Other file - don't support this
        showDialog(title = "Invalid File",
                   message = "Dropping email files must be of type MSG or EML.")
        return(invisible())
      }
      
      # Update the fields
      title <- trimws(gsub("^.*(RE|FWD?|Antw?d?): ", "", subject, ignore.case = TRUE))
      title <- gsub("^([a-z])", "\\U\\1", title, perl = TRUE)
      updateTextInput(session = session, inputId = "title", value = title)
      users <- get_user()
      if (identical(users, "")) {
        updateTextInput(session = session, inputId = "requested_by", value = from)
      } else if (!from %in% users) {
        suppressWarnings(
          updateSelectizeInput(session = session, inputId = "requested_by", selected = from, choices = c(from, users))
        )
      } else {
        updateSelectizeInput(session = session, inputId = "requested_by", selected = from)
      }
      
      updateTextAreaInput(session = session, inputId = "description", value = body)
    })
    
    # SAVE ----
    observeEvent(input$create, {
      
      empty_field <- function(field, value) {
        if (is.null(value) || length(value) == 0 || all(value == "", na.rm = TRUE)) {
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
      
      valid_title <- trimws(input$title)
      valid_title <- gsub("^([a-z])", "\\U\\1", valid_title, perl = TRUE)
      if (empty_field("Titel", valid_title) || invalid_title(valid_title)) return(invisible())
      requested_by <- input$requested_by
      if (empty_field("Aanvrager(s)", requested_by)) return(invisible())
      filetype <- input$filetype
      if (empty_field("Bestandstype", filetype)) return(invisible())
      duedate <- as.character(input$duedate)
      if (empty_field("Einddatum", duedate)) return(invisible())
      description <- trimws(input$description)
      if (length(description) == 0) {
        description <- ""
      } else {
        description <- gsub("[.][.]", ".", paste0(description, "."))
      }
      if (description == ".") {
        description <- ""
      }
      
      checklist <- input$checklist
      if (is.null(checklist) || length(checklist) == 0) {
        checklist <- ""
      }
      
      withProgress(message = "Aanmaken...", value = 0, {
        progress_items <- identical(input$files_teams_or_local, "teams") + 3 # + 3 for Planner and for creating folders
        output_task_id <- NULL
        title_with_output_task_id <- valid_title
        # Planner ----
        incProgress(1 / progress_items, detail = "MS Planner: taak aanmaken")
        if (is_consult()) {
          new_task <- planner_task_create(account = planner,
                                          title = valid_title,
                                          bucket_name = input$planner_bucket,
                                          assigned = input$planner_members,
                                          requested_by = requested_by,
                                          priority = input$priority,
                                          duedate = as.Date(duedate),
                                          categories = input$planner_categories, 
                                          project_number = NULL,
                                          checklist_items = NULL,
                                          description = description)
        } else {
          new_task <- planner_task_create(account = planner,
                                          title = valid_title,
                                          bucket_name = input$planner_bucket,
                                          assigned = input$planner_members,
                                          requested_by = requested_by,
                                          priority = input$priority,
                                          duedate = as.Date(duedate),
                                          categories = input$planner_categories, 
                                          checklist_items = checklist |> strsplit("\n") |> unlist(),
                                          description = description)
        }
        output_task_id <- new_task$id
        title_with_output_task_id <- new_task$title
        
        # Teams ----
        if (identical(input$files_teams_or_local, "teams")) {
          incProgress(1 / progress_items, detail = "MS Teams: map aanmaken")
          teams_new_project(task = title_with_output_task_id, channel = channel, planner = planner)
          projects_path <- tempdir()
        } else {
          # saved to local
          incProgress(1 / progress_items, detail = "Map aanmaken")
          projects_path <- ifelse(get_projects_path() == "",
                                  getwd(),
                                  get_projects_path())
        }
        if (is_consult()) {
          fullpath <- file.path(projects_path, "Consulten")
        } else {
          fullpath <- file.path(projects_path, title_with_output_task_id)
        }
        desc <- unlist(strsplit(description, "\n", fixed = TRUE))
        
        header_text <- c(paste0("# Titel:            ", valid_title),
                         if_else(!is.na(desc[1]), 
                                 paste0("# Omschrijving:     ", desc[1]),
                                 NA_character_),
                         if_else(length(desc) > 1,
                                 paste0("#                   ", desc[2:length(desc)], collapse = "\n"),
                                 NA_character_),
                         if_else(!is.null(output_task_id) & !is_consult(),
                                 paste0("# Projectnummer:    p", output_task_id),
                                 NA_character_),
                         if_else(!is.null(output_task_id) & is_consult(),
                                 paste0("# Consultnummer:    c", output_task_id),
                                 NA_character_),
                         if_else(!arg_is_empty(requested_by),
                                 if_else(is_consult(),
                                         paste0("# Consult aan:      ", requested_by[1]),
                                         paste0("# Aangevraagd door: ", requested_by[1])),
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
          if (filetype == ".Rmd") {
            # R Markdown
            filecontent <- c("---",
                             paste0('title: "', valid_title, '" # laat leeg voor geen voorblad bij PDF'),
                             'subtitle: ""',
                             'subtitle2: ""',
                             'author: "`r certestyle::rmarkdown_author()`" # vervang evt. door certestyle::rmarkdown_department()',
                             'date: "`r certestyle::rmarkdown_date()`"',
                             'identifier: "`r certeprojects::project_identifier()`"',
                             "toc: true",
                             "toc_depth: 2",
                             "fig_width: 6.5 # in inch",
                             "fig_height: 5  # in inch",
                             "output:",
                             "  # word_document:",
                             '  #   reference_docx: !expr certestyle::rmarkdown_template("word")',
                             "  pdf_document:",
                             '    latex_engine: "xelatex"',
                             "    df_print: !expr certestyle::rmarkdown_table",
                             '    template: !expr certestyle::rmarkdown_template("latex")',
                             'logofront: "`r certestyle::rmarkdown_logo(\'front\')`"   # max 16x7 cm',
                             'logofooter: "`r certestyle::rmarkdown_logo(\'footer\')`" # max 16x0.7 cm',
                             "editor: source",
                             "---",
                             "",
                             "```{r Setup, include = FALSE, message = FALSE}",
                             header_text,
                             "",
                             "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,",
                             '                      results = "asis", comment = NA, dpi = 600, fig.showtext = TRUE)')
          } else if (filetype == ".qmd") {
            # Quarto
            # some elements are not set but inherited from _quarto_yml
            filecontent <- c("---",
                             paste0('title: "', valid_title, '" # laat leeg voor geen voorblad bij PDF'),
                             'subtitle: ""',
                             'subtitle2: ""',
                             'author: "`r certestyle::rmarkdown_author()`" # vervang evt. door certestyle::rmarkdown_department()',
                             'date: "`r Sys.Date()`" # moet in YYYY-MM-DD',
                             'identifier: "`r certeprojects::project_identifier()`"',
                             "editor: source",
                             "---",
                             "",
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
                           paste0('if (!is.na(project_get_file(".*rds$", ', output_task_id, ")) & !data_download) {"),
                           paste0("  data_p", output_task_id, ' <- import_rds(project_get_file(".*rds$", ', output_task_id, "))"),
                           "} else {",
                           paste0("  data_p", output_task_id, " <- get_diver_data(date_range = c(start, stop),"),
                           paste0(strrep(" ", nchar(output_task_id)), "                           where = gl)"),
                           paste0("  # export_rds(data_p", output_task_id, ', "data_p', output_task_id, '", project_number = ', output_task_id, ') # wordt standaard versleuteld'),
                           "}",
                           "```",
                           "",
                           "# Inleiding",
                           "",
                           "```{r}",
                           "",
                           "```",
                           "")
        }
        if (filetype == ".R") {
          filecontent <- c(header_text,
                           "",
                           "library(certedata)",
                           paste0("data_", ifelse(is_consult(), "c", "p"), output_task_id, " <- get_diver_data(dates = c(start, stop),"),
                           paste0(strrep(" ", nchar(output_task_id)), "                         where = gl)"))
          if (!is_consult()) {
            filecontent <- c(filecontent,
                             paste0("# export_rds(data_p", output_task_id, ', "data_p', output_task_id, '.rds", project_number = ', output_task_id, ") # wordt standaard versleuteld"),
                             paste0("# data_p", output_task_id, ' <- import_rds(project_get_file(".*rds$", ', output_task_id, '))'),
                             "")
          } else {
            filecontent <- c(filecontent,
                             "# gebruik get_output_folder_consult() voor outputmap van consulten, bijvoorbeeld:",
                             "# file.path(get_output_folder_consult(), \"bestand.xlsx\")",
                             "")
          }
        }
        filename <- paste0(fullpath,
                           ifelse(is_consult(),
                                  paste0("/c", output_task_id %||% "???", " - ", valid_title, " [", strsplit(requested_by[1], " ")[[1]][1], "]"),
                                  paste0("/Analyse",
                                         ifelse(!is.null(output_task_id),
                                                paste0(" p", output_task_id),
                                                ""))),
                           filetype)
        incProgress(1 / progress_items, detail = "R-bestand schrijven")
        writeLines(text = paste(filecontent[!is.na(filecontent)], collapse = "\n"),
                   con = file.path(filename))
        if (identical(input$files_teams_or_local, "teams")) {
          teams_upload_project_file(files = file.path(filename), task = title_with_output_task_id, channel = channel, planner = planner)
          # unlink(file.path(filename), force = TRUE)
          unlink(fullpath, recursive = TRUE, force = TRUE)
        }
        
        Sys.sleep(0.1)
      })
      
      if (is_consult()) {
        message("Consult c", output_task_id, " aangemaakt.")
        if (!is.null(pkg_env$internetMessageId)) {
          # label mail as consult
          flag_mail <- outlook$list_emails(filter = paste0("internetMessageId eq '", pkg_env$internetMessageId, "'"))[[1]]
          flag_mail$update(categories = c("Consult aangemaakt", "MedEpi"))
        }
      } else {
        message("Project p", output_task_id, " aangemaakt.")  
      }

      if (identical(input$files_teams_or_local, "teams")) {
        teams_browse_project(task = title_with_output_task_id, channel = channel, planner = planner)
      } else {
        # open file
        navigateToFile(filename)
      }
      
      stopApp()
    })
    
    observeEvent(input$cancel, {
      stopApp()
    })
  }
  
  viewer <- dialogViewer(dialogName = paste("Nieuw project/consult -",
                                            ifelse(!is.null(teams),
                                                   get_azure_property(teams, "displayName"),
                                                   read_secret("department.name"))),
                         width = 900,
                         height = 720)
  
  suppressMessages(
    runGadget(app = ui,
              server = server,
              viewer = viewer,
              stopOnCancel = FALSE))
}

clean_text <- function(x) {
  x <- gsub("( ?\n ?)+", "\n", x)
  x <- gsub("[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF\U0001F700-\U0001F77F\U0001F780-\U0001F7FF\U0001F800-\U0001F8FF\U0001F900-\U0001F9FF\U0001FA00-\U0001FA6F\U0001FA70-\U0001FAFF\U00002702-\U000027B0\U0001F1E6-\U0001F1FF]", "(emoji)", x, perl = TRUE)
  x <- gsub("^U ontvangt niet vaak e-mail.*?(\n)+", "", x)
  x <- gsub(" ?(\n|[^a-zA-Z0-9.,!?\\(\\)]){3,99} ?", "\n", x)
  x <- gsub("\nOorspronkelijk bericht\n.*", "", x)
  x <- gsub("\nVan:.*Verzonden:.*", "", x)
  x <- gsub("(\n)+", "\n", x)
  x <- gsub("\nGroet(en)?,?.*", "", x)
  x <- gsub("\nMet vriendelijke groet,.*", "", x)
  x <- gsub("^(Hoi|Ha|Beste|Dag) .*?\n", "", x)
  x
}

decode_quoted_printable_encoding <- function(text) {
  # Decoding quoted-printable accents
  decoded_text <- text
  decoded_text <- gsub("=E0", "\u00E0", decoded_text)  # a grave
  decoded_text <- gsub("=E1", "\u00E1", decoded_text)  # a aigu
  decoded_text <- gsub("=E2", "\u00E2", decoded_text)  # a circumflex
  decoded_text <- gsub("=E3", "\u00E3", decoded_text)  # a tilde
  decoded_text <- gsub("=E4", "\u00E4", decoded_text)  # a diaeresis
  decoded_text <- gsub("=E5", "\u00E5", decoded_text)  # a ring
  
  decoded_text <- gsub("=E8", "\u00E8", decoded_text)  # e grave
  decoded_text <- gsub("=E9", "\u00E9", decoded_text)  # e aigu
  decoded_text <- gsub("=EA", "\u00EA", decoded_text)  # e circumflex
  decoded_text <- gsub("=EB", "\u00EB", decoded_text)  # e diaeresis
  
  decoded_text <- gsub("=EC", "\u00EC", decoded_text)  # i grave
  decoded_text <- gsub("=ED", "\u00ED", decoded_text)  # i aigu
  decoded_text <- gsub("=EE", "\u00EE", decoded_text)  # i circumflex
  decoded_text <- gsub("=EF", "\u00EF", decoded_text)  # i diaeresis
  
  decoded_text <- gsub("=F2", "\u00F2", decoded_text)  # o grave
  decoded_text <- gsub("=F3", "\u00F3", decoded_text)  # o aigu
  decoded_text <- gsub("=F4", "\u00F4", decoded_text)  # o circumflex
  decoded_text <- gsub("=F5", "\u00F5", decoded_text)  # o tilde
  decoded_text <- gsub("=F6", "\u00F6", decoded_text)  # o diaeresis
  
  decoded_text <- gsub("=F9", "\u00F9", decoded_text)  # u grave
  decoded_text <- gsub("=FA", "\u00FA", decoded_text)  # u aigu
  decoded_text <- gsub("=FB", "\u00FB", decoded_text)  # u circumflex
  decoded_text <- gsub("=FC", "\u00FC", decoded_text)  # u diaeresis
  
  # Additional common accented characters
  decoded_text <- gsub("=C0", "\u00C0", decoded_text)  # A grave
  decoded_text <- gsub("=C1", "\u00C1", decoded_text)  # A aigu
  decoded_text <- gsub("=C8", "\u00C8", decoded_text)  # E grave
  decoded_text <- gsub("=C9", "\u00C9", decoded_text)  # E aigu
  decoded_text <- gsub("=CC", "\u00CC", decoded_text)  # I grave
  decoded_text <- gsub("=CD", "\u00CD", decoded_text)  # I aigu
  decoded_text <- gsub("=D2", "\u00D2", decoded_text)  # O grave
  decoded_text <- gsub("=D3", "\u00D3", decoded_text)  # O aigu
  decoded_text <- gsub("=D9", "\u00D9", decoded_text)  # U grave
  decoded_text <- gsub("=DA", "\u00DA", decoded_text)  # U aigu
  
  # Common special characters
  decoded_text <- gsub("=E7", "\u00E7", decoded_text)  # c cedilla
  decoded_text <- gsub("=F1", "\u00F1", decoded_text)  # n tilde
  decoded_text <- gsub("=C7", "\u00C7", decoded_text)  # C cedilla
  decoded_text <- gsub("=D1", "\u00D1", decoded_text)  # N tilde
  decoded_text
}


#' @importFrom miniUI miniPage miniContentPanel
#' @importFrom shiny actionButton observe runApp paneViewer stopApp reactiveValuesToList
#' @importFrom certestyle colourpicker
shiny_item_picker <- function(values, oversized = character(0), title = "", subtitle = "") {
  if (length(oversized) == length(values)) {
    # they are all oversized, so turn that off
    oversized <- character(0)
  }
  ui <- miniPage(
    tags$style(paste0(
      "body {
        background-color: ", colourpicker("certeblauw6"), ";
     }
     * {
        font-family: Source Sans Pro, Helvetica Neue, Arial;
        text-align: center;
     }
     h4 {
        color: ", colourpicker("certeblauw"), ";
     }
     h5 {
        color: ", colourpicker("certeblauw"), ";
        font-weight: normal;
     }
     .oversized {
        height: 60px !important;
        font-weight: bold;
     }
     .pick_btn {
        font-size: 14px;
        background-color: white;
        margin-bottom: 5px;
        color: ", colourpicker("certeblauw"), ";
     }
     .pick_btn:hover {
        color: ", colourpicker("certeblauw"), ";
        border-color: ", colourpicker("certeblauw"), ";
        background-color: white;
     }")),
    miniContentPanel(
      if (title != "") {
        h4(title)
      },
      if (subtitle != "") {
        h5(subtitle)
      },
      lapply(seq_along(values), function (i) {
        btn_class <- "pick_btn"
        if (i %in% oversized) {
          btn_class <- c("oversized", btn_class)
        }
        if (!is.null(names(values)) && names(values)[i] != "") {
          actionButton(inputId = names(values)[i], label = values[i], width = "100%", class = btn_class)
        } else {
          actionButton(inputId = values[i], label = values[i], width = "100%", class = btn_class)
        }
      })
    )
  )
  
  server <- function(input, output, session) {
    observe({
      contents <- unlist(reactiveValuesToList(input))
      if (is.null(contents) || all(contents == 0)) return(invisible())
      button <- names(contents)[which(contents == 1)]
      stopApp(returnValue = button)
    })
  }
  
  suppressMessages(
    runApp(list(ui = ui, server = server), launch.browser = paneViewer())
  )
}

#' Git Compare Files
#' 
#' This compares two text files
#' @param files character vector, allowed to be named
#' @param original_file original file path to restore file to
#' @importFrom shiny fluidPage tags h4 HTML checkboxInput div fluidRow column selectInput h4 uiOutput renderUI p downloadButton actionButton icon stopApp downloadHandler
#' @importFrom shinyWidgets awesomeCheckbox
#' @importFrom rstudioapi showQuestion showDialog
#' @export
git_compare <- function(files = NULL, original_file = NULL) {
  
  if (is.null(names(files))) {
    names(files) <- basename(files)
  }
  not_real <- !file.exists(files)
  if (any(not_real)) {
    stop("These files do not exist: ", toString(files[not_real]), call. = FALSE)
  }
  
  ui <- fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/r.min.js"),
      tags$script(HTML("hljs.highlightAll();"))
    ),
    tags$style(paste0(
      "* {
       font-family: Source Sans Pro, Helvetica Neue, Arial;
     }
     .checkbox-primary input[type='checkbox']:checked+label::before {
       background-color: ", colourpicker("certeblauw"), ";
       border-color: ", colourpicker('certeblauw'), ";
     },
     .awesome-checkbox *,
     .awesome-checkbox input[type=\"checkbox\"]:focus+label::before {
       outline: none !important;
     }                 
     h4 {
        color: ", colourpicker("certeblauw"), ";
     }
     p {
       color: ", colourpicker("certeroze"), ";
       text-align: center;
       margin-top: 20px;
     }
     .deleted, .deleted * {
       background-color: ", colourpicker("certeroze2"), " !important;
       color: white !important;
     }
     .added, .added * {
       background-color: ", colourpicker("certegroen2"), " !important;
       color: white !important;
     }
     .line {
       color: #aaa;
     }
     .contents {
       overflow-x: scroll;
       padding-left: 0;
     }
     .file-contents {
       border: 1px solid #eee;
       padding: 10px;
       margin: 1%;
       width: 48%;
       padding-right: 1%;
     }
     .file-contents pre, .file-contents code {
       padding: 0 !important;
       margin: 0 !important;
       border: none;
       background: white;
       display: inline-flex;
     }
     .file-contents * {
       font-family: Fira Code, Monospace;
       text-align: left;
       white-space: nowrap;
       font-size: 12px;
     }")),
    h4(paste0("Vergelijk versies van ", ifelse(is.null(original_file), "bestanden", paste0("'", basename(original_file), "'")))),
    awesomeCheckbox("syntax_r", "Opmaken als R-syntax", value = is.null(original_file) || original_file %like% "[.]R$"),
    awesomeCheckbox("viewboth", "Inhoud aan beide kanten weergeven"),
    fluidRow(
      column(width = 6,
             selectInput("file1", label = NULL, choices = files, selected = ifelse(length(files) > 1, files[2], files[1]), width = "100%"),
             downloadButton("download_file1", label = "Downloaden"),
             if (!is.null(original_file) && file.exists(original_file)) {
               actionButton("restore_file1", label = "Herstellen", icon = icon("rotate"))
             }
      ),
      column(width = 6,
             selectInput("file2", label = NULL, choices = files, selected = files[1], width = "100%"),
             downloadButton("download_file2", label = "Downloaden"),
             if (!is.null(original_file) && file.exists(original_file)) {
               actionButton("restore_file2", label = "Herstellen", icon = icon("rotate"))
             }
      )
    ),
    uiOutput("git_diff")
  )
  
  server <- function(input, output, session) {
    
    restore_file <- function(source_file, new_file) {
      breaks <- ifelse(in_positron(), "<br>", "\n")
      proceed <- showQuestion(title = paste0("Herstellen van versie ", basename(source_file)),
                              message = paste0("Hiermee wordt het volgende bestand overschreven door versie ", basename(source_file), ":",
                                               breaks, breaks, new_file,
                                               breaks, breaks, "Dit wordt als een nieuw versienummer opgeslagen."))

      if (proceed == TRUE) {
        success <- file.copy(source_file, new_file, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
        if (success == TRUE) {
          showDialog(title = paste0("Herstellen van versie ", basename(input$file2)),
                     message = "Bestand hersteld. Dit venster wordt nu gesloten.")
          stopApp()
        } else {
          showDialog(title = paste0("Herstellen van versie ", basename(input$file2)),
                     message = "Bestand kon niet hersteld worden.")
        }
      }
    }
    
    output$download_file1 <- downloadHandler(
      filename = function() {
        if (!is.null(original_file)) {
          basename(original_file)
        } else {
          basename(input$file1)
        }
      },
      content = function(file) {
        file.copy(input$file1, file)
      },
      contentType = "text/plain"
    )
    observeEvent(input$restore_file1, {
      restore_file(input$file1, original_file)
      
    })
    
    output$download_file2 <- downloadHandler(
      filename = function() {
        if (!is.null(original_file)) {
          basename(original_file)
        } else {
          basename(input$file2)
        }
      },
      content = function(file) {
        file.copy(input$file2, file)
      },
      contentType = "text/plain"
    )
    observeEvent(input$restore_file2, {
      restore_file(input$file2, original_file)
    })
    
    output$git_diff <- renderUI({
      viewboth <- input$viewboth
      
      if (input$file1 == input$file2) {
        # identical select input
        lines <- suppressWarnings(paste0(" ", readLines(input$file1)))
      } else {
        diff_output <- suppressWarnings(
          system2(
            "git",
            c("diff", "--no-index", "--unified=99999", input$file1, input$file2),
            stdout = TRUE,
            stderr = TRUE
          )
        )
        if (length(diff_output) == 0) {
          # identical files
          lines <- suppressWarnings(paste0(" ", readLines(input$file1)))
        } else {
          lines <- diff_output[c((which(diff_output %like% "@@ ")[1] + 1):length(diff_output))]
          lines <- lines[lines %unlike% "No newline at end of file"]
        }
      }
      
      first_chars <- substr(lines, 1, 1)
      
      line_nr_left <- 0
      line_nr_right <- 0
      for (f in first_chars) {
        if (f == " ") {
          line_nr_left <- c(line_nr_left, max(line_nr_left, na.rm = TRUE) + 1)
          line_nr_right <- c(line_nr_right, max(line_nr_right, na.rm = TRUE) + 1)
        } else if (f == "-") {
          line_nr_left <- c(line_nr_left, max(line_nr_left, na.rm = TRUE) + 1)
          line_nr_right <- c(line_nr_right, NA_real_)
        } else if (f == "+") {
          line_nr_left <- c(line_nr_left, NA_real_)
          line_nr_right <- c(line_nr_right, max(line_nr_right, na.rm = TRUE) + 1)
        }
      }
      line_nr_left <- c(line_nr_left, max(line_nr_left, na.rm = TRUE) + 1)
      line_nr_right <- c(line_nr_right, max(line_nr_right, na.rm = TRUE) + 1)
      line_nr_left <- line_nr_left[-1]
      line_nr_right <- line_nr_right[-1]
      line_nr_left <- trimws(format(line_nr_left))
      line_nr_left[line_nr_left == "NA"] <- ""
      line_nr_right <- trimws(format(line_nr_right))
      line_nr_right[line_nr_right == "NA"] <- ""
      
      
      html <- lines
      html <- substr(lines, 2, nchar(lines))
      html <- gsub(" ", "&nbsp;", html, fixed = TRUE)
      
      if (isTRUE(input$syntax_r)) {
        html <- paste0("<pre><code class='language-r'>", html, "</code></pre>")
      }
      
      html[first_chars == "+"] <- paste0("<span class = 'added'>", html[first_chars == "+"], "</span>")
      html[first_chars == "-"] <- paste0("<span class = 'deleted'>", html[first_chars == "-"], "</span>")
      html[first_chars == " "] <- paste0("<span>", html[first_chars == " "], "</span>")
      html_left <- html
      html_right <- html
      if (viewboth == FALSE) {
        html_left[first_chars == "+"] <- ""
        html_right[first_chars == "-"] <- ""
      }
      
      line_nr_left[first_chars == "+"] <- paste0("<span class = 'added'>", line_nr_left[first_chars == "+"], "</span>")
      line_nr_left[first_chars == "-"] <- paste0("<span class = 'deleted'>", line_nr_left[first_chars == "-"], "</span>")
      line_nr_right[first_chars == "+"] <- paste0("<span class = 'added'>", line_nr_right[first_chars == "+"], "</span>")
      line_nr_right[first_chars == "-"] <- paste0("<span class = 'deleted'>", line_nr_right[first_chars == "-"], "</span>")
      
      fluidRow(
        if (length(files) == 1) {
          p("Dit is de enige versie.")
        } else if (identical(line_nr_left, line_nr_right) && identical(html_left, html_right)) {
          p("Deze versies zijn identiek.")
        },
        column(width = 6, class = "file-contents",
               fluidRow(
                 column(1, class = "line", HTML(paste(line_nr_left, collapse = "<br>"))),
                 column(11, class = "contents", HTML(paste(html_left, collapse = "<br>")))),
        ),
        column(width = 6, class = "file-contents",
               fluidRow(
                 column(1, class = "line", HTML(paste(line_nr_right, collapse = "<br>"))),
                 column(11, class = "contents", HTML(paste(html_right, collapse = "<br>")))),
        ),
        if (isTRUE(input$syntax_r)) {
          tags$script(HTML("setTimeout(function() { hljs.highlightAll(); }, 0);"))
        }
        
      )
    })
  }
  
  viewer <- dialogViewer(dialogName = "Versievergelijking",
                         width = 1400,
                         height = 860)
  
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
img_certe <- function() {
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAdcAAAMMCAYAAAAB4u0EAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAKrlSURBVHhe7b37035FeeY7/8f8Afu3XTVVUzVVs3+a2ZnKzmQmYcazolE8RsUIalQCosaIaDCKgCIH4SsnQU5mAEWFgEREFNEgYBRGBBElAkENIHGe/V4vT0O/93OttbrX6u7VvdZ1VX2KL++zzoe+1t19d/e/2TSmn377zs11R39i87/+5/Gihx996ebN0088tb1q0pB+89htmwe+/5IDPPLTC7e/SmsQ3pdSZcsTjz6+3Ws6sf2kAuVJal1y+XGbT3zqvy0Gq+bM1emRex/Y3HLSBfRBWDPfO++aLC/u0gUjteb64N1v2Pzud09sl5DWoF/ceQ99r1KTw6zYflKR+nhv+NohalCtcsZnXrE9s+fUrLk6wWRvO/1y+kCsCZnqNMFIrbmCX/3y77dLSGtRiY92RMipxfaTCpSzqfT9u26kBtUyP7n/+9uze07Nm6sTjAUG86VXn0gfjqUiU52uJx6/ixor+Nk/vnO7lLQW4X1i71pqECWnFNtHKlKZ60M/v5eaU8v88J5bt2d3UIsxVye0m9z3te8sul0W56Y21XRiVcI+aI+V1iW8X+zdSwlq3FIqZ8Sdoqz59a8f3a8+ZQbVKqje7tLizNUXvrYQ2bGHpUVwLqm/dqXuKmHHwz/++HZJaS0qldyU8gM5p7mm0NISmP726g9vz4xr0ebqhAcYWcattc2iihvHjGNXlJpHfVXCPr998mfbNaS1CB+y7L1MCd7tVMplriiHpmppCUyfvfAtm6d+25/suApz9eUbbY3tsze996zNXZdfrwi1kH55/5nUTC3qlrNO5U5uwvueSrlq6XANpghtksygWgVV24888uD27Lq1OnO1evxnD++30cJsS7fTwtzx4KJ9B2aq6LSs0M2GGWkX//rbx7ZrSmtRieSmVAmJudqJUU7hgx/bd6DJDQyVWTChpbWzdiUwWa3eXK3wsOChgeHiIYL5Tf16ddtwDyi2rwzf+fX4w1+mJtrFYw9duV1TWpPw3rL3OhXYfgqhbGHbL4Er31BuImCBUG26tHbWb3zz0v1zC5HMdYTw8LgvN4Z7uKS6hW42zES70KAS6xQ+uHM2IaXq81qijTgUXK9LPvVualCtMpTAZCVzlVYpNtxhCIh2pfUJERkzkVSk+CDHhz3b9hxc/OajqEG1Cqq2hxKYrGSu0iqF7jXMPIfQoBLrVc6cDCQjTRGi69zV16Fc8fJjNqeeehg1qVbB4BexkrlKq9OTv/nf1DhD0aAS6xR6GTAzSQGqUccIuRu19eU/88SXUYNqle/c/sXt1Y6TzFVancZGrQ5Fr+vV1OTGPmK639VoquC8d7+BGlSrxLaz+pK5SqvS1KjVoeh1ncrZrhlSNYzqX2QF50ywGstlR7yTGlSrjGln9SVzlValqVGrQ9HrepUreh2qGoaxl+6LH8OnT3oRNalWGdPO6kvmKq1GqaJWB7YnrU85o1dWNVxTslIXS6sOjunP2iWZq7QapYpaHSUH9MdXtAPzYSLJwoKRY9wyIcOzSeOVK3q1M+Wgiw6GSGTL1sLSqoMx8EUKyVylVSh11OpIHb0688RA50imYC9/DGg3wnbwJY7tYvvSdOWMXhGpQshOrrFt1bK06uBUH6YyV2kV+vk9H6DmOJWp0SvMDlFnCiONAV/nMNyf3P/9SUkba1au6BWmWns1sGNp1cH4AE0lmau0eI0djSmU2OgVhobItKYBzWHuKFgwobUUplzR63VHnUz/XhtLqw6e0u2GSeYqLVoYCzh2DOFYQqJXmBYiRcwDyV7smkBUC6NVRDusGvualmJJg0XgQzf1h6XMVVq0Yme+GUtX9Ipq32u/+kn6QrcAjl3ttN0qMSVdjVzwjjfR56VVUlYHO8lcpcUK869iJhtmhqmx0SsMqXQ7ak4QcSMbWdHsrtYWvX7hxccuauzg1NXBTjJXabH65f1nUiPMBaJXZBouyVQtqD5DApZM9jmtLXo95/1H0GejRXJUBzvJXKVFKlfXmy5+/L0Xb7549RvpC7xEZLIHtZbo9fOvfzt9HloFz3AuyVylRSp3EpPPLX/3/M3pZ/53+vIuHWeya9daotcl9WlNNVhEl2Su0uJUKonph7e9aHPx5/6YvrhrA22y6GK0Zi09er3oqPoz3WPInagnc5UWJSQxMSNMzbdvesFqo9U+0N681r6yS45el5bElGLs4CHJXKVFKfX4wYyrrvwf9IUVz7HWquKlRq9LGokJTRklcgVkrtJilHskJlQDn3e+qoFDQZvW2iYQWGL0esXLj6H3t1XQpayEZK7SIoSRmHL2ab3r1hepGngka4tiMZk5M6lWWVLXG4wqhdqFEpK5SotQzj6tyAZmL6oIB1HsWtpiMatNC7PZhLC0rjcYDxnnVcJgZa5S83ri8buoKaZAxpoOtHWtJaN4KdHrksYPRgTun1tug5W5Sk0r58D813/xefQlFdMokak5txC9Xnf0Jw4U5q2xpKgVmc7IeLbnmNNgZa5S03rsoSupMU5FGcF5QZedpY/uhHlZbWHeEkuKWpHtzM4R5DLYJs0VGYjoAAwwmwESJiz4u1tmbRmLa1GuIQ5lrGXAwBNLfzdveu9ZtECvnSVFrZ/8+PNp1OqDavzUqtpcnXliYml86aLNhl28ULA+toNqKWxXptuuclUH3/QVVQWXZOntsK2aKwyJ3a8WwchS7BwtqGlIqarMFWaHqLP0rCKYsxJmu9aRZVpUjupgJS/NR6m+hyXVarXwkoY5xEcCO8cuHrn3ge3dm67ZzRXRKSLTqVFpKlBVBYOX0darHNXBMtb5wQfuktRqQtOSBucPjVod6EL1+M8e3t7BaZrFXJHIAAODkbELUguIoJf4Rd26fn7PB6hBjkUDRNQDPrSXoFaj1iW1tSIhi53jEPgoQrb3VBU1V0SDeHnYhagZfATIZOtQ6hlvMKShjLUulmCwrUatS8oQxocCO8cQbjnpgu2dHK8i5tqqqVpksvPqt0/+jBrkWDDBucYKrpOWDfYXd95DC+zaUdR6kLsuv357R8cpq7kuxVQtqC5WpnF5pa4ObrXLDYYSdFnvrusZA8uUTg5MSasGi6iHFda1o6h1F3wojVUWc3VtqrUkKeUC5yiVUerq4FYSmFBbApOZ2nUMH7qodYEh157r4NOawSIZhhXStbOkmW+QkMXOcQxIcMJMR2OU3FzRZ62ll3cqaxqQfC6lngC99gQmvD/4cMtZO4JnFoaN55cdQ020lEWM0X5YIV07S5r5JjZDeIix7a/JzBUvK/qLspNdOkvvCD+3Uk+AXmM7K54hRGlzNDdgn9h3zTVNLeQ6IMOUFc61s6SoNbZfayhjRnBKYq74slx6FXAISnZKr1/98u+pQY7l2qv+J713c/GZcw/b/OMPv17FOLu1N+fU/gF739e+Qwvm2sG4u+x6t0jqqNUntv/rJHPFy7jWaLWLVpMwalTqCdC/d/ML6T2bg8+c80f77b44LsxFW5NqNVkcT82JhK12v8GMMex6twbOg51fKjCUZYxGmytGVlK0ypHBplHqCdBhaOx+lQRtvc5UfRCh1yaYLJ5ldh5zgTKnhijfqtVEpiUNddg3800qYqqHR5krMg7ZyYnnkMFOU+ohDmuYmxVV0uhby44PoB9vjcKH9FxJih//1O/v4/8NSVi1qdVEpiUNdYi2Y3aOqQmtHo4yV3wxttxvrjRrmBQ6h1LPeINRmNj9KQUSqJChzI7NB1XgOPcahXe/5Ef1qWcdtvn42b+3+cjp/34f/PuM8w7f++0P93+v7eMVXTZYQVwzlx3xzgPXvGWQ7czOMQeh1cPB5oq2DlUDx6Mkp3il7tN68efmyw5GtMqOqQtkRtesEs1Bp531vGdN1QKThfFiuVrerVZHZFpS95tUg0aEguS1IQWZKx5idkIiDBRIUphS92mda7AItK1++6YX0GMaAh8XNQtRbM7+sR876/+lxurzqXNfUE2CU4tVwpg8fCmJTLm63/SBmoqhwf0HzVXtq9NBIaCBJsKUsk8r2jfnGCwCiVMh1cB91Nr+6itHshOiUmamjNPO/R+bz112zOwJTi1WCS8pkemCd7yJnmNu8FHVp15zrS1TMAQYmT/uKqJuRI4A/8bf8FvptuMakzBq0xOP30WNZixz9GlF+2pf0lIoGEe5BaGPO7sOYznrwldRI+0CBvv3N1+0PZrywuTarOCtnSUlMiEKZ+dYgr7kJmquuat9UoO+tmPHXoXpwmxLZENiPxLXEpKY0LabwlgdtVcPO6VsNjrtUHjk6oDBztX0gplTWKFbM0pkSkff0IjUXFsYGALmj5c6ZZUQtpc7olX7K1frSUyYYYcdxxSQPYw26BaUymCZeYaATOI5qodbHDji0HGvpde+RS5+81H0HEuC2gsmaq41d7dBVXXuJAYYYK5IFtudu42oNsFAUo7EhEQidu1zcfmlh9HjSEHt2cO+phos+rMy4wzlkr89dnskZYTZUlhhWzsakSktXdFrM+YKUy2dFIT2WXYsU1H18EE98tMLqbGMpeRITKnaWPvAgBqtaIrBTjVXcMttV2yPJL9aHEsYkR679i0yd5WwD4teqzdXHMucmbaIknO0P6t6+BkhK5YZylhKjsRUwlgB2qJb0liDTWGuaLP9eaF367bTL6cFbc0sqW8r2o7ZOc4Bi16rNVdUn9ZiQKjGTd0OjfOT2u16g/2UMFbHbx67bXvF2tAYgz3zvFdQw4zlnIteV6TppbUuOMiqZde9Rebo2zqEVZXmiurYGpW6a1Kt51lKqccPRlIRu86pgbFO7ccaS2vRKxTbrAJTZGY5hqu+9NHtUeRRiwP1L6lva01Vwg6rqswV1a81jLjSp1RZkWDtg0ukjFphduwa54DNalOC1qJXKOaDNKW5furQC/bf1Vxqsb317A8uZ/jaGrKELVbVmGtLUVxKg0Xhs0aljlpLdb2JHSs4JS1Gr6ieDc1Z+Pin/gs1yrF88uwXZPt4ba29dUlVwrVkCVusZjfXmtpWY5Qyk3iNyU0po9ZSXW9g4Gz/JWkxeoXBhQz2nyKhyefUc/5o39hzqLX21iVVCSMCZ+c4N1azmiuShFru85mqDRbXe01KHbWW6HpTOoGpi1aGRbTCByS7rj6pzRVgrOLUtWIt9m9dUpXwXGMJD2E1m7liuMIlKNW1WlP0mjJqLdX1ZuwMNzloqd+rr6Hanizmuv3wSpnL0doUc0uqEgalJkWPxaq4uaJ6aElGgsg7xWhOa+makzJqxfjBJbrezNnOysCgG62qv2z5Q2qQU0H0mrJ6+EdfupkWrrWypIEjauyC47Aqaq54wJeYHYuv4pA2pSFyZjfWopSjMZXoelNqoIhYMNFBixpqf2XmOBUXvaaqLcOAAaxwrZUlDRxRYxcch1Uxc229fXVIKabeWnr0CkNgRjGG7938QnoNU1O6P2soLSY2Of3k/u/Taw3GzIoTAqLXVF3fWktmQrTHrnWLIDGLnWMNWBUx17V0N0kxitNS2qKZfvXLv6dGMQZElOz6pQTtuWzfNdBqYpNT17ty9kVHUHOcioteUbZNUWvJTGiftNe4ZWoa8tBild1c11DV6YTIfGr1MNZfaoQPQ2BGEctNX8mfxATzZvueAhK5wIN3/yn9PZZWpqNj6npXzr7w1dQcU4B+tNgHIuexam1y9CV1wQHsHGvBKpu54sVZk7E69VV5hdLSgBqhghEwg4il1PjBU6uDf37PX20ef/grm6d+8+PtFdgVfnvsoS+MNlvUBLQsNhhLylGaLJ8+76X7+0Dzy9gP2NaSmZbU3nrmiS+j51gLVlnMFcZa+zCGOTW1eniJ0WuqKuESSUxjq4NhkjDL2Ijyd797ct+I2Tb7aL1qGLJlTepRmiwnf+oP9vcz9gO2tZGZPn1SuWFBc4NJ3tk51oJVcnNdu7FCoSPS9LG06PWX959JDSKGEklMGJCC7XsImCNMcop+++RD0VFsy1XDEMoK//qf/Kn/j5piKj792WeiVzAmuam1TGH/2rZOzclMwCqpuaKrzVLbC2M11GE+hCV1W2LGEEuJJCYYONt3F6j+hSmmUqzBtpw17PSNb1564B4wU0zFx8/+vWf3g3IuVqxQrZXPv/7tB65r6+B82HnWglUyc5Wx7mrq4BJLybJOMXBEiZGYYgeLgLFOjVaZnvjVXXR/DNQItC6b3JQrY9iBbjluXzHJTa1lCiuZqSxWScxVxsqVIrlpCVXsjz/8ZWoMoZQYiSl27OBcxur0y/vPovu1PHj3G7ZrtC0/uSlnxjA468JXPbuvmL7lrWUKn/fuNzx7nq1T60w4PlaTzVXG2q+xVeyOMVVXtWlqe2uJ6eRixg5GtW3KqmCmmOzq3z75s+1abcvV9Jx+7oupKaYEQy26ex+a39DaHK7IrnXn2Dq1ZwoDq0nmuta5SGMUMhvIEFP65dWgKf1bMTE5uyYpufzSw+i+u0DyUgmFRq+td8lxcjU9uZOaAAzc3f/Q7PzWuuEsyVxrnWbOx2q0ucpYwzW1a86Ufnk1iBlCCCX6tGL7qHZm+2c89MN3bc8qv5CsxI7B0vJA/lau7GGGmJJPffbgHMBIqhpSa91w/PNrHVRxs3OsCatR5ipjjROyftl1jKHVrjlTBo9ARMmuRUow2hPbdxelolYIbbrsGCxL6O/q5Gp6zrzgFdQUU+JXDYOh7Hx1w5mPVZirjHWccN3Y9YyhxeSmsZnCJaqD0ZbL9t1HziQmJkTK7DgsSxLKn5wjNTn8rGGAGqY+tTRg/9K64dQ6QbqPVZS5yljHy3Y3GAPuS2saY66l5mmNHeIQ4wKXVujE8q1OoM6E6DX3SE3Azxp29M01zQrUWlEf1/JYBZurjHW6Ugws0dqsOWOGPSyRHTxmiMOSVcJOGE6RHYtlCYNJ+Lryf32QGmJK/AElHF0fsE8/8RQtUGtF5loeqyBzlbGmUYroFeu3VD0c28e1xGARY2e86RuEP5dCxxzGdV6S0O/19PNeQk0xJchMts8Hi17Vx3VeFmmuMta0SjGpOvoWt6IYcy01AXrsEIeOOcbxDTXXJYzUZHXu5/6UGmJKzjxv92OXDSwhc52XxZmrjDWPpg6LCEK6DtSgUHMtNZVc7BCHPnMo1FznaA/OrVtuu4IaYkrcNHQWO11mawNIyFzLY9VprjLWfGLzWI7BFgA1KtRcSwzKjxlvYoY4tMyhUHP92T++c7vGcoRmlI+d9XvUFFPB2l2BjV5bG0BC5loeK2qurY8I1IJsu/YYWmh/DTHXEnO0grHVwY45FJotDJaoS/72WGqKKWHtrsD/eJW5zstizFXKrxTDIoLQodvm0hOP98/wUqI/Kxg7AbrPHArt5wqWqG/ffhU1xJTY/q4Of2S07513DS1Qa0XmWh4rmeuMQtU7e5BiqXnyhL5+rhgsn51PasZmB1tKC9nJ7Di6WKIwahLaRZkppgIDVrDnBriR0VobnWlp5nrxm4+i51kTVjLXGYWCY2rXHEetBttlrhjAoUQCE/YRO1hEF6W74sRUCYMlDSTh65wLX09NMRUYapE9O8DVDMlc5wXnw86zJqxkrjMrxcASjloN1ppAKWMFqHa2+x/LrwvOPhMbtYKlmisy45F4xIwxBacd6h/DGu+ozHVeZK7SKMEU2QM1hhoNFhN6OwMoaaxIlPLNZyqYAq6EMH4x5oxlx9DHUs0VCZZnnHc4NcZUsOfHgej1K+/4G1qg1srSzHUxU85JZZUquckBgx2a4aOkXPVmSWNFO+uUbjdd5B64H9v/+T1/Rfc9xFLNFfrU2S+ippiKj3/q9+lz5Ghh4HifpZnrYiZLl8oLVV/soRpLTd10HnvoyqLGmrKd1ZJzfOEpxgqWbK5I/kP1LTPGFAyZ6yc//nxaoNbK0sz11FMPo+dZE1Yy10qEqtwUIzf5wGBrGGjiju9dWMxYAbKQmfmkIscwiGhjHVMV7LNkc0XV8Kc/my9rmA2DaGkhY9WxNHMFX3jxsfRca8FK5lqRUlcPO/DVP1c7bMqErRBiJz8fA6LLVNXD2E7ozDdDLFl4fjHYAzPGFPR1x3G00O7nWKK51t7X1UrmWplymRHaYUtWE6MwTDEKVQypE5j6gMFOiWBhqqhinhqt+ixdeIZzVQ2fdcHu3K6M2qMnxxLNFefEzrUWrGSuFSqnKaFtN3cUi6roVP13QylprD6PPHjR5rdPPrQ9837BUJ/41V3767BtTWXpwodnrqrhsy86gj5XlouOegstWGsDx8mOv2VqrzmwkrlWKJhfTnPCtnNMuo5q7dLRKsiVGRwDhilEVx1EoxaY6ZREpRB+fs8HtndhuUK7a66q4VBzbaVqGFWo7PhbpvakJiuZa6VCFS57wFKCBCpEA1Mj2blMFdRgrDWwxCnnrPCc4p7nqBoONVfQQtXwEs0VXPHyY+j51oCVzLVipZqaLoRrv/rJ/f2F9o+FoaKKOXWGcwwy1udAd6c1CM8b2keZQU4hxlxbyBpeqrnWXC1vJXOtXKi+ZQ9ZTlBtjEgUWcaIbB34W8rRpKZw+aWHyVg9flVwaMY5hWfw45/6L9QgpxBjrue8/whauNYGO/bWqfnaW8lcG1Cq2XOWwlzJSzWz5D6uvvCRh2eAGeQUYsy1lQEl2LG3zqdPehE91xqwkrk2IhnsM5Tox9oiv/tdfRM25JBrKsFMNswkxxJjrqDmtj8HPgLYsbdOrW3eVjLXhjRHFXEtYISn3CMvtcoaMoWd3EArp5/7YmqSY4k11xbaXTEeLzv21qn12lvJXBtTySSnWkDi0g9vyzNW8BJ45KcXbp+OdQjPROouObHmeui419ICtibQbYgde+vUeu2tZK4NCt105szSLcn1X1Q18BBPPH7X9slYh9yzkXKO11hzbWGWliWO0gRqbXe1krk2KvT5Q/cZ9vAtgYs/p2g1lLW0tzq5PtVnXZiuS06suQJWwNYEpsljx70Eamx3tZK5Ni6MWrOkKPYz5/zR5pa/ez41EbHLL+8/c/skrEfOXDGTDTPKMSBByj6LQ9Se1LTUvq6gxnZXK5nrAoQoFl0USo/nmxKZ6jh+89ht26dgPXJzH2MOVmaUYwiZFcdSe1ITzJ8d9xKosd3VSua6ILVosjLVaaytShhyfV0BM8oxjDHX2mdpAey4l0CNbd5WMteFClnFrvqsRtBejG4VyHRlpiGGWVuWsJPfJQ1tpcwsYzn1rMMOPJ8htDCIP5J/2LEvAXa+c2Ilc124MFYwCqMahi3EMeBY/IkCEHk9ePcbqHmIfn775M+2V3Fdcn1dQapxhlHF7D+rIbSQMbzU7jigtsnTrWSuKxKMFhEtosYSVcfYh5sQwDdUK4yLy8xDdLOGWXC65JsrIk5mlrF84lN/+Ow2Y2CFbE0stTsOqK1a3krmumLBbJFt7Abln5J1jHWxDSSbwExDZ9dxglkwExGctYwlzOSba6rBJPxnOYbaM4aRdMWOewnUVi1vJXOVdgRjRAHmQFUuDNjh/xZrol1CFSczEbHLmqNWCLUgfiHLzDKGMX1cHbVVTVouO+Kd9LiXQG0TKFjJXKVq9PjDX6ZmIg7yr799bHvF1iu/kD39vJdQ0wwF7bb+9mLAQA2soK0JdtxLoaaaAyuZq1SVVD3cDz5ApIPmOnWkplPO/KMD24uhhe44Sx3AH9TU19hK5ipVJURlyh7mrGn2myH5BSz6qDLTDAXttv72YmihOw4GXGDHvgRq+rixkrlK1QkD0TNzWTP44Fhr1xsmv4D9+Kf+CzXNEDD4v7+tWFrojnPRUcud5KOmjxsrmatUpR576EpqMmtljcMc9snvSjZlGERUKfuF9RhYQVsTSmoqg5XMVapWan99BnxoSAdlRx9jxhkCJl33tzOGGmdosbDjXgrsfOfASuYqVSuM3vSzf3wnNZy1sNYhDodkzXXs3K5T2lsdtXfHAUtOaqrl+lvJXKWqhXbGtSY4yVi7Zc11zBjDpx2KH0+YUfvsOGDJIzXVcv2tZK5S9VpjgpOqgvtlzRXzsTID7ePcz/3pgW2MpYXuOEseqamW628lc5WaEBJ6mAktESUvDcua65juOMgy9rcxlnPefwQtbGsC7cLs2JdALdffSuYqNaOlj+CE9mV1twnTVHOd2gXHp4XuOGCp08/Vcv2tZK5SU1pqFx2c1xonPh+rnYSmyL6un/7sSw+sP4VTTz2MFra1sdTBJPDRwM63NFarNtdHHnnwwCD0ffRNmSaV1ZKqiBGtrnmGm7HaNde4vq4psoR9WuiOs+R2V3a+pbFahLnC+JwJuplbMPUZXkAHuyFTQUd2fx/Yp505JtWsMdJBwZBa7qaDDGjMYyuNk32nY8z1U599wYF1U9BCd5wlt7uy8y2NVTPm6qZB841zyvyjpXFGjMnDcQ6Y8xTno4h4vFCN2lo1MUwVbceqAp4ma64nf+oPqJEyMMG6v24KWpgdByy13ZWda2msqjRXmA7mEL3ha4d2XqKlgvPE+bqoVxFvuJAEVPtoThh0X1nA6cTKBWaklmcSmf5wZ92poD2TFbi1sdR21xpqDqxmN1e0ezojveTy4+iFWzMoRBCpI9LFtZK6hapiDLzAzG0OEKXieJQBnF5jzfWT56SvEgatZAzDhNjxt47MdU+uahfVo/7g2yIc33AV4e4KVa5oz0S0yEwvJ2gHRlU1Br6Q8mmsueaIWh2swK0RZDez42+ZVZqri0xlpvnAdcX1xXXGx4v0nDA/LKpjEUHmMFtUR8NMsQ/sSyqjMeZ6xnmH76yTEsw+wwrd2sA0bez4W2YV5ooEnZ/c//39at6WEo6WBgof134rHRSqj2GGSCyCMcIgh/jl/WfuLw+wvqp65xV75jFWMDNVB5Ke2HqpaCWpaYnzuy7WXFE1iSpKRE/sxMX8uMhW7bbSEsSe8b7B+3O1tfrUNHF3H1e8/Bh6/C2zKHOFoaKwVhJSe6AaGTUL+CBSVyCpRbHnustcc2UIW1oZqQksrUtODYN4WEWZqwx1meB+ogpZUa3Uithz3GWuJaJWRyvtrkubgo6dY2msBs0VkQ0iHBnqOkA7OaJatJtLUq1izy7GC7bGeuo5f0SXzUUr/V3xEcCOv1XYOZbGqtNcUbiqDXXdqPpYqlXseUXVLzKCfWNNPYbwEJ/8+PNpwVsjOFZ2Dq3R1Kw46jIjGPjYktFKcwvNU+z5rAUMkM8K39pYymhNtSSSWVFzZScghI+MVppL6F7GnslaaCVreClVw2g/ZudXGiuZq5iM2milkqrdXAG6u7ACuDaWUDVcQzccYCVzFclwbbQauEKyeuTeBzY//fad2/+bphbMtZWxhluvGq6p+5OVzFVkAVnHGP9YYx9LEIz1lpMu2P7fNLVgrqCWiKqP1geUqKkK3krmKrKDblxqn1237rr8+v0C6IlHH9/+ZbzQJ5s9Z7WBqKqGwQ2GaHlACQzlyM5pDqxkrqIoqjZep25671n7BdD3zrtm+5fxasVcAYzLFsK1gTGR2bHXTm0jYlnJXMUsoNoYo30pml2+nn7iqWcLoC+9+sT9/5+ilswVnPP+Iw4UwrWB6Jodd+3UkiXssJK5itlBNKuhF5crtLf6hdB9X/vO9pdxQls+e45qBhFszVXE+ABgx10rNVa5W8lcRTVgmjy0zUrL0m2nX36gELru6E9sfxknNpdrC8AQUk5Lh2QkDFiBCA7miAxlwPbtwDFgGSQCYT2sj+0g+YotXys1Tu9nJXMV1YEqY1T9qcq4fflVwj5TuuW0aq4O9C2FscX0hUWUBgPEejBHmCTb9lhwTKee+sf0t9qotZuTlcxVVAv6zcJk1Z2nXdkqYceUbjmtm6sPTA1mAdO0oA8qflvKGMApqLE62GElcxVNgHZZmWx7clnCDAwsMUYa+3ydwFhrntLPSuYqmkIm244e/9nDtBByjO2Ww54LsWxqN1ZgJXMVTSKTrV8wT1YI+YwZVII9D2K5INO6hbGarWSuomlgskp8qk8wTVYAWWKjV3TZYs+BWCatTD4PrGSuonlc4pNMth6FRK2OmOi1lXGFxTSQyNVCtOpjtRpzRfcOZBn6oEDuAy8yA9OrseV97L6UhJEfXGP1k51foVGrA+MOhwrvH7v3on3Qror+uq2ZqsOqaXPFgPAwLlQNOlPzTbBWIcJyxwgzsIYsI54GPqRqvv9LV0zUCmKGRMR7wu65aBN0M4KhYjAL9my0hFXV5urM0xrnWoSEHd+Ar/3qJ/evB7tWYhdcLyU9ldVQhnAXP/rSzdst9EvmWh8YfOKMD7+U9snF3xxuVCiMroQBMWoeDnIMVrObK6I0GAbGC3UGqgJxWM54Mfg9rp1Ml+PaY6UywuAQrOAZIjR6RS0Vu89iXmqaV3UurIqaKyJRRBMy0XzCNXVtwqpifg5VFefXL+68hxY6oYREr/qIrJcax/stiVU2c0Vhhq9MRFYq1OaVNVx2v9aCuu7kEwbkZ4VOKIhehzKHZa51U/tADzmxSmaueOhReKMQV+FVv1yVMmoS1hbd4nzxnErphKiTFTixDPV7ZfdT1APaVtl9XQNWo83Vmami0mUInfPXZrY4V30ITlds15sh+qJXdh9FXVx01FvofV06VsHmivZSmel6hPuM+437zp6HpYAPCT3T0zQ2iamLruhVozO1ATKG2X1dOlad5opCB+1T6Aair/t1C/cfz8GSo1pkXOs5j1fXlHJTYTPm4COI3TtRH2uMXq2oueILUZK6hPZKfHgtzWgRpevZDxe6ziAJiRU0U2HzveIDj903UR8YbJ/d1yVjRc1VkkK1NKPFeaAQl4Z12+mX00ImFeja4wvNFOyeiTpZW+awlcxVSqYlGa267PRrap/WENC1xx9YAveE3StRJy3NaJMCK5mrlEWujZa9dK2gamKunNXBFn9gCfVxbYu1JTZZrcpcf/noTzcP/vyHz/IPd311863v/m0y/G0DRT7PJEOhiw8GFWEvYO0gClef2IPKXR1scV1zWn2G1syaqoatFmGuzsycyV311Y9uLr36vft85PR/XwVnXfTKZ4/JHecP7vnG/nH/6tePbM9k2UIU2Gq1Mdr7pL17eO8DtGDJiUtuYvdF1A0G6mf3dIlYNWOuMCGYkW+ezMRaxhnwV286Y/88f3z/9/aj7aWp1Wh27e2wqA6eOsThWO685jp6T0TdrGnEJqvqzBVm4kwURgPDYUa0Npzxuoh3KaaLvosttc2iHXatBotJzVmhUoSXvndnOjNRP5gAnd7PBWI1q7k6I73p1gsXGYmW4PzLj9qPdNF+jOi+VWFygVaqjBFxry3RaY7qYAvmAGX3Q9TNFS8/ht7PpWFV1FxduyiM9JRz/hs1CzGdlg0XUSHaN2s3WRzfmgx2rupgC9rw2P0Q9YKPInYvl4ZVVnP1zZSZgCiHq1JGO24r1ZrozlNzu+xaMolTzXiTCoz+w+6HqJO1JDVZJTVXVPMiWkLCESvgRT0gukV1fAtmW7vJLnlEp9Qz3qTgCy86dnPqKYfReyHqQ+Y6UiicUQXZWuLRaWe/aHPuoaOe5YLzj9tc9fkzJnPJhR86sN0zz3kt3X9t+GZbq5D8VOtAAks12NQz3qRC7a/tcM77j6D3cGlYRZsrohwkIdUancLMYGpXfO7kfbO79rLzN9+46rp9vvulb29+8NUfzcb3v/z9Z4/l+isu3T8+HKczYnY+c4H7i1qIGvvg1mqySzPYEkMcTgHD67H7IOpiLd1xrILMtTZDhYG6SBMmBbOCcTFDaw1nwF/7wtX754fznNt4USuBqLa25KgaTXYpBjtnn9ZQUD2s7jn1I3M1qsFQXdUtTAZm861rvk4NKSUPfetz9O81gMgbxutMd44qZ2R5oxmgpurj2kx2CQZbWxJTFxe/+Sh6D0Q9yFw9zWGoHzvj95810jkj0Ufv+Aj9e83geqH6G+29JQ23NqOtyWRbNlgkMZUamD8FKLzZPRB1IHP1xArS1MAEYAao1p27LdTxo7+7Y/MvP3wt/a01nOF+7nPv3Zx54Z/Qe5CSmoy2FpNt1WC/d941tPCoFSU31Y3M1RMrPKeCKl4k76B6t9b20Z9+8wubzQP/hv7WMj/+5v2bf/7nf9qv5i+R2e2Mdu4hGmFuc3fhaW2giRq73oSg6LVeZK6eWIEZC6p5a4tMh0CVcE3meu/X79vc/52fbn5047309xhgsL97+nfbO7zZzwIuYbbY9txZx3OabGsjObUWtToUvdaLzNUTKyRDQFUv2kxLJB7l4Ml7XrhvrqgeZr/H8Mvvnkr/3gWM9Bc/+qfNb3752Ob//OvT2ztxUE8/+fTm1//0m81Dd/9ilOFag/UF88s9AAi2PVe1MRL05hpWEfusfaAOqNWo1aHotU7O/uAr6P1aGlaTzRVZq2jbSx2dTjW42PXvvfFb+8YK7r/5y3SZUO77+5uC224Rmf7LY/EF7+/+9Xebf37o8WiThcGGCCaI7jc5olpsE0MxzhHNwuS+8c1LaSGQkxZm02k1anVc9Na2pjBcCxqhyRMrEH1gqKjuzdl2CnOMjf58fvGdc+jfu0AXHGeuU7vjwFiHso7vu/X+UaZqBZNFxMv20cVDd/1iu3aY0HYKM8QoTux5mAKi2Tn6z2IWntJT3WF/tQr9WlmBUZrr33bK5u4r/m7zizvv3Tzyv3+6+em379r/f/ydLW/RsIj1IXP1xArBEoZq+ec7j9+PKNlvQ8DgYqLXX//gbc+a6xRTd0lRfeb+yx+nj9gQxbJ9dfHoA49t14yTqz5ObbSIZtEGXFrILEZUyQqFHCBqrlH3fe07tMAoyY+u7b//MNwvvebDdF3HOe87gl53MR8XHfUWeq+WhlWvuaINNUeVbyiIIGF67LchYK6hEShM2BkrGNsdB9t5+se/t78NVrWMKly0meZSrMH++p9+vV1znHIYLTKNESWXrkJF0lOp9tgau+jc9N6zaIFRCkSoIfrVz/6p12AvO+Kd9JqL+dCUc57QZaaGDF9neogG2e99wCBDjdmvEnaw5YZw2cbARs0w1id//dT2CudTjMH+8IZ7N799kidPxQpVx6nbaJHJXLJdtlR7bG0ZxHNXCd/26cu3RxImRLhsOw5VDdfFF158LL1PS8OKmisriOcCBoloMKaKF8BcYXIh1couS9gnNqkJy/vr+7+VMlYnZBP7++8jNMEpRkiGgjEywxxDaZNFe2zuQShqSnD66bfvpIVFKZ5+Mu7dQFss245DVcP1cOqph9F7tESs9pxgV6wQngsXVaL9lf3ehTPXofWsKTpihkGE8f/uJ//3s+vaamVEkyWFJKeYLGIkROUQzCNltXHp5CdMhJ6zqviGrx3a7mle3XX59bSwKME/nP/F7VHEiW3LoazhelhLH1dgtecGu2IF8Fy4qmEQUz3szBX0Ra/+cj4wS7Y8w27DN9dcxjUkJE35xzhEiszlPqHaOFU0e+nV7y1msrmrimtof51zztb7bvrO9ijChUiXbctxxcuPoddalGctmcLAas8NdsUK3zlx1bYwvNDsYd/wutpeu6JWR0hCFCJju54zVwwMgShyDsVGr/d+/cedA0yklItmU7TNlqwuRhtpjqxiRMaohp5TrKAoBap4Y4WsYbYtH01FVweYtYjdnyVitecGu2KF75z4iUIwWraMxa+mBbZrDCJil9nbBbbR19bLjBU4c82ZGRyimLZXUDrKRtssolBmnDGUNNkcozzBtOcUKyhKMSZyRVUy25aPRmuqA9QisPuzRKz23GBXrOCdE9d31DHUjupXJfvApBH5YnssiYmB5azB4v/9frEWrINBIuYWzN0/7hByVw8zpaoyLtWFJ0fCE0x7Dj3+s4dpQVGKWz564fZIwoQq4aG+rgDVkew6i3Kg9oDdm6VitecGu2KF7pwws+yrsrVmPBWYJbaJamQMMGGjYsbjPx83SENKoWqYXZ8+cmQPhwrRJ7rzoK8rM88QsC6qnUvo+3fdmDSKnaN7ziP3PkALipLEJDWFRK1ASU3zc877j6D3ZqlY7TnBrlihOzesCrfLYLuqa4vx4H/YXsn5hXZfdo36KJ3dbIXoE1HoFJNFm26JSQJSRrFzVA/XYK4AEWxf++sTjz2+3x+WrcvQLDnzs6b2VmC15wS7YgXu3NiMXAdrSw2JLLPyyCnbKzm/MDGAf31CwOASJZKbhpTCZNGmW6I9NlUUW7p6uBZzdWAMYUSnGCjCcdP74keP0khN87OWwSMcVntOsCtW4OYA7Z+IMm2bJqOvjRO/YSYabC+0LTUrv/n77ZWcX7Fdchw5xj8eqxQmW6I9NkVGcens4VoG7M8Bu76iDGvq3+qw2nOCXbHCNhcw1/0EoD1zZL87/Izh6nnqH7dXcn6NNddaoldfU022VFUxok9W4IRSevYcVlAsAXZtRRkueMeb6D1ZMlZ7TrArVtjmAtGmMyUYaFcU21UtXCUVaay5gpqiV1/OZJmBhlCiqhiz7UypJsb6pfSlV59IC4vWYddVlGFNXXAcVtQJWEHbBcwxpFq3D7/KF+2lyMjFdt3viGoPmNdE7v3mf9znmsteMIhblm2nk4o0xVxrjF59wSDHduEpkVWMj4CxyU6fvfAt263k15wjNOWEXVeRn0+f9CJ6P5aOFXUCVtB2AWOFOYaMZtSFGz/YguriqRHrg9/9d5sbrvrjzWmnvm5z5LvfvvmvrzluNMf+5Vs3h85+5ebWr/zB5tG7/i+6v30q0hRzBbVGr77QT3bsYBRYD+vn1NhqYiRJldCPvnQzLSxah11TkZ+1zN9qsaJOwArZPhBlIuKEEcbOJgP8quEU3PG1/7xvgof/2bupSaYCZgujfeLef3vwGCpSqLmiy85P/+Fn+8s7kGmM6LUVoT117LCKqGbOqTHVxFg+dxIW9Is776GFReuwayrys7YsYYcVdQJW+A6BqlvXBWaMyR4wpxEgQi1hqF1g389GsxVpqCsOfu8bphEDUZScLi+FxiY9YfaenG2xMMrYbOISXXOWmDGswfvnYW0DR/hYUSdghXAItno3xmT99WJAe+iJf/0manhzAJP91yfzVjPGqG8QiUcfmH8UqVyCSWKKOmaifZRoi8VUc6xgYpSKXm87PXyAhhbQIBLzgOvO7scasNpzqF2xgjgUJCNZA8ToShjsoSvxaWh2GkZtpurzre/eub2S8+rpJ5+m17v05O1zClPTjakqhjHnNDVMNccKJ0aJ6HXuCdNTo+EPy7O2sYQtVntOtStWIMfQN/wgkp9gtDBUgDF7Y0ZUQtVrrabqOPT5a7ZXcl5hGEN2f+YYnH9ujem6gyg257yxoe2wJaJXVA0vqUuOBu4vz1oTmRxWe461K1Ygx4AINcdISegaM1ebagwvP/pD2ys5r1h7KxKV1qqxWcWYTCCXMKoTut2wwsqnRPT6vfOuoYVGi2jKubKceuphq01kcljtudaubIE8Bj/BaSotRKuWuauGUe1r7wnaX6XNfptqbMJTzmSnkESnEtHrE48+TguNFmHXUOQDNQXsPqwJqz332pUtlMeC6l9mljGgbbWFaNXy4U+ev72a84hFrXPPdlOTYJSxUWzOauIQgy3R73UJ0StmY2HXT+RBUeszWO052K5soTyFKYNAoA8pM65W+P4P7tle0bLqmiQd3WqkgxoTxebsE9uXSVxi1KYlRK/nvO8Iev1EHhS1PoPVnovtihXMY3EDTDDz7APtq8ywWuIt7z1588STZbNyYaDIBrb3AZGsxIW2WFT7MiPtImc2cZ/B/uT+72+XyqfWo9dTTzmMXjuRnrVnCPtY7TnZrmzB7EBmL/v7EKx7Th/oK8rMqkVKZg7DWO+79X56Dx66+xfbpaQuxWYUw5BzDZ3YZbAlZsxpOXNYXXDKsvYMYR+rPTfbFSucAZKUkAWMwSJiBuvHsqHRa+tVwYwbv/Gd7ZXNpz5jBWvOEo5RbL9YVCnnmsauy2BLzPfaar9XRFLsmon0rHHO1j6s9hxtV6xwdrgsYIAp4vzZa/ro6/vqWKKxOnJmDyMzuG8kJiBzDReqe2NHd/rBPd/Yrp1WzGBLdMuBWpstR1FrWS474p30PqwVqz1X2xUrnH1sNxskLQ1Fs0NVwxgbuMWs4Bguvfr67RVOI0SrME12vS1qc40Xkp2YkXaB6e9yyE5bV2o6upaqh7/womPV1loQJTHtYrXnbLtihbOlqx8rjBZGitGXnNm66mS7rAOzykydDq4VjvvrMzc/+/k/ba/0ODlTZYlLqEnAtcc9wGhY+C/+jmVT6f/87v9s/7V8oU01ppo4R6IT66aDwSdKqJUZc87+q/ET04s41jpf6xBWe+62K1tgd9FlsLFcesHLqBEtGSQ63XtfXDT5r7/+webX99+4eeSOs/ZNE6BqHh809uMFf8P98e9X3+w3MfrNI/+y/dc6BHOL6ROLRKccBusPlfiNb166/SW/7rr8elqY1IKGOiyLqoM5Vnsl8a78AnmIqQaLQSKY+awFdNeB0X71xq9vfvfED/dK0X98jidu32wev2wvfPrgZvPgf6DXz4J7gcE72L1KUTWMqHmtIz1hGERmpgxEu6kziRGtOoMtVTXsVOusOWpnLYuqg7ux2iuRd8UK5j7sVHMxrKU6OAT07WXXKBREq0MJZlOnmcOE6tjOWmbVsULiUuigE1gutcH6s+mUqhqG0P5603vPooXKXChiLYuyg/ux2iuVd2UL5BBQPckK/D5uuOqPqcmsFSR0of2ZXashXNtqCGOMEREr+sq6bfz6n369/WV9immHzWGwLoO4xHCIvmpJcELyktpYy6IhDoex2iuZd+UXxDHEzIQDE1l6dvAY8MHBrlcXqAZGAhO7H10guSlmnGFMUWe7+qy9aw/aQENHdUrdFxb7RrUwsohL6/GfPby57uhP0MKlBJiMW31Zy7PmSdBDsdoroXflF6IxoJBnBsBQ1Mo59i/fSq8XA8Zqk5ZiQBtsV5ITJlqHAXcNTIHq4bULJofuN8xQGSn7wmIuWBR6cwgRbOk22Ctefoyi1ZlQO2sYVnul9K5YYRoKMwGG2lq7wRR77JpZxg5HyYDROoYGpAApkqOWophEp5QGi+phmOxcQjed3FEsIiYNxD8famcNx2qvlN4VK0yHQCIN+lUyE7Dc8bX/TE1FPENo1TC7D6X44Q3zFeo1CqbJzJSRymAROSPBaW5hqMSUozkhSr3gHW9S9e/MqJ01Dqu9UnpXrDC1oAoYWcJIZIppawVLGpg/B6ed+jp63SwpI9cxSAcVk0mcMoKtRaguhtFiVp2YzOLL/vRd+2aKCFWGWg/qzxqH1V4pvSsUnDBON1ABung4WCEfixKZ+sH1YdfNgjbXOQ32t08+vX1iJCdkBocabK4B/2sSDPeRex/YAYlRTnZ4RzE/X/jEq6mBiG6s9krpXbnCE1W9GJAA1b1TBorwUZVwGBhrmV0/Bu5P6AQKKUEWsbSrUIPN0U2nRWEiAlbAi3m4+HN/vLnvO4dvrnnFcdREBMdqr3TeFStIAaqCUQ389I9/jxbyIaxxqMMx4COEXb8+UD2Pj6FSRitz7ZYMNlyYAJ4V8qI8p5/53zc//t6LNw98/yWbb3/mSGoigmO1VyrvihWkFnQBGWO06GrCzEQcZOpoTbgvmOYP1fr4KMphuDLXfsE0Q/rCrt1gMT8tK+hFeb590wv2jRUgev3yG/+CGonYxWqvJN4VK0j7QPtsqMkyIxG7TDVXC6JatJkjsp3SN9ZHc8QOK3SwCYz4hGXXKn9SAjEPl1962LPG6viHy9THNRSrvZJ3V6wgDWGoK87aB+mPITRjmOGiVnz0pDJShsw1TKEGm2M2nVZ07Vc/SQt8UQa/Othy6+l/Rs1EHMRqrzTeFStIh8DcrUNdcpTMFM6Jf/0meg27QMIZqulzmqlF5houmGbIeMSYD3aNwjjJrNAXZbjpK8+jxur40Y2v2txw3J9TUxHPYLVXMu+KFaR9oDtISDYxqjqZkYhdYswV7apuYvqSyFzjFJrkhCEV1ybM8MMKfZGfz5zzR9RQGT/4yqtlsh1Y7ZXOu2IFKQOmGtP3VZnC4YSY69Sxhacic41XqMEucZCJIanddR5u+bvnUyPtA22xSnY6iNVeKb0rVpA6ECEhKWZMdxwYBjMSsUtIm+ucxgpkruMUarAP/vyH2zXWoWuuPYUW/iIf6NPKzDMUdNdRf9hnsNorpXfFClJEqUiSYYV8KDLXcIayhXEv2H0qicx1vGCwzFB9YMC/+vVyr/ETTz61ufX2OzeHLrl6c+TxJ2/eeMyR1ABEPsZErRZ02ZHJRpqrM9RUozPJXMMZMldkAvtGNwcy12kKGex/iRnEMNTTzr1s55l/4ZvfRQ1A5AHjOKMNlRnmGGCyN//N0dR41oDVXkm9q5SG6iNzDQfdltg1dKDbEzO8kshcp+sf7voqNVWfJSQ4IUq95rqbN4cffQJ93h0f+tiLqBGI9GCyBLSbMqOcwv++5RWrNFmrvZJ6V0hSip3pJgSZazghc7riPs0xprBD5ppGl179XmqqPi0nON1w822Dpup411++hhqBSI+bTg7JScwkp7K26mKrvVJ6V34BigQmDJ+HKmJ0+UDENNZ4Za5hhM6K40BNgwbub1NIWgpJbmpxBh1Eqyeedj59xrt42Z+9gxqBSMs57z/iWVPIEb36rMVkrfZK512xgpQB03VT0rFC36KuOGGMHZ0J96FkW6zMdZpC2lxBi9XCDz70cHC0avnrk59HDUGk4+I3H3XAGGB+zBhT4kzW3++SsNorlXfFCtIhEOEOjTGsQSTCuOGqP6bXLxS0lyOaRW0Du1epkLmO1023XkiN1LI2YwWqGs7LqacetmMMiCphfswUU4M22Zs++PadY2gdq73SeFesII0B/WBZoX/rV/6AvkziICHtraE4o8WHT+pRnGSu8ULmb0gbK2jRWFEVjG417LkORVXDefGrhH1iote7vviayW21yFRe0kAUVnsl8K5YQRoLxrm1Bb0G7h8GU/LZ6xYKag1QNdxHyvGHpTiFtq+CVhOYLr3qevpcx6Kq4XxcdNRbqDkARJXMCBkwRkS8yAzG2MNsmRCWUlVstVcq74oVpLEwcwXsRRLPMVQljEgUSWVuntY5R2mSwhVaDQzzbdVYH33scfpMj0FVw/lwWcIMGCUzQIY1RZjt7Re+aVT1Msy59SjWaq/E3hUrSGNA9WNXP1llDPfTVSWM61nDwBGOH95w7/ZpkfqEkZhCppsDMFYs36ow0hJ7psegquE8fPqkF1Fj8AmNQmGibH0Ak44doALba7kt1mqv5N4VK0xj6JvXVUlN3eDDg10zkDs5KZb7v9OuCZQSBogIrQZewkhMU9taLaoaTs+h415LjcEHs94w82MMDRaBbcWaLKJftq3asdoruXfFCtNQkDzDzMHx4Hf/HX2RxHH7CV/smoGaolYgc+1WTNISWMIITCmrhB1/dtwbqUGI8dguOF2EGiKWY+tbYk0Wy7bWL9Zqr+TeFStMh8AgBqGDSxz57rfTl2nN4Jqwa+VAslLqbN8paHQmLgz2EBqtgpZHXvJ1730P0Od6ChprOD1XvPwYagyW697+bmp6DCzLtsFApBvaJovq6ZYM1mqv5N4VK0z7QNebrjZWhgaT2KUvanXg46UWg5W5HhSi1au++lFqoIyzLnpl0+2rVhiMnz3XU3nvh19OTULEw/q39hHa1QbLsfW7gGHeedXr6LYsLRms1V6pvStWmDKQrYruHcwI+kDSDnuR1spQ1OpTi8Gqj+tzio1WYcJLm+kGg/KzZ3sqr/3zt1KjEPGceeLLqCl0ETosIiLRMQYYGsW2YrBWeyX2rlhh6oMq4L6kpRAwxB97mdZISNTqgyriuSdK/+2TT2+flvUqNloFSHJaonJUCzuU2JSG894dF2GC0Oj1m598K11/CFQph2QnYxm2fk1Y7ZXWu2KFKRgbqTI0oMQzxEStPqiGR3U8u08lWLvQVhoTrS6tGtgqp7m+7b2vp2Yh4ugbPKKL0OgVg0+w9UMIrSaOrX4ujdVeSb0rW5AiU7XLVN2oQMgSxsAGKPBhwg50IekaUOLQ2a+kL9OaGJq3dQhc+9Iz4vz4m/dvn5T16Ve/fiQqExgssRrYCsMesuc7BUpsSsPnXz+uD2lo9Dq1j2rIfoa6/syJ1V4JvSsUoCiwYYouUQn/RUEOA4XZwjhtodsHM2e0vWJ6NfZCrQF8XNhrwkA7K66fj/976Sj2obt+sX1S1qWYfqsAyy4lGzhEqfu5+hx7wquoYYhw+kZm6iM0ekX0ydaPYWh8Y7TR1jqSk9Ve6bwrFN7+EHtTE2j6RmzCcH/sZVo6+Kh44t5/S6+JA9e/79rjA8jNs4t7BhOO/egZw9oyhWNGWXJg+SVXAzOlHKHJ8sq3HU0NQ4TDDCGU0Og1hfEhOmXbdoT2rS2N1V4pvitWoE5hKPlpjUMiDlUHj4lEYcQw29zZxGvJFEZVbuiYwD7f+u7fbrewLuXqjuP4wEkvpaYhhgkZ9rCPW0//M2p0llSD8A8ZbI3Vw1Z7JfmuWIE6lqERmwAiuDVVD6OfL7sOPiUi0LH87unfbZ+U5Qrda5CExMyzCyyPmW/WKrS7TpnHdYg3HnMkNQ4xTGw3HEvowA9TEpssfdHy2O4/ObHaK8l3xQrUMYQYq2MtwyL2jR/sU9twh46lJzMhYSm2ew3AEIZLT1oKUc6qYaBuOePomsM1hJixhkHKwff7DLa2qeqs9kryXbFCNZYYY3UsfTJ1zNU61M7qQBt17urdMSw5mQnVuTEJSwDLI8qVntGDDz1Mn/1UKHodx5g+rg4MpM/MrYsUiU0+XWMS1xa9Wu2V5LtihWooMITQMYYZSzVYVHt3TSfXRU3DHTr++aHHt0/JcoSq3NgqYLCGLjZjdOJp59N3IBUfO/UwaiCimynmGjLIgyVlRi8MtGsS95qiV6u9UnxXrFANAaMGuaxg9H9F9Gr7vSKz1ZqIZWkGC2NFtTc71yFqGI3JZ0kjM8EYUZ3LjLMPRav9yjmgBNCgEvGEzoZjgbFZQ7v/ey/d+Ztl7IhNXWAkJ9buW1P0arVXgu+KFapDwEQRaeG/Q4MawDCYkfjAjJaQ5DTFWH3wUcKuZUnu/fqPt09I+xpTBQwUrXYLCU0wVpCzz+thbzhG0WskYweQQPupNbSQSDZlYpOjK4O4lujVaq/k3hUrWPtA1WXoKEGIwph5MFCNinZK9oK1QEwbqw+ifwzgAUMFqAHAhws+SubMIl5Ce+vYKmBFq88I87becfc9+wP1o/o3dxVwF4pe4xhrrqy9NbQNNmYqulDYMIk5jHwMVnul+a5YwZqCse2x11z2AvqC1QwmJhhjrH3trPg7sohhvHOYbMvtrWOzgAH6uq41WkU0iv6ryALOGY3Goug1jrHmyqJURJBzjQXc1f6aMkN5LFZ7JfquWMGaAjtsXwyIYlsZbAKjTrFzCCGmBqB0d50W+7fCFFEFzExziDX3W0V0etq5l9HnuxYUvYYzZujDrmEP0TWHVRdb0B7KtjsV1jUodYbyGKz2SvRdsYJ1Coi4phirD0Y2wkwy7GWbG5j/lPZVRK3s+tVAi/1bY2eu8YEhry1aRZR6w823VRWh9qHoNRxmBkN0tXG6BCKWYGTJFVGyqum5E5us9kr1XbHCdSxjq4KHuONr/7maSBZJS7FzsjLQ1squYQ08+sBj26ejfiHajB0L2IEZb9Y2JjAEU805ulIu1O81DGYGQzAD86PRkPGGc0WUMFJr7nMPiWi1V6rvihWuY/C75uQCkSKGE5wjsxgRNNqDx7StdhFaLVyaFrrgTGlXRYS71InM+5Q7s7cEGrVpGGYGQ7D2Vn/Q/NCRm3JFlDaynrtq2GqvRN8VK1xjQZccZh45QTQLo81dbYxkpRSRKmPMgP25qb1KGNW3YwbYd6x16MJLr7qePt+toeh1GGYGfcAQfeNy2G4vXYM7+OSMKO3oTXNWDVvtlei7YgVsKKgGtrPg4P/dYBIuExazt+SMapEABQOE2U6tPoZZY+5VbC9llMpAdxt7Teem5irhsf1VwVoTltC2euxHzqDPeqtoxpx+mBn00ZWwZNtQQ7rl5IwobfQ8Z9aw1V6JvitWwIYA83SGCUMdmv4MXUqsueQEhouEKJgkqnMBTBPmCxCRur8j4xfL5jZTRm2D9tdYJYxkpTH9VQHMGKa8RmHs39argRma77UfZgZ9IEL1TcthhzWcu2oY+NErzJ4tUwKrvdJ8V6yA7QMGCqNE1IUItc9QfYbmeV0SbmAId53YMo6aotfaqoSnJCsBVAGjbXaNgrG2mLQUyns//HJqLCLeXNlg+V1da0KyhnNWDfsGP+eAElZ7pfmuWCHbBaJVjCCEKJX93kfuZKeasNEorltf96QahjsEtQwcAVNFJi8zzBBgyGvtswphZKUlGyt44ZvfRY1FxJurb4wOP5nJZ86sYYf/MZBy0oAYrPZK8l2xQpaBzNaxIwXBjJmpLJWuaB7Xj3VVwofH3JnDP7zh3tkHjkCUOWZwfQeqgFGFvGYtsY2V8bqjP7L5/DmfoOaydpgZdIFhC31jdHSN4RsyoATIWTXsR69ztbta7ZXku2IFbWpSDSrRCuwa+KA63UbyuEZs2VLMOZbwVFMFaxwIgin3BOY1cOiUKzf3XPuPmx988R82Z535Omowa4aZQRddg0fAwNjygC1vyW16ruvQXO2uVnul+K5YQZsSRGu+ibROSPU2uw4WRKr2o2POrjlP/vqp7RNRTilMFX1d19quapV7+re5QbR6y6W37hur4xtXXEMNZs0wM+iiq5q3L/JEtS9bx+fOq19L102F+yjoqr7OjdVeCb4rVtCmhEWt+JubtxTtk/b3WkHbKKp8h5Kz7DXoA4bq1oNxzzGf6/3fKTtKkRsDeGy3GoA22TW3qzItMTPY8bETz9/cdfX3Dxir46wzlD3sw8ygC5bMhKiQLevA/K12Hcv9d7w0ez9Ul1zFfsuN1V4JvitW2KbCRq0wD5YM1VVtjOQpbKN0N54u/DZnHBtbBvjLhQBDdW2xfTPl5KJUIlMKU0WXnLW3qzJhNhtmSq3zkjd9YHPtZ2+gpgqzPeEDn9lPbtK4w8/BzKALa4pgaIabrnZay51X501sclF3junuhrDaK/l3xQrbVPjdUGAaXUk7iAjdcg6bcZtjzGIGjrnL7H3ThAF2HdOYbGo/IoZxs2VyUGJS9BSminXXOGRhqJYYtR79rpM3t1/5XWqsN1709f1qYresZs15DmYGjC6TDOlKEzJaE8jZLccdf859dGG1V/LvihW4KfANE6bRF43ZbGLWNYUZ8BhgiNgWazuFsbrjhJHaZazhY1m2HXb8obhq4lKDS+SMWtEWmsJUlazUL/Rp9U1pCfRVAyOhia3zoY+9iJrN2mBmwOhKZgrp3hLSJceRM7KEyXdlNufEaq/k3xUrcKeCCNWZTkgUBiNzxgTzY8v0tc1iXzC0oejWN09UxdrfraGFmL5/7A5Evna5GJyx+5FyDnJFrSkSlQC2IVMd1lLGDXZ84eyvUFOF2R77ntPpOuBlf/YOajZrg5kBg43M1DV4hKXLmBnYZi6DRbbwHElNVnsl/65YoTuV2OpN36C6EnqYiQEYqh8V+wlCFmuefrtp10hJfnsvzostY6NqGCNbLgaXTZwzwSl11JrSVJUBHK6l9GtF+yqqe5mxIksYv7P1fN71l6+hhrMmmBkwWDJT6AAQXZOrd4EkqRwJTujyM5SAlQOrvZJ/V6zQnYKL9mLaDZ0h9q3DzNUaq4MlQDHD86PXvn27tuMuAwa2nTaFKeLcYNzsHKeSMmpF1q5MdR5h0AhmMq2B9lXbzcaBSJatw8Ck6muflo6ZAYMNZRhTxRra7urIZbDYNvt7Tqz2Sv1dsYJ3LDABmFiMsQIX+fWNUmSrhbGfLgNzx+Ev33VMzjj72jh9Y+8yOr8qHKTss5rDXFNErVOHKXTIVMdrCX1bYaxd7atoe2Xr9LH2gf2ZGTCs+YG+wSMsMe2ujhxDIyICLz0MotVeqb8rVvCOBdWmscYKsM7Qerbqdci8bPTalcHrlhtq33SRaV8msF8lPbXdNSdTo9Yps9T4yFSn64abb6MG0wroSsOM1XWzYeuEsObsYWYGlq4ZbmIiy5h2V5+hrj6xINqO+ShIgdVeqb8rVviOAaYzxlgBjKgvagW+uYYYl1/lC7qiP1eNzX7zwfFhOZgx+93hVw/niDhT8C+PxScJpehO45CpptM1191MzaUFEJVaU3XGimiWrRPDWrOHmRlYmDHGzjIT2+7qk9pgS2OVzVxhIkOm00dXopCPMy1UvQ4ZscNV+XZlIAMcO5Zhv1nw8dC3LeBXD/dFuXMROxrTLx/9aZL2VCBTTa9WzbXLWEMTl0JYa/YwMwMLyxQek3Ub2+7q07LBWmUzVxjr2CgNZhRiQq6bTUxbJswQ6wxF1KERt4teh841JEFrDjDzTehk6Kj6TdGe6vqpylTz6MTT4tsk56aEsTr+7Lg3UgNaMswMLGx84DH9Rce0u/pgKEW23dqxyhq5sr+HELouzGooarQ4k0OVMvvdMfS7DwyzL/nJgWNFBMt+m4tf/rjf4GCAMMIU7anYhgZ/yK/WzLWksTo+cNJLqQktFWYGFtYNZ8xMNiHjDA8xxwhLU7HKZq65cdm6Q0lHltD1YrrNYFshEalr862lavjH37x/e8d39eP7v7c/uwwzyVg09m9ZnXbuZdRQamQOYwXonrOmsYeZGViYyY1JCuoaQjGW1gzWqllzRaQ4porVVeOGttGGEtJGDFBdXkvVsJ1SLmWUCmDOmqWmvFppc53LWB2v/fO3UiNaIswMLMzg2HIhsG2NoSWDtWrWXGFQYw0S5sr+PgWYfUi0iyrv2KrsHPjVwYgqU0WpaE+96dYL1Z46o1ow17mN1bGW0ZuYGfiwaDN02EMGq2K2uMnNhyjdpWYsVtRcv/aFqzffuubrtFCuhdjqYJ9c/U1DE6tQLTxn1TCqg1Hti0zdFN1ogKv6VXvq/Kq9n2stxupYQ/ccZgY+rI/rmExhB8s8tvzjdUcEGWzOcYhTYkXN1S80zz101OaKz528ufay8zffuOq6zfe//H1aYLdETLJSDCFJTY6YDOcU4L7ho+nznztx84lz/vDAPZ4CDFpVv3Wp5hGaajNWsPS5X0/dOzdmBj6sj+sUc0UilN2eBV120C+WDbloacFgrQbNlfGxM36/adOdEvW2BO7LVZ8/Y/9esfs4FkSpmEdVUWqdevSxx6mJzA1mr2HGigEi5jJWx5LbX8888WXUDHxYpInZZdiyIYQOJoFlu0aGssCMc4xDnAqrUebaBwryC84/br9Qv/6KS6s03tTJTDXw3S99ez8yzWGmANXHilLzCdGmD+ZjnaLDjz6BmshcdI0VnGrkpRQstf11rLmO6ePqExKRuvbU0O47uQb6T4FVcnPt48xzXrtf8MMAAIx3KVXNJcE1w4eLM1LUJLDrnQIkOqktNZ0wY82tt9+5OXTJ1cH9UbEcutcgUQnGi20Mqaa+rn2D8PfNxToHS2x/DTHXVANI+LBtWvxs4NDBJ2odxcmqqLkOcdrZLzoQ+QJEY86EmdEsEXxs4HxdJIrqd1wXXB923VJz/uVH7Vf7KuM3nWCKMFRWoI8B87XCbLsi3FoyhlHd22WsaH9l68zJEvu/nvP+I6gZ+LDs3qnmyqJhi1/1jIg0NIN46rHlwKoqc40BZuOA+Tgzdm3APjVExsi+dsfjTNM3zpLmyXARqgw1rdD+mTuKPPL4kzeXXnX9AaPFv9myJYGxIlGJGWvMfKylWdr0dOe9ezjSy2GuIW2pNmkKBhtSnQzGjB6VE6tmzTUFvkGnhO2rNpCUhP6o6JIj5RGqf0u3fcJo0RUHVcf4N1umBH3Gir+zdWpiSeMPz2WuMEq7TQuM1K6HrODQDOLSc7b2YbVqc10TMFMkJCk6LaOUVcBjgKnPaa6ITJmx1pAZHMqxJ7yKmlVrXPzmo6gZ+OQwVxAyQw4zSNY1iGEj3zmxouaKwhcRDYbCQ3VhquHwRDkwew3uH+6jkpHKam5jnZsuYwW1ZAaHgPbXJSQ4ff71w9Wnucw1JKmpq3oX+2fLW1IcZwqsqLl2Cd0wEPmg0EbhjcQXVrCLcqCLDO4FqnhxbzDXqjSfUBXMCuq10DVIBKgxgWmIJSQ4feHFx1Iz8GGZuilMK8Qg+/YTmkFcwwATVlHm2iUU6DBemC4KeRT2inbT4kwUVbu4zrjeqt6tSzUkEc1Jn7HWnMA0BCZYb9lgmRFYmAki6mTLxjAmqcknNIMYy7D1S2KVxFz7BAPwI15nvoCZyJpBTYCLQl2VrgZtaEc19S0tTV9f1jmHNkxFqyM4hfRxBcxcU7RnhiQ1AbauI3SIxLln0LHKbq4hcgYM0L8SxgLQ3uuMuNVI2EWcDndu+Nhw5yy1L/RjZYXyGugzVvz9dUd/hK7XGm885khqYDUT0scVpB5b2CckqcmN1NRFSAQ8d/RqVYW5jpGrivbxjdniR8yxuEiS4ZukQwlE69Nao9a+LjegthGYptJaBnFINxzAzAumyJaNJaTdNKR9N6T9ds6uOVbNmqsk1aJaB8ovQZ+xnv43l9B1Wqclgw3JFAZdA+2zZWMJGTc4NEpmWc0+2BdbrwRWMldJmqja50/NRV+Xm5YTmEL4wEkvpWZWGyGZwg5mVmy5WEKqdAFb1zI0glOqquwxWMlcJWmi1lglfOiUK6mpgiUkMA3RQh/YT5/0ImoCXbCoMFUXF7tdRuhwhkMDTLB1SmAlc5WkiWKF75Lp63KDBKaWBoqYQu0Ge+i411IT6IK1jQ4lGoUS0p0mZv7Yvurhufq8WslcJWmC1ta3FcbJTNVxwgc+Q9dbKjUbbMiwhz4sYSiVucI47bYtMQlUfVXNc3XJsZK5StIErakLTl+XG4CqYrbe0qnVYJkB9MEMKySLNwRU+dptM2Kizq4s5JgIOCVWMldJmqBa5k3NzVCXm2s/ewNdby3UZrBnfzC+Gw0b8CGVuQK7bUaMMXZlOM+V1GQlc5WkCVqDuQ4Z6xoSmEKoyWBjq4Qdtm00pbmGDOIf27e2K3ply+bGSuYqSRO0BnPt63KzpgSmEGCwc/eDPfXUw2jhH4I1q5RRYOg0cjFVw13R6xxJTVYyV0maoKWba5+xgrUlMIUyp8GGjsrEsAaY0lxDxxmGwbP1u2CZw3MMJmElc5WkCVqyufZ1uQFrTWAKZY6xiBG1xgwcYUHE55tU6vbLrmpcHwwSASNm6zNYIlaKGX1isZK5StIELdVch4x16SMwpeKVbzu66HR1U6JWhz8CUmpz7etC4xPbncZGrzgHtlxOrGSukjRBSzTXoS43SmCK44VvfleRRKdPfvz5k6JWh594lNpcwU8Cpo+LTWxipl26v6uVzFWSJmhp5jpkrPhtKVPIleZt7309NcVUhA7SP4Q/0H4Ocw3JGgahwyE6bPSa49j7sJK5StIELWkQCUSjt1/5XWqqDmUGTwPVxCed/DxqjlNIUR3s8NtdcxhUyNRxIHbfLHrF39iyObCSuUqL1BNPPrW54+579iPLQ5dcvT+4vuXSq67f/x0GieXHaCnmOtSXFaAdlq0r4vnwca/fTz5iRhlL6IToMbh21xzmGjIUoiPWHG30iurlmOSoKVjJXKXFCAaJ6d+O/cgZtEAbAuvBcDFecIzYtloixFgvPv1quq4Yz9mHH7tvjMwwQ7ngHXmG+nNVtznM1RpgHxjUgm2jCxa9xnbtGYuVzFVqXpisHNEpK8DGcuTxJ+8bdUhEi2XZNlphqC/r2oc2zMXLjzhuc/nzjt9PQkK1LhKSmIEyzjzxZZvLjngnLeRT4Npd5zZXEJuYxLr7lDBYK5mr1LRQrXv40SfQwisVMG4YeJdQxczWa4EhY1VmcF7ed/h7DhTQMEwYLczTmi3+hmnkcpqqw7W71mCusVW7WJZNqI795qwitpK5Sk0KEWVpU+sy2VYzhoeMVZnBZfj0iw8abC3AoHJEfLHmCmLHOGbVwwDnlKuLjpXMVWpOaBPNHa32ATP1q4uROMWWq5kQY1VmcBme/+pnqodZgT0naHdNOXC/A5Gob3ihZhs7XnBfVjKOASabMpK1krlKTWluY3WgnRWmCsFo2TK1cvrfXEIN1UdjBpflyFfWF72i3TVHlGeNDvuws/EwxlRRhwy3iI8InOvUbjtWMlepGaFKtgZj9UFVMcx1bIZyaYaGNQTqcjMPH3pZXQaLGWcA+20K1txganZM4y7GRNIhBusDo3dRO4w/NGK2krlKzahWA0MUe9q5l9HfaiLEWDVm8LzU2v6aEmtmLmL0R4bqI7Z6GGDbLMkpBkTOMNyu/VvJXKUm1GrSUC3IWNsA7a/nv3C34F4S1rT86tiQKHPswBBYByZr23zHgG1gW/5xWMlcpepVY3VwS4QY640XfZ2uK8rz2lfVmeCUCmtUvrnCrELaX1Ft628zFlR3wxxh5mOylx2Iht02rWSuUvVKPUDEmhjKCgbqy1ofb/+T5VYPW4Oy7bow2JAq3NSZzNgvjB5g0gBs34EhG2HCwDd/3+StZK5S1WotE7cmZKxt8xevWKbB+gYJ2DJo1wwx2Fx9VkOwCV9WMlepamEIQlbwiH5CjBV9WWWsdXPKS5ZnsNYguxKEQgwWv49JcMqBlcxVqlqtdHGpBZglxgJmZuqjQSLaYWkGaw3Sb3O1oHrWLm+BwfqJRXNhJXOVqhUSmVhhIzgw1qHZbYCMtT2WlEFsE4j6zBWg6tdfnoF20LkN1krmKlWrW2+/kxY0YheYpYx1uSypi06suQIsM1RFPDWDeCpWMlepWmFuVVbQiIPALGGazEx9ZKzPgetw7HtOP0Dt7c9LMViYoG+KoVm/IW2wOSYaCMVK5ipVq5ancitFSB9WIGN9ptociV59HyK3X/ndzaFTrqzWaJdgsDDTsYYYYrBzZRBbyVylUUJ76L33PbADBtZPJQ0c0c/Fp19NDcIiYw1vj3bgmsFk2bbmpnWDtW2oaC9ly3URMtDEHAZrJXOVggQzRbeY0GgSWb6o1p1itmy74hmjwIhKzBQsMtZnCJkJiFFrP+CWDRbtp9YMY5ORsLytXvaZo4uOlcxV6hWSiqZWz2JgexizPwfqkJQpzIFRouqSGYFFxvocoR8jjFqvY8sGa80QXW7YckPYKmaf0gZrJXOVqGCqMEX2Uo8F1bwYgD9EqGJm21gzoe2rQMZ6kCnmCmSwabEZw1MSkWDMXe2w+HupLjpWMlfpgDABeGpTtWD7MM8+yVyfwyXisEKfgarM1x39EbqttRJz/bpQFXE6WMQ5xQQRoXa1w5bqA2slc5X2hWrY0tm5aJPtksz1GRAtxSTi1GoAc4OuNux6xYLRr9j2a6ClkZxghtYEQ7vkdAED7ZqyroTBWslcpf320Lkyc5H4xNpiZa7xSTio+pSxdpMiegW4L2z7NdCSwdp5VVNV4SJTmFUToyqaLZ8KK5nrigVTO+3cy+hLWhIYu80qXrO5wiBDxgf2gXGwbYmDIIJFF6bQpDAG2l9rrnZvZTYdzKdqDTDVKEtd1cQ5B5mwkrmuVDCzmgbFZwbLlls6J3zgM/uFNyvUu6i1P2btIEFsrMnWXD0MMB9s7ROuI0q15gfGZg4zMA+r3X4ug7WSua5QMLEaB2iwBsuWWSpjolUAg2DbE+GEDsZhQRTMtlcLr33V3rk9nxtBLbA2UlTp2gnUp8DGJc5hsFYy15WpVmN1+AabO2u5FlBIx0ZQ6mqTlphuTg60cbNt1QQyiT/zIm4GNYDolbWPpk5AwrbsoBOpDdZK5roi1W6sDmewSx9beGy0qq42eRhjsLVHr46PvrTedtiugSByJCChndc385QGayVzXYlaMVYH2oMPXXI1/W0JjGlbBUhcUkZwPtB+za57Fy1Er46a22G7+qjmqL5FlbO/v1T7sJrdXLsGgAdSGiEruKbkpTWDiHPsaEE1dwFZErH3p6VaBLTD1jjgBLJ7WfUwQHVuyipihx8xpzBYqyLmisIdI/9g6DtU9Y0p6LEO1sU2MDRfLeaL48AxIcrC8QEWIbrf3Dmgb2mpc6ihu414JioaE61inVaqH5cAzDLmPrXYDarGamI7W45PrkEgYOquv+1Ug7XKZq6ohsQIPLkjJpgVjMolweQWPhRiZocJAdcIhpvjHPBRw/YpyjEmYcmxtoEhnvfqZwp+9ltJYqqHYcRsG7VTYzUxDM4aqwORLcyQrTcFmLbrsjPFYK2SmiuMByYxV5YnTCp29pVQofq6RBsgrl2qc8A2WmpnXRqIgMYkLDnW2n/1vBcev5/hevgR/PcS4IMmJnpttUtUjdnEfQYLkJTE1puKmwBgrMFaJTFXFOIwnloKchwHTD6VQSECZ/vJSYpzmOO4xTMFc2xijA+i3DVXA7sqS0RV73v5fFFszD2sfVCJIe772nc2X3r1iTuGMRdDBotM4pR9YR2IYrHtMQZrNdlcYQC1Rkc4LlSLjhWqaefuazn2HDQf6jwgghnTrupAIb32bOC3/cnB9kAMhHDMK96zX2XMls9FbPTa6n1DjR/0xKOPb2456YID135OhgwWUWauKBbbxf5j2nmtRpsrjCd3e2oqkNATGwEiaYptay5iz2HJ3VhqZEq7KkAhju45bNtrAybKCi9EshiYHuZbymhjRm9qtWoY+SO+aopi2fjDFkSaOdpisc2YoRitRpkrjKfWaLULfAiEmlNtxurAOYQkPeE8W7s/rQJTHdu1xoH1NSjEQUKG7XNts6hGPuFleaqPcV/YPWO0WjWM2kerp594anPb6ZfT614aNnwhA11rcmQUh2IVba4tt+OFGGytxuqAaQ4ZLBKi2LoiHVOTlQCiVfVd5cQm2cCM2XZSEDOfbotVw33NTo/c+8DmuqM/Qa95SVxbKDNVH3SrSTnwfwxWUea6hKpGWwXiC6bVQsQ3ZLCtVNe3CEw1xbygilb7QSTKCrAuYMZsOymImVe3xar9kBq9H33p5iqqiruGSrTkGniiD6tgc11SGx6rBoFaMqWuKFyJTHlIZapqWw0j1lxzVQuDmKrh1gaUQDkSKiQ8fe+8a+j1LwnaQruGS/RBVXLJKNYqyFxrryqNBZEfTMgXDJctWzNIcrJa2r2am1SmCrCdtWcCh/LmV8ZFHbm77IRWDSOpja1fK2hCihWqimvIKkYUG9oWy9ZPjdWguaL6kd2U1kEk7tRyAhDM1JeyhNOA6dxSmSoKZg1fGEesuWJ5tp1UxGQNt1TdH1Il3KWffvvO2dtj0dc1pC0Wy+SuJrai5urGvMWFX+qcmjBT92C1GLU6/POA1N46jRTZvw4lLI0n1lxzd83Bc8HuMaOVLjl+gDFFNZgsxiUeimJRlZyjy47DiporMoKhlk0nBJcl1/oHhN+GzH4Xw6BATGWqQFXA08DQh6wA64JtIzXsPjNaaXdNPXHI3CaLyDRk4IlcBmtFzRXRzxoSY/DltoSB7V30qmSmOGB+MNUpgz9YYNCqAk4DK8AYOTOFfUI/vlpod+3rNTFVc5ss+sW6mW4YMFhEumzdKVhRc8XFX8M0ZfiIWEobJaJXfImy38RB0CaGNrSYoe2GQIHa6gg9NRITuZ7+4jLjD8eMNVx7u2uJ6S7nNFlEsW6mmy5SG6xVp7muhVYTmSyo2pa59oOIcurADxYY9Fpnr8lJTJtrzm44PuhCxZ4BRs3drVgvg5yaM7t4KIodM0B/F1arN9cloZGZdslR9QucqapdNQ81mivuNXsWGKgZYduYGwQTthtiKbl+sqUHo0AUi0ElmLmCVAZrJXNdECkncG8d15UmZdWvQ8lK+cEsOKwAY+TuhuMT+jyhfZatPzd+8uNcwrjFmBygdJVxX0YxMomndtWxkrmKxeCi1JixYGOAqbbUh7FlYkZoKmmuMRnlbP05iRmNqZRQZVxy1CdkCndVE0/tqmMlcxXNkzNKBTLV8sQM3F+qWhjEJDXhuWTbmIM5q4NDhGgWCVAl2mYRoXYNn4jIFu20bL0hrGSuoklgdhigIXVbqgNGLVOdD1Z4dVHSXFEzwp4XRk3Z43Ykt5qFtllUG3/lTR+l9zsFQ31ix0zCbiVzFU2BAit1xq/P0hOV0MWF/b0mMBk6K7y6KGmuMSM11ZJF7gYFak2YiYfd75T0GWzszDpWMldRPSjQclb7AkTAiISXnqgEI8o9yP1U0G+VFV5dlDRXwJ4fRg1JTTkHi8itEuYKkOjEzBWgmjh0Zh0rmauoErRXoTtDrmpfBwrAmqrvcoMxeC9/3vGb815YNhEolNhhD0GpQSQcoR95eHbZ+qVAAtOUgfnnFqqG2f3OQV+iE0AUi0kC2LoOK5mrqIZShgoQCa91mMKPvvS5yPCUl7ynqqrimEQmR6nhDx0tZAzbCT1aFDKJ2f3OBaqAh0Z1wvR1XVXFVjJXMSslDdW1p649SYlFhzDc3DPLDBHTt9XCtpeLmDb/OT7gYKyYKrR1lTZXB7KF+6ax6xqb2ErmKoqDoeFKGSpYW9VvCKxdE9XFMNk5ItnXvCq+Otin5DHHdMcp/dwtxVihuczVgapiJDyxgSdYO6yVzFVkxw3ukDspyQf7gYGrKw1nKCMX1cWl2mRxLDB2dhyhYBts2zlA4ht75hglM4bf9VcfXoyxQnObqw+MFoaKamHA2l+tZK4iC6juRSEU0z6VAlTZKUoNI8TQkPiE7OIcVcbYJjJ92X5jQcTN9pGDmO44pTKG33b8OzcP3vPpbQm+DNVkriFYyVxFEuaITh2oXoaRK0qNI7bLC5ZHu+jUKliYKgz74ufz/YwB22L7ykGMuWIoTraNlJz4N2/b3POdwzcP//jj2xJ8GZK5ilUCM3Vtp7nG8u3DVfvWNMRca8DgWCERAswMVcfYRkj1MdpUYcyxhh4D9sH2nRo8++yZ7IJtIyUvOfIvNhec9+bNT+76wLYEX4ZkrmIVzG2mAIaKyLjmuTJbAqbICokpwHTRNcYxtS01hpJVw+z57ALvDttGamCymPWm9S44TjJXsUhQxTq3mQIZal5YIdEqJauG2bPaRenuOMgYbmks4S7JXMUiQAGAdkskBJXqItOFDLUcJSPLEqDqmZ1namIS9eZKsMPQhy1nD8tcRXOgnRIv/NxRqQ8MHcczR6f7NTNmRKSaKRW9xpjr3AP41zBB+hg9/rOH6T2uFSuZ68JxESkiwdLdYoaAsePYlJQ0H0szV1Aieo15l/DusW2UBFFszfO5dond31qxkrkuBJgoolF8JePFn7tql+Gqe3GcpZI8RD9LNFdUdeceyhHPMXvGGXgf2TZKg7bYe+97YFvKtyF2f2vFSubaEEgycpFozSbqg2PEsSo6rZMlmivIPVMOnmn2vDNQQ8O2MRctJTuxe1srVjLXyoAJWQOtpV00BBwr2k6VjNQGaKNkBcUSyDlvbYy5AraNOWllAnV2X2vFiporCnYU8ECj3qTDGaervnXmCdgL2QK+maqqtz1YIbEkcrW/tm6u4NAlV29L/HrF7mmtWFFzZQ8HQBWkMwN02XAGAZwZg7VUATqztIYJs3HXqfZq21hwTu5+y0zbJscgErWB9tccExAgAGHvRxd4X9h25qZ2g2X3tFasosx1Cs5sfGBCzpAsiIScceXCN0QLPh7s8SIhh53bUsGHARI3lNG7TFINmt8CqSNYlB/snekCy7Pt1EDNXXXYvRzDNS/74Oba15xEfwvl+rd9evPdc27c/MP5N+9z84c+v79d97tVMXMVdQMjdbURKAgUlS6fJbe3MlJOCB9rrviQZ9uphVqTnNh9DAXGd8tHr9zcfcVztYc/vOrO/b+x5fu47dM8OxzbdqZtJXNdIWgnlZGumzVUCTPwQZGimjjWXPGuse3UQq2TrLN7GAKM9c5LvkXvBfjWKdfQ9RgwY7YNB6JYLGclc10wro0cLza+nGuumhJlWWoXnFBw/lNMdmnmCo79yBnVDfrP7l0IXZGmD6p52bo+iErZuhYsZyVzXQCIRGWiIpS3/cl62lqHwGTwY6aqizVXvJ9sO7VRWxcdds+GCDVEF3H2EWLSAEZtJXNtBN9AXVcpdZOqj1IDx48FE50vbbD+MWCQCXxksGsUwlLNFdRUPXzd0Z+g968PGB27B4y+6DXUpMFX3nzK9oifk8y1EvDyAZdB7bKl1R7aFjAvFNy5h98bA44JkRorSNYCJnjHPWLXJxb2HnfRkrliHOJadMtJF9D72AeMjt0DBrJ/2TbAUFurA0lSWN6Kmqsb5B2srftJatx19PsFO+NU95ZlggIcJpaqEE/B2o01pak62PveB9tGrdQyBvEYcwV+hvAQLtt37DZccpQVNVd2sUHXoAnA9gtd2uAJrlrWYfvouusC2LUT6wGFOF42VL9OqXpMxZqNFYlLY9pUQ2DlRB9sG7Vy2rmXbd1gXo0119CoE6C/ql0/NPpF1OrM2SrKXKfiBp5n+EZVCnYcQG2ZYiqIlNyLin/PVU0MY1lbf1aAD5ucYwsDVtj2wbZRMzVMUTfWXEFo5MkSmxCNsmUtfp9Zq6LmKsRasBm5KOxLJjvBzNc0ApNPqSp5Vtj2gQ93tp1aueHm27aOMJ+mmOuNx5xD7wPDrtvXR9aBqDV6hCZ2oYUQcbCs3Kn9K0OAia8xWgWoJWDXJAeswO2jNXNFv9e5NcVcQWhXGrseW8ZiR3qykrkKkQlkDfsvnw9MNnUku2ZTBYjU2XXJBStw+2jNXMHcg0pMNdehkZocdh22jMWPWoGVzFWITKDNz3/5GIhuMebt2GgW1c+I1liUvCbm6F/MCtw+ah9fmHHH3fdsXWEeTTVXgIQjVOGyewJcVxpHSD9ZRMT+OsBK5ipEJsaM34uIFmaLKAzrW2DY+B3LsfXXyBzGClih2weSKNl2ambuGXNSmCtA9m+XwVqjDIlcsT1/HWAlcxUiE65LjshHyTZWCyt0+2jRXOfukpPKXAGrIsb/2+pd0Bfpdg08YSVzFSIj7CUUaUD0zq55KVjB20eL5jr3aE0pzdVx0/su2E9Gwn+ZsYKuRCiYbtegE1YyVyEywl5CMR20Mc/Vd9jBCt8+MPId207NzG2ut51++YH7jupYGJ+bsBxRZMjsNrF0tdPCkNnywErmKkRG2EsoplPDyFe24B0CI7ux7dTOnPrRl54b4KGv32qf6Y0FUS0Gkwg1cSuZqxCZwOhI7CUU00AXJ3a9S8MK+T5krvHyzbWvHRS/dVXxlsJK5ipEJpDdy15CMY1aJkRghXwfLZrr3NXCzlxDusfkiF5jsJK5CpGJtQ4/mJM5s4MtrIDvQ+YaL2euIQPx2xGTSmMlcxUiE2ufOzUHtUStgBXwQ7Dt1MzcXXFkrkKIA6iPa3pqaWt1sAJ+CLadmpl7EAlnrkgqYtfTh1UL428uKQlZxjmrjq1krkJkQFXC6akhQ9iHFfBDsO3UzNyTpseYq5/Niy47XWMKdw0cMRUrmasQiUH/y7WP9ZsaXE92reeEFdxDsO3UzNyKMVd/cIehwfqxPbdsKqxkrqJK3nb8O+nfWwBj/7KXT4ynpkQmByu0h2DbqZW521uhGHN1zwqqftnvltTRq5XMVVTJ8Se8Y/OJ046iv9WMut/kYa7B+ftgBfYQrzv6I3RbNXLr7XduHWE+hZqrH4mGGDFgg0LAmDFgBLaB/2LgCrtMF1YyV1Elr//zd28e+P5LmjJYVQfnAwNysGs+J6zAHqKVOV2PPP7krRvMq1Bz9TOF2e8Ma65d4wnbWXO6sJK5imqBuYKzznoL/b0mYKzqepMPds3nhhXEQ7RirjVErZAzV0SR7Ho6nFGiqpf9zvCfr6GuPjd/6PMHlmdYyVxFtdz5zVc8a7DXXPkGukwNyFjzMvfsN12wQniIFsy1lqgVcuY6ZH6u/TRkJCeH/4z1Da3oGKoitpK5imq5+bpXP2uu4PovvnbzkiP/gi47FzLW/NTWv9Vx+5XfpYVwHy2Y69zdb3z94s579p+BviQlZAa7ZyXUXP022tBoFwbMJkl3WMlcRbVYcwW33fSqajKJMVCEjDU/6DPMrv/cYDhDVgj3Ubu5zj1ohNUj9z6w/wygmw27ngCDRLhnJdRc7WhObBlGXx9ZK5mrqBZmruCe7xw+ezssBjRQ8lIZZK5lmHscYSZnrqArqcnv3xraDcdW8Q71i/VB+6+/rsNK5iqqpctcHfgdWcVs3VygGlj9WMuyJHP92Inn023NzbEfOWPzxJNPbR2gHvnmyiYwt5m8Q22zANvw1wF9c8UyWIKTlcxVVMuQuTouOO/NRdpiEa1e/PyDL5TIz5LM9dApV9JtzUmtxgr55gpgsIgc777iu/v/tVW0IdXCXV1rurriMGDQfsQMrGSuolpQ/cvMlIFlc5ksBoZAxqr/IolyyFzzUbOxQtZchwgxVyzD1oVRT6ketpK5imphJjoETBbddlIkPSFSlanOj8w1D4cuuXpb4terWHMF7Lo7EPGydRww2JBuOQ4/e9hK5iqqBMMfMvOMAf1kL7/kjZsT/+ZtQREt2lNhqBjHVslK9YA2bna/5qZVcz386BOqGSRiSGPMta96N2TKORhmqMH6VcxWMldRJcgGZoY5BUS1aMd1XPzOd+9HpkBmWi+1DiJx7WdvoAVuH3ObKwbjf/Sxx7clff0aY64s8Ql0tbUyQg3Wj4StZK6iSjBgBDPIlNxw3DsPvFCiXlCrwJ6TOYFRsgK3j7nMFW2rNQ0OEaox5gpQvesmSUfbaEjEagk1WLe8lcxVVElMMtNYZK7tUOOsOC2YK/qu3nH3PduSvT2NNddUwGCHkpzcslYyV1EdmAmHmWFqZK7tUGPVcAvmivZVjLpUc0Zwn6aYK/qu+gNP4N9jIlhEwYh+/fvo8DOGrWSuCwUJQaUHWEhFaP/Wqdz0wbcfeIlE3aBLFHte5qIFc3XAZFuMYMeaa99ITTFtrz4wa7Sxuu2gyljZwisE0V/NM8l0kSJLOJRvf+bIAy+PqJvaoteWzNWBhKaWotgx5hrS13VMBOuAoWIfdgALK5nrQsGACjCQWga5D6VU1Apuv+BNB14OUT/oKsWemzkYY64Xn3413VZJkNz04EMPb0v7ujXGXEMGgsAybN0pWMlcF4rLtsUsMuz3GsnR/aaPH3z5NfQlEfWCLlOveRV/fkozxlzRN5ZtqzSoJm7BYJ9+4in6HHTRVx1sYetPwUrmulD8bFtEsWyZmkCEXSJD2Oe+vf2xl0TUDab5q6FrTsvmCloxWPYMdBEzfCFbfwpWs5pra1WWrYARiayRoC2TLVsDGD0JoynZYy7BdW97N31RRN3UYLCtmytowWDZ/WegLZRdc8aiq4VhAC1EVC2CRCZrIogKa/yYgbGi6toebym++cm30hdF1M/cBrsEcwW1Gyy794yQ6eYcdrL0FFjNZq5IXKmtPRCGjyxb9lsroPsNMxFQm8HiWOc0VvCjG19FXxTRBjDYw4/gz1duTvjAZ2jB3UeN5gpqnh2H3XdG12TqDDtdXAqsZjFXv7tFLX0xEUHBfGD67PdWYFGrTy0Giw+Z0m2sXVz7p39BXxbRBkhymiOL+Nj3nE4L7j5qNVdQ6yw57J4z2PVmwITZ+lOxmsVc/e4WmLWELVMafyzbVgdfCO0jClODubFt5AYfMbjn7LjmQv1dlwFmzylZTbw0cwU1DjTB7rUlpr11Sh/XPqyKm6tNtkFBz5Yrie0C0uLgC2OSgmByWI9tLwe4zrVEqz7IGr7mFcfRF0a0xcXPLxfFLtFcjzz+5Oqqh9l9tmD0JHa9LUPzuU7Bqqi5uqpXW7jN2c6JKlJ7PKC16HXsLDIw5JzXH9cRiWs1mqqPotdlgdGccg+XuERzBRiLuCax+2sJTWaKjVoxEpM/rjDM+eYPfZ4ua1XUXLsMAAU8Wz43XWYPcKxsnRoZamcNIaXJ4rpiW2MNfw4UvS6TnCa7VHNF9nBN0Su7r5YQc41ta+0bkALT2dnlrYqZ69DoO6WjVxjAUKbqXO2SMaQwVh98bGCbuB+h0TuWw7VCNfPc2b9TUPS6XGCyqaetW6q5gpqiV3Y/LUPmagfZHwLZxGw7Pjbj2KqIuaKgZoWZT+noNcSUYDS1DnSBj4NS4/BiP4yWjdRy7y2v2DfXq170ngMvjFgWyCxG4lOKIRSXbK41Ra/sPlqGzDW2OhiRKduOj60etspuriHG6kB0y7aRmphoDwYCI2PbmQtkBdfehtkKGF/45r85+sBLItYBkp9gtGOrjZdsruDW2+/cOsK8YvfOAqNj1xsRK5Kd2Dp9hPSZtaM8WWU11xhjBTCM3IlEY6pRazFYXJuW2jFrBqaqydIFDBbVxWO68CzdXE887fytI8wrdt8YmKfVXWckHiGatdPChRI6IIW/fass5gojGtsWiOpGts0UjD0mAIPNbfxd4Hq2kHHbAqj+lakKVA+f8LJpbbBLN1fw6GOPb11hPrH7l5sx2cdWyc0ViS2x/S0tOcYcnmKsDphbySQntPemOG7xTDawqn8FSDXYxBrMtYaqYXYPcxM6KAW66bh1rJKZK9oBEXWygm0MqbKHEfWlrkrFeeZKdEJ0jLbnJSULzQ0mRVc3G4FxiFPOBbsGcz3t3Mu2rjCfrjv6E/R+pgDVuujLyn5Dey27hz5Yxi1vNclcYTDofjE1Uu1iqsHi+HKaFEx2aiTrd2PJdR3XiqqAhWNqFTBjDeaKEZvm1i0npR+uEElOdu5XtNn6bah+G24frouPFTVXVMsiekI06oAB4O8AplKq/W9MFbFro2TbywWuCfaJ64TrZdtn3XXEB4O7hjLTfChaFQBtqxpEYhpzd8lJba59g0P4GcChQyq6bGQraq6ssJoTRJ8wJnbjfWCqMC+Z1npRtCocued7XYu53nvfA1tnmEcpzTVkcAi//2pI1TCSn7CsVRPm6oDJIupzUaAfDSLxR9m06+bOq16naFXsc8pL8s+QI3Mto5Tm2tUf1scfJjEka3gR5ipEF9/85FuffSHEuoGxMpNIjcy1jFKaa2gXG7c82mCHoleZq1gk6GKjamDhKGWsQOZaRinN1Z/hpg9/naFo1/V1tZK5imb50Y2v2lz3tncfeBHEeilprEDmWkYpzTVk5CU2e06fKbsB/K1krqJJMHyh2leFA7PeMGPIyRhzvfazN9Bt1cyDDz28dYZ5lNJc2T2xMHNF9bDtumOXtZK5iub4h0v/9MCDL9YNxgfOnbzEGGOuh065km6rVjA7ztwqba6uDZWB31wbLP7rTztnJXMVTSFjFZaUoy7FYgvmIVoz1xpGaCptrnYqOQYb1clK5iqaQRnBwpJj5KUYWOHcR2vmesfd92xdYT6VNteYSdV9rGSuogk06L6wYJAIZgglYYVzHy2Zaw1DH0IpzRVT0bH74vDHCo7FSuYqqkfGKhi5hjWMgRXQfbRkrrVMlp7SXIfGC8bvbL0QrGSuompkrIIxR3YwgxXQfbRirrVMlA6lNNeh4Q/9BKVYrGSuolq+/Zkj6UMsRA1RK2AFdB8tmCsyhGuYJN0ppbmCruj1W6dcQ5cPxUrmKqpEWcGii1qiVsAK6T5aMNe5B42wSm2urM/qlOpgh5XMVVSHjFX0ccwr5s0Q9vEL6BBqN9da2ll9pTZXAINFlxv0W+2aLD0WK5mrqAqMvMQeXCEccwwY0QUz0D5qNVdUBddorFAOc82BlcxVVAPGCtaQhqKPmqqEATPQPmo0V3S5mXuIwz7ddvrl9FnIBSJZEJvcZCVzFVWA2W2u/dO/oA+tEI65B42wMAPtozZzxQhMTzz51Lbkr1M/+tLuWL8MmKE/MD/6tPYNZWjxhzZ0YHuhJmslcxVVoNltRAi1ZAk7/II4hFrMFdFqDaMvhSjEXNGG2jVABJKX8Dtbz9HX/xWGO7Q+sJK5itlRX1YRSk3trYAVxn1gsH+2nVKgbfWa627elvRtKMRcEXWy6+3oM9ihvq8gZLxhK5mrmBVlBosYmGHMxZhZceYyV0SqN9x8W/VVwEwh5upXB3fRZbBoX2XL+7Bp6CxWMlcxG0hgYg+pEAxMLceMYy5aMNeWqn+7lMpcAevPKnMVi0IJTCKW2jKFW4lcURV86VXXVzXqUoxCzPW759xIrzfjxmPOObBuiLmGDOhvJXMVs3DTB99OH1AhupC5TufQJVc3Z7Ih5opp4mymbxdIfPLXRVUxW87ir8OwkrmK4tx+wZvowylEHzLXdCCSbaX9NcRcASJSds0ZN73v4MAUbBmLvzzDSuYqiqKBIsRY1OaaFrTH1jaOMFOouQKYJrvuFiQ3+esNzfMKUH3sr2OxkrmKoqg/q5gCM4m5aN1cHbV3zYkxVxBqsP46IQlRMldRLZpCTkylpn6uY8z16HedTLc1N2iLrVWx5gpQRdzXBmuzf/sGkXDIXEWVqNuNSEFNIzSNMVe2nVqo1WDHmCvA4BDMNPE32991aBAKIHMVVaLqYJGC9728nrGFl2auoEaDHWuuDhgpjBF0jdKE39j98pG5iupQdbBIxSkvkbnmprap56aaayjsfvl0GbPDSuYqsqLqYJGSmjKGl2quGHSipr6wpcy1L6lJg0iI6rjhuHfSB1GIsRx+BDeF0izVXAGmoqtFU8wV0Sayh9GmCmz/Vp++LGM2bKLFSuYqsqFB+UUOaml3XbK5glr6wI4xV5gqm58VdA3gj791ZRhjBCi7vMVK5iqygLGDNViEyMHpL5a5lqCW6DXWXIe64YCuSJRFr33Rro+VzFVk4ZuffCt9AIVIQQ1Vw7Hmesult9Lt1EwNba+h5orIM6S/KuhrQ4U5o/0V2xrKEPaxkrmK5CiJSeTkCy/+i8073vV6agYliTXXGy/6Ot1OzWAO2LkVYq4wVlT3suvOsMMfpsBK5iqSoyQmkYtLj3j75mMn/6fNBz7xn6gZlGQN5nriaedvXWE+hZjrt065hl7zLtAey7YzBSuZq0jKD778GvrgCTGVC456w+ajp/3HzUdO//f7vO6YeaPXNZgrmFsh5jrUxuqDZVlC01SsZK4iKZoAXeQAxupM1TF39LoWc33woYe3zjCPQsyVXe8uQhOUYrGSuYpkqOuNyMEZ73/hjrE63njcq6khlGAt5jp3l5yU5hrSX3UsVjJXkQR0vVHUKlLTZ6zgQ6f9P5vnv/nPqSnkJtZcr/3sDXQ7tTN3UlOIuYZMGZfTWIGVzFUkQeMHi9QMGavjuJP+GzWF3MSa66FTrqTbqZ2553v9xZ330OfDZ2jgfSQ8sfVSYiVzFZPRgBEiNaHG6njL+/6EGkNOZK5l9Mi9D9BnxMIGgEDyUq42VouVzFVMRlGrSEmssTpKZw+f8IHP7BTmfbRqrnNXC4eaK8Acrm4c4Zs/9HmaFYyhDP1qZBgwqoyxrl02BiuZq5iEolaRkrHG6ihpsDBL3zyHaNVc505oijHXIWCsXd128PeQMYS7sJK5ikncfsGb6IMmRCxTjdVRqop4LeY69xCIKc11KPFpSh9YK5mrmIQyhEUKUhmrA0lOubOIY831YyeeT7dTM0cef/LWFeZTKnMdSnpyoDqZrT+ElcxVjEb9WkUKUhurA910EMX+8RuOocYxlVhzRQIU207NHLrk6q0rzKdU5ho6qD+iW7cODPm759z47G/4d1fVsZXMVYxGYwiLqeQyVh+Y7NEfeOnmJX92NDWQsazBXO+4+56tK8ynFOaKqt6utlaLM1fMjsN+x3ZY8pOVzFWM4t5bXrHzcAkRQwljtWDIxD8/4YX7Ee2fHHXU5s2vPG7zvFc/Zyb4/9e86qDBdLF0c62hShhKYa6sm04Xrk9s3yw7bEAKK5mrGIXmaxVTmMNYLRe/fvcZ/uhL33PAbPtYurneevudW0eYVynMNWY6OheV9kW6LPHJSuYqRqHuN2IsNRjr2e952YFjOu+FxwdHrI4lm2stUSs01VxDE5mAH5H6ba0MOziFlcxVRHPnVa878FAJEUoNxor5YDHhujumE172HmowQ8SaK9tGrczdt9XXVHMNTWQCMGK33pAp2wnXrai5SpIkSZI0XjJXSZIkSUosmaskSZIkJZbMVZIkSZISS+YqSZIkSYklc5UkSZKkxJK5SpIkSVJiyVwlSZIkKbFkrpIkSZKUWDJXSZIkSUosmaskSZIkJZbMVZIkSZISS+YqSZIkSYklc5UkSZKkxJK5SpIkSVJiyVwlSZIkKbFkrpIkSZKUWDJXSZIkSUosmaskSZIkJZbMVZIkSZISS+YqSZIkSYklc5UkSZKkxJK5SpIkSVJiyVwlSZIkKbFkrpIkSZKUWDJXSZIkSUosmaskSZIkJdZizPXRxx7f3HvfA5tbb79zc811Nz/LaedetjnxtPOfxf/tjrvv2V9HkiRJklKqSXN98KGHNzfcfNvm0CVXb479yBmb//qa4yaD7WB7MGcYtSRJkiSNVRPm+sSTT+2bHqLQw48+gZpjao48/uTNpVddv2/ktQjH40fh4hly3SNsl+2vVvBx6NfMANTMgJY+GNm5ielMUWvvwhxYVWuuvqEy8ysJjBaRMo5pTuEGsuNbO7mq9rFdtr+WwbOM5wjGW2stDTtuMZ0pWuK7kBqr6swVLzu+wEtFqDHgmHBscxVIMleOzHUaeK7xEYsPyBrMlh2jmM4UyVyHsarGXHHzWjKPOUxW5sqRuaYF+QdzGi07JjGdKZK5DmM1u7m6unx2sC2A6rVS1cUyV47MNR+IaHNd3y6x4xDTmSK9C8NYzWauMCREf+wgWwPtWOjWk1syV47MNT94xtFGW0Js/2I6U6R3YRirWcwVVU41tqlOBV/5OaNYmStH5loOPIO5rrcT26+YzhTpXRjGqqi5wniWbhD4ws9V+MhcOTLX8qBbWK4PSbY/MZ0p0rswjFUxc0W16RKj1S4QnaeWzJUjc50HJD7l6GPM9iWmM0V6F4axKmKuSPphB7N00Kac8ute5sqRuc4HPphTt8Wy/YjpTJHehWGssporjKWGQSDmBF/3qQxW5sqRuc5PSoNl2xfTmSK9C8NYZTNXGEqqcX9bJ5XBylw5Mtc6SGWwbNtiOlOkd2EYqyzmKmPdJYXBylw5Mtd6SGGwbLtiOlOkd2EYq+TmKmPl4JpMlcyVI3Oti6lJTmybYjpTpHdhGKuk5ipj5aSIWiGZK0fmWhfojjbleWfbFNOZIr0Lw1glM1cZKwfZlKnGaJW5cmSu9YFExrFi2xPTmSK9C8NYJTPXpQxlmBIYa8p+gDJXjsy1TsbeF7YtMZ0p0rswjFUSc11rP9YhUo83LHPlyFzrZGyeAduWmM4U6V0Yxmqyueqic3IMci5z5chc62XMe8C2I6YzRXoXhrGaZK5oZ13TkIahYNzVHJK5cmSu9TImemXbEdOZIr0Lw1hNMte1j77EQNtzLslcOTLXuom9P2wbYjpTpHdhGKvR5or2RLaDNQPzyymZK0fmWjexH5xsG2I6U6R3YRirUeaq6uBdUvVl7ZPMlSNzrRuUFTFi2xDTmSK9C8NYjTLXFrrdwOxQbY1MZgvaRGFUqT4QsJ3cxgrJXDlLMle8W+yZDQHPe63PSEzmPFtfTGeKZK7DWEWbK/ptsg3XAAoXZCfGGB3OB3OvYlQZts0hYKw55rRkmqPgxD5rJ9f1n6NASfWh4J7rWvIiYpL82D3Oydh3fwrYJzuWnEzRHO/CHNdoClbR5oqNsAsxJ/jaTzEKEh6g2PNLVRiGaI5rv2a1bK6+8G7MXds0ts9rCSHqZ8ecE+yzJc3xLrR2jayizHWOC9wHvspTDS3oC1VYIV+zOfqy9knmWlZLMVcnbHvOXIlaJXMdlsw1XlHmWkvUigIit7Gharnva3+OGy9zLaulmSuEj9G5DDb3uY2VzHVYMtd4BZtrLW2tJds4IZi4LYxgunNI5lpWSzRXaK53uXRNT6hkrsOSucYr2FznbrMBJbq7MKEwctXEqIqeSzLXslqquUJzvM+1FpYy12HJXOMVZK4wNHbyJUH0OIexOrlq4jmPQeZaVks2V1QPs/3nZK4anyHJXIclc41XkLkipZ+dfClKVwXXKplrWS3ZXCHUBLFjyAWe3xolcx2WzDVeQeZa+iW0pJ66rVXJXMtq6eaKvqfsGHIhc30OmeswizfXuROZaq1KmkMy17JaurmWrpGSuT6HzHWYxZtr6a9bn7nbWWuTzLWslm6upc9P5vocMtdhFm+uc1YJ15q6P5dkrmUlc02LzPU5ZK7DLNpc58godKDri3RQMteykrmmReb6HDLXYRZtrogc2UmXQFHrrmSuZbV0cy39fstcn0PmOsyizXWuGTUUtXLJXMtq6eZa2lRkrs8hcx1m0eYaMnh9DpDFKO1K5lpWSzfX0h/PMtfnkLkOs1hznbO9NcdMN0uQzLWslm6udszs3Mw5dGifZK7DkrnGq9NcMXADO+Hc1Dzv49yaw1zxUtVC6W5Z2Ce7JjnBPktojve71sJS5jqsOd4FjHHg3v25GTNCYKe5zvHAAVUJd2sOc60JPOQlhf2x48hJqXOc41mq9d2WuQ5rjnehJsY0aXSa61wFucYQ7pbMVeaaQnMVlKXvX6hkrsOa65mphaTmOkcyE9qApG7JXGWuU4Wq9bkSFWsdbU3mOiyZa0JzZTvIzZgTWJNkrjLXKYK5zTXiWs3d62Suw5K5JjLXuQbrb+2BKy2Zq8x1rJCBP+dQpjVPwCFzHZbMNZG5znUhNbVcv2SuMtcxQkFeutuNpeYR12Suw5K5JjLXubrhlC48W5PMVeYaKkSqKMDnal+11Dy7lcx1WDLXROY6x8MGpH7JXGWuTFgHoKsLql9rMVRHrYNHOMlchzXHu1ATMteFS+a6fHNdIrVPwiFzHZbMtWFzVTecYclcZa6t0cJ7LXMdlsw1kbmiaontICdjDn5tkrnKXFujBRORuQ5L5prIXOcoxGWuw5K5ylxbAlFrzYlMTjLXYclcZa6LlsxV5toSrRiIzHVYMleZ66Ilc5W5tgIylluIWiGZ67BkrjLXRUvmKnNthZYGhJG5DkvmKnNdtGSuMtcWqHmoQyaZ67Bkrg2bq7riDEvmKnOtHYxf3Ep1sJPMdVgy10TmeulV19Md5Ebql8xV5loz+EBucT5mmeuwZK6JzHWOhw1I/ZK5ylxrpkVjhWSuw5K5Nm6urb6cpSRzlbnWSu1DHPZJ5josmWvj5lq68GxNMleZa22gKrj1qSJlrsOSuSYy17mmnMOsHlK3ZK4y15potY3VSuY6LJlrInOd60IikUrqlsxV5loLeBZbywruksx1WDLXROaKiZbZDnIz5gTWJJmrzLUGWjOGIclchyVzTWSuENtBCaRuzWGua5bM9SB4/paYdChzHdYc70LrH3Gd5orO4OyEc1M6OmlJMteykrk+B8qDpUrmOiyZa7w6zXWuKsjWL2hOyVzLSuZ6kKV++MpchyVzjVenuc7xwIElfyFPlcy1rGSuB1nquylzHZbMNV6d5jpXdxyAhCppVzLXspK57tLyYBFdkrkOS+Yar05znStjGLR+UXNJ5lpWMtddME/r0iRzHZbMNV6d5grhRWInnZslvsApJHMtK5krZ2kfvzLXYclc49Vrrqedexk96RIssfppqmSuZTVHgYJ9hgrzprJt5AYjMy1lAAlI5josmWu8es0VBsdOugSKXnclcy2r2s11zqabJY2mJnMdlsw1Xr3mOufLCxS9HpTMtaxqN1dorugVLCXxUOY6LJlrvHrNFZprMAmA6HVJ1U9TJXMtqxbMdc4PYDQbLUEy12HJXOM1aK6o/mEnXgoN5v+cZK5l1YK5QnNGr2OOtzbJXIclc43XoLliLFF24iWpZb5IRNGI5FGYzSGZa1m1Yq5zRq94JluXzHVYMtd4DZorNGfVMKhh3khnrO6Y5mgPlrmWVSvmCs0ZvbaeGyFzHZbMNV5B5opJzNnJl2ROg8V+2QdG6UJF5lpWLZnrnNFr65n9MtdhyVzjFWSuiNrYyZdmDoNFlTT2y44HjC0Mx0jmWlYtmSs0Z/TackEocx2WzDVeQeYKzfni+sDoSkSM+KAISeYqafgy17JqzVzxzPZ9COYE+8X+W5TMdVgy13gFm+vcfV4t6AaQq58dzDtm6EcULLmOxZfMtaxaM1doDqNwtFoYylyHJXONV7C5QnMU7n3A1HADUhlbrKn6oE0295e7zLWsWjTXOaNXUOIjM7VkrsOSucYrylznuMChIJKFOcYYHJZFmyqqvFMUSLkNdg5zxT5bIbVaNFdoDrNwzNVNbYpkrsOa411AoMPe81qxijJXaM7B/ENxNwUPMAO/5epelHPUGhw326d4htRq1Vznjl5TnENJoUxg55ET7LMlzfEutIZVtLnW1vZaI7m+3mWu/aRWq+YKzWEYDvYVX7NkrsOSuQ5jFW2u0JwvbivkGLZR5tpParVsrnNHr7WMqhYimeuwZK7DWI0yV7y4YxN/1kTqLkMy135Sq2Vzheb8CEb50IpkrsOSuQ5jNcpcIV3sMFIarMy1n9Rq3Vznjl4xslsLkrkOS+X9MFajzRWae8acFkDhlmqQCZlrP6nVurlCc0avePZzZs+nksx1WDLXYawmmSuUK+t2SaQyWJlrP6m1BHOdO3ptwURkrsOSuQ5jNdlckT0858vbCim+4mWu/aTWEswVmsM8fGofWELmOiyZ6zBWk80VQlTGdiYOMnWQCZlrP6m1FHOdO3qtfWAJmeuwZK7DWCUxVwiJO2yH4iBTkjxkrv2k1lLMFZo7ei09m1WMZK7DkrkOY5XMXCEZbD9TR2+SufaTWksy17mjVzy7tUrmOiyZ6zBWSc0VksFyUow7LHPtJ7WWZK7Q3NFrznObIpnrsGSuw1glN1dIBnuQVAP6y1z7Sa2lmevc0WutA0vIXIclcx3GKou5QjBYZRGnnSlH5tpPai3NXKG5o1eUC7VJ5josmeswVtnMFUISw5oNNnWWpMy1n9RaorlCcw5divIg1cdmKslchyVzHcYqq7lCeJHWaAo5Xh6Zaz+ptVRznbvZpjZjkbkOS+Y6jFV2c3Wa4wGeA3yZ55oRRObaT2ot1VyhuaPXmgaWkLkOS+Y6jFUxc4VQTbxkg8C55Sw0ZK79pNaSzXXu6LWmgSVkrsOSuQ5jVdRcnTCQwpLaYnEuJWYAkbn2k1pLNldo7mkja4leZa7DkrkOYzWLuUJoi8UD1rrJYmCIUoWEzLWf1Fq6uc4dveJ5rkEy12HJXIexms1cnVo1WRQMJQtCSObaT2ot3VyhuaPX0ufLJHMdlsx1GKvZzdUJJosv6blf9iEQqc5VIMhc+0mtNZjr3NEr+oHPLZnrsGSuw1hVY66+kPiEidhrMVocB45n7jYimWs/qbUGc4Xmfs9g8HNK5josmeswVlWaqy8YLR5EfOGyE8oFChxkNObqVjNGMHgYrOCklstuLwn2WVp4xtmxlALP9ZyCubPjysncHxSxmuNdaA2r6s3VCl9QMFtUz6Y0XGwLZoqs3zkKOEmSJGk5as5cmVBdC9PF1yCM12EjPf83mCjWkZFKkiRJqbUIc5UkSZKkmiRzlSRJkqTEkrlKkiRJUmLJXCVJkiQpsWSukiRJkpRYMldJkiRJSiyZqyRJkiQllsxVkiRJkhJL5ipJkiRJiSVzlSRJkqTEkrlKkiRJUmLJXCVJkiQpsWSukiRJkpRYMldJkiRJSiyZqyRJkiQllsxVkiRJkhJL5ipJkiRJiSVzlSRJkqTEkrlKkiRJUmLJXCVJkiQpsWSukiRJkpRYMldJkiRJSiyZqyRJkiQllsxVkiRJkhJL5ipJkiRJiSVzlSRJkqTEGjTXe+97YHPokqs3x37kjM1/fc1x+/+99KrrN088+dR2ic3m1tvv3Jx42vmbw48+YR/8G38LFZbFthnYn78vqwcfenh/n3a9I48/eXPauZftH3+I/GMIOfZHH3t8/zr41wXXKXR/TtiOvb74f/x9SP65Yx0m/B2/Yzks3yVcY3cMjJj72aWufcQ+MyHnbeXuF/bjuOHm23qfrS7hOPFsue3gGGLvO4TzwLr+vccxhtx7XzgHnAuOBdcF4Phi75m/HRyPe4dCtoNl3P3sou9elXhOcW5Ypmsf/rbx3zHPRpfwfOBa+seaeh9d8u+rI/Y5c/fHgm2FvkdYxr032D8T7p87xr7nwD1vuJdDz2fXsYOhdSH/+mGdUG/pNVdcAHswDndSfQ96yMNzx9330HV9uk7CL2T76HupndyFA/h3n3Deffu95rqbt0v2yz0gXQzdeOzHX57J/73vuPruIxi6JiEa2gcIeWZCzttX33XGfcQzGCIcV9854IUbOnYnew4+IQWGE96BvmPCfQs5JmwHhQbbBhjajv/+9NH1LvvL5HpOh5bDsfnLDBWeoeorR1Pto0tDZWSXyfnqO34Hnp0+M4TsM8/O3X+O+u5l6HJ9xuro09B70ffOd5pr38sfw1BhGbKfrgcw5hiHHqLQm2VfwC7wpdOnkI8K0FfI2vNn8n/H8l3yl2P0XZNQse0yYp+ZPg19wDiGCjkcz1DBDvCyDSn0uR0yfRxT34vvGLp32E7IR2rfdvz3p4+u6+wvk+s5HVrOvttDz0SIhu71kCFN0ZCxOvquNxR6b7GvmPeWXV9/X333MnQ5t0wfXQp9L7reeWquqC6wG8BLjJPo2xkKH1YA9d08e8GxD58+U7TrYt9uPVbo9FWDYB23HP7dJbtdXA+2P/y9b39d27HXF//f9cDa82fyf++7D/5y7l77pCgEuvYx9ZnpEns5uq4zjqdPdp+g69j7Pohi3q1Sx8S+7ru202U4eD6wDsBx++u4v/cdg798rufUXxfLWqU2V/b84ZhRpuEcQ2tMxgrn6O8b4G/2/oDQstG9Pw5/G6CvltA+r6XNlT0vOd8Laq6ubcLhXzA8MEMbtw8pTqpL9oLHaOhm2YvTZ9S4aG45/JsJL66/PVwH3/js/rqiV3t9/O3gv3Y7XQ9AyLXzf8fyXQpdbor69mEj+RTPDK6bv5xfhcOe476C2RZI/rJsP12KfbfYS+vkHxMKvb5j6nqmId8AhrYzJjIPkb98rufUX5ddD/te9l37ENlr59/r3LJlFZ4V30BtGdN3LfvKRuzHfkB0yT4X7Pr27ctX6HJuGRD7vEx9L6i5YkF/RVcgOdmCkD009uZ1yV7wGIXcLP8CTb1ZtmC0X564Tv7vXS+TPW7/pkF2O10PRci183/v2g4UutwUDe0j9TNjlxt6jrvO294P9pHmmyKeuS4NvVu2UOy7F/5yQ8cEmOz+hraDfw8p9P748pcPPee+5Zj8ddk7ntpc7XWw9xpCIY1jwXPBfh8rW/izsipV2Wj31XXd7PWo2Vzte8ECpaH3gppryIG73wE7aHshuxS6HFPpmxWyP//30O0w+b9jeaZU24FCl5uioX2EnA+Ucjn/967zxn0eWs5/fkCXQpbzf+86Jshfjj2LIecfcm4h2/EVuzzkLx96zn3LMfnrsnfTXgt2TWMUch3832PPp0923+xc/Gexq6yChpYLvW4lj8nJLQNirq89J3xAWPnHAKxkrluFLGe/0Fhk6v/etZ2Qc/Z/Z9cXSrUdKHS5KRraR8j5QCmX83/vOm/7orHl/OcHdClkOf/3rmOC/OXYsx9y/vYLHdWHNoIK2Y6v2OUhf/nQc+5bjslfl72b9j6zaxojex1Y9OP/Hns+fbL7zlk2hl63ksfk5JYBMdfXvheITO174R8DsJK5bhWynK1KAfaLxq/aZF87UMg5u+3Yun5fIdvxf8fyXQpdboqG9hFyPlDK5fzfu87bFh5sOSzjno2u5gDIf84Ak/971zFB/nLs2Q+9TvaYbPsRnr+Qc3MK3a8vf/nQc+5bjslfl73j9j6zaxojW0ADVLtjuw7/N1xb9/epVcT2HmCbViFlHjS0nD0Pti+o5DE5uWVA7PPi7wPYZ98Ptth7IXPdKnQ5207XZ35dmnLOvkK24/+O5bsUutwUDe0j9LqkXM7/veu8beHRtVyI/OcMMPm/9+3LX449+6HXCYku9qOxKxkvRKH79eUvH3rOsffBX5e94/a4u0wiRraNPZSQtu0+hZxLaJk3tJx9P7quW8ljcnLLgNjnhX0cdQVMTDLXrUKXg/D16ZYFrMqgT1PO2VfIdvzfsXyXQpeboqF9hF6XlMv5v3edty08upYLkf+cASb/9759+cuxZz/0OkH2oxHEfjQ6xezXyV8+9Jxj74O/rn3Hcf3sB0YKsa5XoUyRvQc5y0b7frB9QSWPycktA2KfF8gmscYEU9WZK/bnw7IXnea4WU5+phgIqS5zCr02Qxq6dsD/Hct3yV8O7W52O2MLWl/+PtixhF6XscvZcwL+713XxxYe/nJ4Pu02+75u7T6Z/N+7jgnyl2PPfuh1crIfjaz9NUSx+4X85UPPOfY59ddFIenWse8yiHmf+4Tt222HkDpyxfbc+Tr8jwn8f5fwW99y9v1gzyJkj6lEee2WAex5CYlEbe1DaDBVnbkyarpZTqwqLbTKIPTaDCnk2vlg+S6x5X1CrsmQ/O2xYwm9LmOXG6Lr+tjCwy1n/+7TJf8561rO/73rmCB/Ofbsh14nX9ZobPtriMbs118+9JwZfc8pW56B93pqmyfEqhVxbrhXDv+3nG2uQ/RdN/+ZZcvZ88D/M9ljYssN7cspdDm3TBe410PCvYAx++uFfHzJXLcKXc7X2PbX0GszpJBr54Plu8SW9wm9Jn3yt8eOJfS6jF1uiK7rYwsPt5z9u0+X/Oesazn/965jgvzl2LMfep18pWh/HbNff/nQc2b0PadseUZoZDIkex3mzBYeou+6+c8sW86+B+xZhOwxseWG9uUUupxbpo8QsQ+loWCq+mrhvs7Vc9wsqzHtr6HXZkhD1w74v2P5LvnLja0+GZK/D3Ysoddl7HJDdF0fW3j4y+ELFtfHGlKXsOzQcv7vXccE+cuxZz/0Olmx9teuApNpzH795UPPOVW1sI1KQN8xhCrkOvi/p9ink933ELgOXcJvfcvZ96PrWbHHxJYb2pdT6HJuGTC1XMOy/vaGgiklNG0VuhyTrUobqjKYcs6+Qrbj/47luxS63BQN7SP0uqRczv+967xt4cGW858f0KWQ5fzfu44J8pdjz37odWKyH40oSEKjuTH79ZcPPee+5Zj8de07juvnfyDh31MVch3832PPp0923znLRvt+sH1BJY/JyS0DUlxflO3+NvuCKZnrVqHLMcW2v045Z18h2/F/x/JdCl1uiob2EXpdUi7n/9513rbwYMv5zw/oUshy/u9dxwT5y7FnP/Q6dcl+NIa+F2P26y8fes59yzH567JzsR8UXSYRqpDr4P8eez59svvOWTba96PrupU8Jie3DEhxfWGkocGUzHWrkOVwYVFNjd/xIvqyDxjMFqbLFHLOMGfsB3RVPYRsx/8dy3cpdLkpGtpHyPlAKZfzf+86b3tv2XL+8wO6FLKc/3vXMUH+cuzZD71OeJZxXLYJZmzSXuh+ffnLh55z33JM/ro4Xyt7n9k1jVHIdfB/jz2fPtl9s3Pxn0V2PZyGlgu9biHH5FfRTzkmJ7cMiL2+7r2wZT3KY/te2LGbIZnrViHLDe3P/m5vilPIOfu/Y3mmVNuBQpeboqF9hJwPlHI5//eu87aFB1vOf35Al0KW83/vOibIX449+yHnP3Rutv217x1yCtmvlb986Dn3Lcfkr8vOw14Ldk1jFHId/N9jz6dPdt/sXPxnse++Di0Xet3scjbis793lZ9Q6LH724u5vkPnZNtf2THIXLcKWS5kf/7vodth8n/H8kyptgOFLjdFQ/sIOR8o5XL+713nbV80tpz//IAuhSzn/951TJC/HHsWQ84/9twQWQwpZL9W/vKh59y3HJO/Lns37bVg1zRGIdfB/z32fPpk983Oxb+v7Ho4DS0Xet1QK2IjPr92z1a3smjQKfTY/e3FXN+Qc/KPF/+2Wry5pqxmCNmf/3tXH0G7HVbt6/+O5ZlCrp3/e9d2oNDlpmhoHzZZoEsh5w2luj4oAIaW858f0KWQ5fzfu44J8pdj3Tzw/PnLMNlChO3P347MNVz2OrDEF//32PPpk903O5eQMg8aWs5GcX3XDdGov6wrI+02mFn5Cj12f5sx19c+C6w5xDdX9l4s2lzt0FV9Wbwh52wvuF3OHg+7LpCtarMmbE2mq59hyLXzf+86Hih0uSnq24dtx+grxEPOG8p1fXBsfkGJY/d/73p+IP85A0z+733HZK9X3zH1FVb+ckPb6fpg9BV6f3z5y4feh77lmPx12T2y73efSYQopPzxf489nz7Ze8DOxX8WQ59ZuxyeFd9kQJ+wPGuvxHPn/23o2oceu7/NKc+LfS+GynCoOnPF//v0VQ3YdfHwuvX8r21H3w0LvVn2IcD/Y3/++o6uhCb2gLnt2AcVTEmM8n/H8l3yl8O5YFkf/8Eaq6594L7Z69HX3oJ1/GW7FLKc/zuW75L94HH3C8dpj73rYwiyzwmT/3vMMeE4cDz276DvmGK2U1NCU+xzate1Sm2utsYD2GP2f8P/p5Lddipzdc89YM9+yMeX/eiw2+g7FqfQY7fbdcfuYLWGTtZD3HvBvIW9F9WZK6PrAoSs6xi66aE3y36xdIGb0Cf7gHXBrq1TyDX2f+/blr8cI+SlGRLbLgMPcV8hGXLeUMrrg4LSFgIMFD59x+4/Z4DJ/z3FMeGDre+YYrYTotD748tfvu+c/eUYfc+pvxx7x1ObK2SvRR995x0ru99U5toHnqE+s/KF94RtA3QFE75Cj93fLgPH0SWcy5T3YnZzDTGrroc81OiGChco9GZBtn3Agi/+ELHIwGdoOyHX2P+97+UdeoiGrkmIQh7UkBc09NlKeX2goZcNL+rQsfvPGWDyf596THj2h44JCtnO0DvkFHp/fPnL953z0DPU95wOLZfDXKGh99wxdK9jZO/BFHNlkZoF9yWkVsOpq+wOLTtDjx3Prb99Rp9wTmPfC2quvnl0RWDuoLFj9vL6LyuW7VOfWWEbfV8yfetiv33VYb5wnm69kIcED6t96LC/kHV9se3gYemrDnfyH9Cuh9J/sfu2ifvl7ikj9ryY+vaB+4xjDSnA/fPui1RCro87nq7n2ArPIrblnm3gqspCjt1/Xms5JqdU2wm57lb+OfeZml+uMPqeUxxL33I4f7dtHE/MOQ8J16Tr2Xf0vZ+x8q9T17n4ZV5fTRvW7Tp27APXta+M7pIt9/CshV7z0PK679hByPM59r2g5ipJkiRJ0njJXCVJkiQpsWSukiRJkpRYMldJkiRJSiyZqyRJkiQllsxVkiRJkhJL5ipJkiRJiSVzlSRJkqTEkrlKkiRJUmLJXCVJkiQpsWSukiRJkpRYMldJkiRJSiyZqyRJkiQllsxVkiRJkhJL5ipJkiRJiUXN1Z/vTgghhBD9WMlchRBCiIlYyVyFEEKIiVjJXIUQQoiJWCmhSZIkSZISS+YqSZIkSYklc5UkSZKkxJK5SpIkSVJiyVwlSZIkKbFkrpIkSZKUWDJXSZIkSUosmaskSZIkJZbMVZIkSZISS+YqSZIkSYklc5UkSZKkxJK5SpIkSVJiyVwlSZIkKbFkrpIkSZKUWDJXSapQDz708Obe+x7Y54knn9r+Nb2wbbcf7FNKK3dtwRK0tPPJKZkr0a2337k58bTzg7n0quu3aw4Ly7JtMHAcMbLr2+NC4WmXAY8+9vh2iX51rd9FTGEdcs1PO/eyzQ033xZ8vFOEfbBjYEbHlj10ydXbX8OFAgvnyOaKPPL4k/fPPYVwDtdcd/P+Nu1+Dj/6hP1jn2K0Ic849oF7nvPDwSrm3QOx75/THXff03kfj/3IGaO3C4W8g6nfE/dusvPBvmS0XDJXIhQ87EHqAg9eqLoeUgaOI0Z2fXtceLHtMjEfBniJ7Pp9xLx0sdc89tqMETMfFJxWKHzscrHmivtgt8HAPZxifFiXnRdj7DWOecZh5lPOJ0YxxwVizx8fCrjvbFsWHMuYD4vYd3DKewJzZmUGI6YcWYtkrkSxBb01sT7FvOCxL4Zd3z8udk54cWJUk7mCKQVHiJjhsUKELcdMuEuhxuqAIY0pmFFYYl22zS7GRMuxJoZjShVl9Sn2uGKfr65otYvY9w+KfQfBmHsY+6yUuoctSeZKFFvQx7yEc5grIgP725iXoTZzxTnkrFaEQdp9+h8sTuzrPvS4xhSWAAV5rGLNxRH7nIzZz5hq9FjFHldM9S2rvQgh9h0f87zgPYlVaMTqiPmYXItkrkQoTPAQW9jLGfv1ybbB9gViCzW7XewLYi/KmHYfHJPdDrZtj9sRY3zMXP2CBy8vq87EfnKKfb37wjna32OMj0U72Ke7P3gGuiKimOcD14ltA9vGdnAeiHDY+cYaH3vGndx+7O9jDCBW+Mj0n08HO1737oSqq/3amQ72w95DLBPznmA7dhv2PWH3EOuFqutDAc+Kq8LHM+Pe2TFlyRokcw0Ue6hBbHtRX8EzVXa72BczrbFRArsGsYVQl4bMFWIv/ZgqrxgxY/MLKnZNYo7JrgtsYYXClxXe9vr0iVU9sw/DFMYX8oznfA9ixO4fzjfmw4XVDAFbNuA+MuOLifrY8drngL1L/jM7JPYRgPeAKeY6rU0y10CxBy6mcHNihQoefEusaUN2uyiQ7cuMv8V8KfvCcfnbAl2Ra+w+WIFgry8r+HN/NTND94+LHXdogYPrZNfF/WFi5x7zYcOeu65rx4w85nkMMU72PpVW10dL7AcbuzddZsQSnlg7fpfYM2PfE/YhhfVChGti1wUy0XjJXAPEClD21R8iVvAwYgpOJ7YdS2zB4Yu92F2EvsxO7Br7hQa2N7W6a4xYYePfG3s/u8yRiRl3V6HMrn3Mvuy6oKvAHIrWhzRkrsyMYs4llUKj+SGxZ7frPUOUapeNedfZcxDynoR+HLHt+8eHZwb7Y8Q8I2uQzHVAXVU+Yx8kVvAwYl44J7YdC168lJFrF7HXBy8n204fOJcS6ouybEEWE4Wwc8bfumSXBaGKWZcdV8xHWegz7hNz3VKo61keU2PErlfX88/2O9Vch4j5cGHb9+9N3/77nt01SuY6IFawTikI5jZXMPb4Y15sLBsjVkANEdNWNUUs0kIhzD68Yo6JnXNfAWWXBaGKWZcdV05znfLBN0Zd1cFjzYFdr67nn71Duc11apuuzHWcZK49Yi8NXsopBUEN5grGfKHHvNhYNkbsWveRu63VFzNRmI2t1o2NpFm1cNd9Z9e+pWrhLnAOY57FKWLP2thmHohtr+tjJOaeM8W8gyD2PWHb94+vb/8y14OSuXaIFaggppBhYgVPKtntOtg+Y15op6EXb4pizLV0pAPZSAeJKTY5pau9tEvsenYZNIueY649ewa6Cl5b1Q1iDJDtq4vSBXLXez3F4Nm9iUloirkGfeZmGRMI4IOLbcttx29zte9E6XtZu2SuHUpdHezECp5UstsFOOauarDUX7VThBfTbtu9rOyalX6RbaGI58Ne09jrCfnrd22n6/7FVNXiObDrs2iNGUVsRN71jHcV3F0RdA7leK9DDRv3kX24pOqKw56RMe8J2w6efyt7n0u/k7VL5kqEh8R/aABeCrwEeLgtU7/q2TYdMbLbxUvivjhRYNvfcU7u9xDheOw2UFjZY3bEFJrsmruXFduyv+HYSxbKuPf2GCxjjodFMjg3Z7DYJqumBTH76zIAbNttB8bKCn9WsPapy1whdr6x2x+rnO81M21sG9uBsK2uZWKE7dltuPeEPaPYfsw7DrHrBPCsuGuCZ8aejzsO6RnJXI3w0LACpg8UJqFiBU8fMbLr2uNi+455IdiL3UfMttkL7a/Pjr1Uoexk9++DgmaM8Lyx7Q0x5txjnz2AdyHGxCG2H6eu840xsjHq+rjow74/fQr5+GLEGlKfuUIp3pOuCHuI2HNZumSuRrEGAmJewtgCLkZ2XXtcXecWWnjOaa5d+85dKPvqiiDBlKpFdu59wMhjoxFoTKE5pqqbPeO+WPQa8w6NUew1BrHHxM6rjzHnPGSuU99xpzEfC/5xSDLXHcUaCIh5SVjB00eM7LrsuKYUbHOaK8SuXcy1nypWte6YavLs/BljjdUJx8mqJxljjBVi98lXV/SK5yuXQq+vz5hnK9Rgse0x93HIXCF2/bsSrPoEgw39GMMzFWvgS5fM1Wjp5ooXgL0weJGGNLe5du2/VPTaZQq4nimE8+uKjtF+HpPA1CcU6ri2LHEFwCCmFJTsGbdiJoTjySX2fA0R81776ruPMKGxHy0QewdyvidDzwrOc8r5LFkyV0mqUCgIUUiCKZHqkGCibj+lPlLWJHdtc9/HEsLx++cj9UvmKkmSJEmJJXOVJEmSpMSSuUqSJElSYslcJUmSJCmxZK6SJEmSlFSbzf8P5L/ON27RoiwAAAAASUVORK5CYII="
}
