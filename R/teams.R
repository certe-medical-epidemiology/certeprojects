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

#' Send Teams Messages
#'
#' Send messages to Microsoft Teams from R. This function relies on the `httr` package and only requires [Incoming Webhooks](https://docs.microsoft.com/en-us/microsoftteams/platform/webhooks-and-connectors/how-to/add-incoming-webhook#add-an-incoming-webhook-to-a-teams-channel) to be set in Teams.
#' @param message message to send, supports markdown. Can also be a [data.frame], which will be transformed using [certestyle::plain_html_table()].
#' @param channel channel to send message to, run [teams_webhooks()] for list of valid channels
#' @param title title for message
#' @param subtitle subtitle for message
#' @param items text items for in message, a named vector
#' @param images URLs of images to add to card. Local images will be uploaded to Imgur.com, or will will be transformed using [base64enc::base64encode()] if that fails.
#' @param items_title title to show above items
#' @param images_title title to show above images
#' @param buttons buttons to show on card. Like `items`, this should be a named vector where the values are URLs (starting with `http://` or `https://`) or mail links (starting with `mailto:`)
#' @param message_thumbnail image to show on card
#' @param markdown turn markdown support on/off
#' @param webhooks_file the file locations of a YAML file with MS Teams webhooks in the form `channel: "URL"`
#' @param ... not used at the moment
#' @details
#' \if{html}{
#' \out{<div style="text-align: center">}\figure{teams.png}{options: style="width:1422px;max-width:55\%;"}\out{</div>}
#' \out{<div style="text-align: center">}\figure{teams2.png}{options: style="width:1372px;max-width:55\%;"}\out{</div>}
#' }
#' @importFrom yaml read_yaml
#' @importFrom httr POST warn_for_status add_headers upload_file content
#' @importFrom xml2 as_list read_xml
#' @importFrom base64enc base64encode
#' @importFrom certestyle colourpicker plain_html_table
#' @export
teams <- function(message = "",
                  channel,
                  title = NULL,
                  subtitle = NULL,
                  items = NULL,
                  items_title = NULL,
                  images = NULL,
                  images_title = NULL,
                  buttons = NULL,
                  message_thumbnail = NULL,
                  markdown = TRUE,
                  webhooks_file = read_secret("teams.webhooks.file"),
                  ...) {
  
  if (!file.exists(webhooks_file)) {
    warning("Webhooks file not found: ", webhooks_file, call. = FALSE)
    webhooks <- NULL
  } else {
    webhooks <- read_yaml(file = webhooks_file)
    channel <- channel[1]
    channel <- gsub("^#", "", channel)
    if (!channel %in% names(webhooks)) {
      stop("No webhook for channel '", channel, "'. Create it in your Teams channel.")
    }
  }
  
  if (!is.null(items)) {
    items_list <- list(list(name = names(items[1]),
                            value = gsub("\n", "<br>", as.character(items[1]))))
    if (length(items) > 1) {
      for (i in 2:length(items)) {
        items_list <- c(items_list,
                        list(list(name = names(items[i]),
                                  value = gsub("\n", "<br>", as.character(items[i])))))
      }
    }
  } else {
    items_list <- ""
  }
  
  if (is.data.frame(message)) {
    message <- plain_html_table(message, max_col = Inf)
  }
  
  sections_list <- list(markdown = markdown,
                        activityText = gsub("\n", "<br>", message),
                        activityTitle = ifelse(is.null(title), "", gsub("\n", "<br>", title))) # ActivityTitle cannot be omitted
  if (!is.null(subtitle)) sections_list <- c(sections_list, list(activitySubtitle = gsub("\n", "<br>", subtitle)))
  if (!is.null(message_thumbnail)) sections_list <- c(sections_list, list(activityImage = message_thumbnail))
  
  if (is.null(title) &
      is.null(subtitle) &
      is.null(items) &
      is.null(items_title) &
      is.null(images) &
      is.null(message_thumbnail)) {
    
    # plain text, no titles etc.
    message <- paste0(message, collapse = "\n")
    
    if (length(message) == 0 || all(message == "")) {
      warning("Nothing to send to Teams...", call. = FALSE, immediate. = TRUE)
      return(invisible())
    }
    
    if (!is.null(webhooks)) {
      tryCatch(
        warn_for_status(
          out <- POST(url = webhooks[[channel]],
                      encode = "json",
                      body = list(text = message))
        ),
        error = function(e) message("Error in Teams: ", e$message))
    } else {
      return(message)
    }
    
  } else {
    # also includes title / subtitle / ...
    
    message <- paste0(message, collapse = "\n")
    
    if (identical(message, "")) {
      message <- " "
    }
    if (!is.null(images)) {
      if (any(images %unlike% "^http")) {
        for (i in seq_len(length(images))) {
          if (images[i] %unlike% "^http") {
            if (!file.exists(images[i])) {
              stop("File not found: ", images[i])
            }
            imgur_clientid <- read_secret("imgur.clientid")
            imgur_clientsecret <- read_secret("imgur.clientsecret")
            image_name <- basename(images[i])
            message("Uploading '", image_name, "' to imgur.com...", appendLF = FALSE)
            error_upload <- FALSE
            images[i] <- tryCatch({
              upload_imgur <- POST("https://api.imgur.com/3/image.xml",
                                   config = add_headers(Authorization = paste("Client-ID", imgur_clientid)),
                                   body = list(image = upload_file(images[i])))
              stop_for_status(upload_imgur)
              message("OK")
              result <- content(upload_imgur, as = "raw")
              as_list(read_xml(result))[[1]]
            }, error = function(e) {
              # Imgur failed, try to generate base64
              message("ERROR - trying to generate base64 version")
              base64enc::base64encode(images[i])
            })
            if (nchar(images[i]) > 21000) {
              warning("Image ", i, " too big to send via Teams: '", image_name, "'", call. = FALSE)
              images[i] <- paste0("data:image/png;base64,", question_mark_image())
            }
          }
          # add as buttons to open images
          if (is.null(buttons)) {
            buttons <- stats::setNames(images[i], paste0(i, "e afbeelding"))
          } else {
            buttons <- c(buttons,
                         stats::setNames(images[i], paste0(i, "e afbeelding")))
          }
        }
      }
      images_list <- lapply(images, function(l) stats::setNames(as.list(l), "image"))
    } else {
      images_list <- ""
    }
    
    body_lst <- list("@type" = "MessageCard",
                     "@context" = "http://schema.org/extensions",
                     themeColor = colourpicker("certeroze"),
                     summary = message,
                     sections = list(
                       sections_list, # = title, subtitle, message_thumbnail, markdown
                       list(
                         title = ifelse(is.null(items_title), "", items_title),
                         facts = items_list,
                         markdown = markdown
                       ),
                       list(
                         title = ifelse(is.null(images_title), "", images_title),
                         images = images_list)))
    
    # add buttons (name is text, value is URL)
    if (!is.null(buttons)) {
      buttons_list <- list(list("@type" = "OpenUri",
                                name = names(buttons[1]),
                                targets = list(
                                  list(os = "default",
                                       uri = as.character(buttons[1])))))
      if (length(buttons) > 1) {
        for (i in 2:length(buttons)) {
          buttons_list <- c(buttons_list,
                            list(list("@type" = "OpenUri",
                                      name = names(buttons[i]),
                                      targets = list(
                                        list(os = "default",
                                             uri = as.character(buttons[i]))))))
        }
      }
      body_lst <- c(body_lst,
                    list(potentialAction = list(
                      list(
                        "@type" = "ActionCard",
                        name = "Acties",
                        actions = buttons_list))))
    }
    
    if (!is.null(webhooks)) {
      tryCatch(
        warn_for_status(
          out <- POST(url = webhooks[[channel]],
                      encode = "json",
                      body = body_lst)
        ),
        error = function(e) message("Error in Teams: ", e$message))
    } else {
      return(body_lst)
    }
  }
  
  # sleep for 1/4 seconds, only 4 messages per second can be sent:
  # https://docs.microsoft.com/en-us/microsoftteams/platform/webhooks-and-connectors/how-to/connectors-using#transactions-per-second-thresholds
  Sys.sleep(0.25)
  
  return(invisible(out))
}

#' @rdname teams
#' @importFrom yaml read_yaml
#' @export
teams_webhooks <- function(webhooks_file = read_secret("teams.webhooks.file")) {
  if (!file.exists(webhooks_file)) {
    stop("File not found: ", webhooks_file, call. = FALSE)
  }
  sort(names(read_yaml(file = webhooks_file)))
}

question_mark_image <- function() {
  paste(c(
    "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",
    "UuRRykvk09oAAAAASUVORK5CYII="),
    collapse = "")
}
