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

#' Send Validation or Authorisation Request by Email
#'
#' These functions send an HTML email with interactive action buttons to request
#' file validation or authorisation from a team member. In Microsoft Outlook the
#' buttons are rendered as an **Outlook Actionable Message** (inline card that
#' updates without opening a browser). In all other mail clients a standard HTML
#' fallback with hyperlink buttons is shown.
#'
#' Clicking **Valideren** / **Autoriseren** or **Afwijzen** triggers a
#' **Power Automate HTTP flow** hosted by Microsoft, which patches the file's
#' SharePoint metadata using the SharePoint REST API (`ValidateUpdateListItem`).
#' No custom server is needed.
#'
#' @param drive_item a Drive Item object (`ms_drive_item`) of the file
#' @param email_to email address of the recipient (validator or authoriser)
#' @param action_flow_url Power Automate HTTP trigger URL for the
#'   *validate* or *authorise* flow.  Defaults to
#'   `read_secret("validation.flow.validate_url")` or
#'   `read_secret("validation.flow.authorise_url")` respectively.
#' @param reject_flow_url Power Automate HTTP trigger URL for the *reject*
#'   flow. Defaults to `read_secret("validation.flow.reject_url")`.
#' @param from sender email address or shared mailbox. Defaults to
#'   `read_secret("department.mail")`.
#' @param teams a Teams account, e.g. outcome of [connect_teams()], used for
#'   Graph API calls.
#' @details
#' ## Required secrets
#'
#' | Secret key | Value |
#' |---|---|
#' | `validation.flow.validate_url` | PA trigger URL for the validate flow |
#' | `validation.flow.authorise_url` | PA trigger URL for the authorise flow |
#' | `validation.flow.reject_url` | PA trigger URL for the reject flow |
#' | `validation.actionable_message.originator` | Registered originator GUID (optional but required for Outlook interactive cards) |
#' | `department.mail` | Sender shared mailbox |
#'
#' ## SharePoint fields updated by the flows
#'
#' | Button | Fields patched |
#' |---|---|
#' | Valideren | `Gevalideerd` (UTC timestamp), `Gevalideerddoor` (person lookup ID) |
#' | Autoriseren | `Autorisatie` (UTC timestamp), `Autorisatiedoor` (person lookup ID) |
#' | Afwijzen | *(none — flow returns a rejection confirmation page only)* |
#'
#' The person lookup ID for `email_to` is resolved at send time via
#' [get_sharepoint_lookup_id()], matching exactly how [validate_file()] and
#' [authorise_file()] set these fields when called directly from R.
#'
#' ## Outlook Actionable Messages (one-time admin setup)
#'
#' To enable the inline interactive card in Outlook, register an originator in
#' the **Actionable Email Developer Dashboard**
#' (<https://outlook.office.com/connectors/oam/publish>) using the
#' *Organization* scope (requires tenant admin approval — no Microsoft review
#' needed). Store the generated GUID as
#' `read_secret("validation.actionable_message.originator")`.
#'
#' Without registration the `<script>` tag is still included in the email and
#' the HTML fallback buttons are always present for all mail clients.
#'
#' ## Power Automate flows (one-time setup)
#'
#' Create three instant flows in Power Automate
#' (<https://make.powerautomate.com>). Use the JSON definitions in
#' `system.file("power_automate", package = "certeprojects")` as a starting
#' point (paste into the flow's **Code view**, configure the SharePoint
#' connection). Each flow accepts both GET (HTML fallback links) and POST
#' (Actionable Message buttons). Store the trigger URLs as secrets.
#'
#' @seealso [validate_file()], [authorise_file()]
#' @name validation_authorisation_email
#' @rdname validation_authorisation_email
#' @importFrom httr GET POST add_headers stop_for_status content
#' @importFrom jsonlite toJSON
#' @export
send_validation_email <- function(drive_item,
                                   email_to,
                                   action_flow_url = read_secret("validation.flow.validate_url"),
                                   reject_flow_url = read_secret("validation.flow.reject_url"),
                                   from            = read_secret("department.mail"),
                                   teams           = connect_teams()) {
  send_action_email(
    drive_item      = drive_item,
    email_to        = email_to,
    action_flow_url = action_flow_url,
    reject_flow_url = reject_flow_url,
    from            = from,
    teams           = teams,
    action_type     = "validate"
  )
}

#' @rdname validation_authorisation_email
#' @export
send_authorise_email <- function(drive_item,
                                  email_to,
                                  action_flow_url = read_secret("validation.flow.authorise_url"),
                                  reject_flow_url = read_secret("validation.flow.reject_url"),
                                  from            = read_secret("department.mail"),
                                  teams           = connect_teams()) {
  send_action_email(
    drive_item      = drive_item,
    email_to        = email_to,
    action_flow_url = action_flow_url,
    reject_flow_url = reject_flow_url,
    from            = from,
    teams           = teams,
    action_type     = "authorise"
  )
}

# Internal workhorse shared by send_validation_email() and send_authorise_email().
send_action_email <- function(drive_item, email_to, action_flow_url,
                               reject_flow_url, from, teams, action_type) {
  if (!inherits(drive_item, "ms_drive_item")) {
    stop("`drive_item` must be a Drive Item (ms_drive_item)")
  }
  if (is.null(action_flow_url) || action_flow_url == "") {
    stop("No action flow URL configured. Set the appropriate secret or pass `action_flow_url`.")
  }
  if (is.null(reject_flow_url) || reject_flow_url == "") {
    stop("No reject flow URL configured. Set `validation.flow.reject_url` or pass `reject_flow_url`.")
  }

  file_name <- drive_item$properties$name
  site_id   <- drive_item$properties$parentReference$siteId
  drive_id  <- drive_item$properties$parentReference$driveId
  item_id   <- drive_item$properties$id
  web_url   <- drive_item$properties$webUrl

  graph_headers <- add_headers(
    Authorization  = paste(teams$token$credentials$token_type,
                           teams$token$credentials$access_token),
    `Content-type` = "application/json"
  )

  # -- Resolve SharePoint context (3 Graph API calls) -------------------------

  res_site <- GET(
    paste0("https://graph.microsoft.com/v1.0/sites/", site_id, "?$select=webUrl"),
    config = graph_headers
  )
  stop_for_status(res_site, task = "get SharePoint site URL")
  site_url <- content(res_site, as = "parsed")$webUrl

  res_drive <- GET(
    paste0("https://graph.microsoft.com/v1.0/drives/", drive_id),
    config = graph_headers
  )
  stop_for_status(res_drive, task = "get drive SharePoint IDs")
  list_id <- content(res_drive, as = "parsed")$sharepointIds$listId

  res_item <- GET(
    paste0("https://graph.microsoft.com/v1.0/drives/", drive_id,
           "/items/", item_id, "/listItem?$select=id"),
    config = graph_headers
  )
  stop_for_status(res_item, task = "get SharePoint list item ID")
  list_item_id <- content(res_item, as = "parsed")$id

  # Person lookup ID for the recipient — matches get_sharepoint_lookup_id()
  # used by validate_file() / authorise_file()
  certe_login    <- sub("@.*", "", email_to)
  user_lookup_id <- get_sharepoint_lookup_id(site_id, certe_login, teams)

  # -- Parameters sent to the PA flows ----------------------------------------
  # Flows accept identical params whether triggered via GET (HTML fallback link)
  # or POST (Actionable Message button press from Outlook).

  params_list <- list(
    site_url       = site_url,
    list_id        = list_id,
    list_item_id   = list_item_id,
    user_lookup_id = user_lookup_id,
    file_name      = file_name
  )
  params_json <- toJSON(params_list, auto_unbox = TRUE)  # POST body

  qs <- paste0(                                          # GET query string
    "&site_url=",       URLencode(site_url,       reserved = TRUE),
    "&list_id=",        URLencode(list_id,         reserved = TRUE),
    "&list_item_id=",   URLencode(list_item_id,    reserved = TRUE),
    "&user_lookup_id=", URLencode(user_lookup_id,  reserved = TRUE),
    "&file_name=",      URLencode(file_name,       reserved = TRUE)
  )
  action_url_get <- paste0(action_flow_url, qs)
  reject_url_get <- paste0(reject_flow_url,  qs)

  # -- Copy depending on action type ------------------------------------------

  if (action_type == "validate") {
    action_label  <- "Valideren"
    action_color  <- "#107c10"
    action_symbol <- "&#10003;"
    subject       <- paste0("Validatieverzoek: ",   file_name)
    intro_text    <- paste0("Het volgende bestand staat klaar voor uw <strong>validatie</strong>:")
    success_color <- "107C10"
    success_title <- "\u2713 Gevalideerd"
    success_text  <- paste0("Het bestand **", file_name, "** is succesvol gevalideerd.")
  } else {
    action_label  <- "Autoriseren"
    action_color  <- "#0078d4"
    action_symbol <- "&#9733;"
    subject       <- paste0("Autorisatieverzoek: ", file_name)
    intro_text    <- paste0("Het volgende bestand staat klaar voor uw <strong>autorisatie</strong>:")
    success_color <- "0078D4"
    success_title <- "\u2605 Geautoriseerd"
    success_text  <- paste0("Het bestand **", file_name, "** is succesvol geautoriseerd.")
  }

  # -- Outlook Actionable Message (MessageCard) --------------------------------
  # Outlook replaces the card content with this JSON after a successful POST.
  refresh_card <- toJSON(list(
    `@type`    = "MessageCard",
    `@context` = "http://schema.org/extensions",
    themeColor = success_color,
    title      = success_title,
    text       = success_text
  ), auto_unbox = TRUE)

  originator <- read_secret("validation.actionable_message.originator")

  message_card <- list(
    `@type`    = "MessageCard",
    `@context` = "http://schema.org/extensions",
    themeColor = "0078D4",
    summary    = subject,
    title      = subject,
    sections   = list(list(
      activityTitle    = file_name,
      activitySubtitle = paste0("Afzender: ", from),
      markdown         = TRUE
    )),
    potentialAction = list(
      list(
        `@type`  = "OpenUri",
        name     = "Bestand bekijken",
        targets  = list(list(os = "default", uri = web_url))
      ),
      list(
        `@type`         = "HttpPOST",
        name            = action_label,
        target          = action_flow_url,
        body            = as.character(params_json),
        bodyContentType = "application/json"
      ),
      list(
        `@type`         = "HttpPOST",
        name            = "Afwijzen",
        target          = reject_flow_url,
        body            = as.character(params_json),
        bodyContentType = "application/json"
      )
    )
  )

  if (!is.null(originator) && nzchar(originator)) {
    message_card[["originator"]] <- originator
  }

  message_card_json <- toJSON(message_card, auto_unbox = TRUE)

  # -- HTML email body --------------------------------------------------------
  html_body <- paste0(
    '<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <script type="application/ld+json">
', message_card_json, '
  </script>
</head>
<body style="margin:0;padding:0;font-family:Calibri,Arial,sans-serif;font-size:14px;color:#333333;">
<div style="max-width:600px;margin:0 auto;padding:24px;">

  <p>Beste collega,</p>
  <p>', intro_text, '</p>

  <table cellpadding="0" cellspacing="0" border="0" style="width:100%;margin:16px 0;">
    <tr>
      <td style="background:#f0f4f8;border-left:4px solid #0078d4;padding:12px 16px;border-radius:2px;">
        <strong style="font-size:15px;">', file_name, '</strong>
      </td>
    </tr>
  </table>

  <p>Gebruik onderstaande knoppen om het bestand te bekijken en uw beslissing te registreren:</p>

  <table cellpadding="0" cellspacing="0" border="0">
    <tr>
      <td style="padding:4px 0 12px 0;">
        <a href="', web_url, '"
           style="display:inline-block;padding:10px 20px;background-color:#0078d4;
                  color:#ffffff;text-decoration:none;font-weight:bold;
                  font-size:13px;border-radius:4px;">
          &#128196;&nbsp; Bestand bekijken
        </a>
      </td>
    </tr>
    <tr>
      <td><hr style="border:none;border-top:1px solid #e0e0e0;margin:8px 0;"></td>
    </tr>
    <tr>
      <td style="padding:12px 0 4px 0;">
        <a href="', action_url_get, '"
           style="display:inline-block;padding:10px 20px;background-color:', action_color, ';
                  color:#ffffff;text-decoration:none;font-weight:bold;
                  font-size:13px;border-radius:4px;">
          ', action_symbol, '&nbsp; ', action_label, '
        </a>
        &nbsp;&nbsp;
        <a href="', reject_url_get, '"
           style="display:inline-block;padding:10px 20px;background-color:#c50f1f;
                  color:#ffffff;text-decoration:none;font-weight:bold;
                  font-size:13px;border-radius:4px;">
          &#10007;&nbsp; Afwijzen
        </a>
      </td>
    </tr>
  </table>

  <p style="margin-top:32px;font-size:11px;color:#888888;border-top:1px solid #eeeeee;padding-top:12px;">
    Deze e-mail is automatisch gegenereerd door certeprojects.<br>
    Reageer niet op dit bericht.
  </p>

</div>
</body>
</html>'
  )

  # -- Send via Microsoft Graph -----------------------------------------------
  res <- POST(
    url    = paste0("https://graph.microsoft.com/v1.0/users/",
                    URLencode(from, reserved = FALSE), "/sendMail"),
    encode = "json",
    config = graph_headers,
    body   = list(
      message = list(
        subject      = subject,
        body         = list(contentType = "HTML", content = html_body),
        toRecipients = list(list(emailAddress = list(address = email_to)))
      ),
      saveToSentItems = TRUE
    )
  )
  stop_for_status(res, task = paste0("send ", action_type, " email for '", file_name, "'"))
  message("Verzoek tot ", action_type, " per e-mail verstuurd voor '", file_name, "' naar ", email_to, ".")
  invisible(TRUE)
}
