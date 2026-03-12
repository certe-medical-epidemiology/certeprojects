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

#' Send a File Validation Request by Email
#'
#' Sends an HTML email with three action buttons (view file, validate, reject)
#' to request file validation from a team member. The validate and reject
#' buttons are hyperlinks to **Power Automate HTTP-triggered flows** hosted
#' by Microsoft, which patch the file's SharePoint metadata via the SharePoint
#' REST API. No custom server is needed.
#'
#' @param drive_item a Drive Item object (`ms_drive_item`) of the file to
#'   validate
#' @param email_to email address of the recipient who must validate the file
#' @param validate_flow_url Power Automate HTTP trigger URL for the *validate*
#'   flow. Defaults to `read_secret("validation.flow.validate_url")`.
#' @param reject_flow_url Power Automate HTTP trigger URL for the *reject*
#'   flow. Defaults to `read_secret("validation.flow.reject_url")`.
#' @param from sender email address or shared mailbox. Defaults to
#'   `read_secret("department.mail")`.
#' @param teams a Teams account, e.g. outcome of [connect_teams()], used for
#'   the Microsoft Graph API OAuth token.
#' @details
#' ## How it works
#'
#' 1. Three Microsoft Graph API calls resolve the SharePoint context needed by
#'    the Power Automate flows: the SharePoint site URL, the document library's
#'    list GUID, and the numeric list item ID of the file.
#' 2. These identifiers are appended as query parameters to the Power Automate
#'    trigger URLs — the same URL already contains a Microsoft-issued SAS
#'    signature that prevents unauthorised triggering.
#' 3. An HTML email with three styled buttons is sent via
#'    `POST /users/{from}/sendMail` (Microsoft Graph API).
#' 4. When the recipient clicks **Validate** or **Reject**, their browser opens
#'    the corresponding Power Automate URL. The flow patches the SharePoint
#'    list item fields and returns an HTML confirmation page.
#'
#' ## One-time Power Automate setup
#'
#' Create two instant flows in Power Automate
#' (<https://make.powerautomate.com>), one for validate and one for reject.
#' Use the JSON definitions in
#' `system.file("power_automate", package = "certeprojects")` as a starting
#' point (paste into the flow's **Code view**). Each flow:
#'
#' - **Trigger**: "When a HTTP request is received" (GET method)
#' - **Action**: "Send an HTTP request to SharePoint" with parameters
#'   `site_url`, `list_id`, and `list_item_id` read from the query string
#' - **Response**: returns an HTML confirmation page
#'
#' Store the trigger URLs (available after saving the flow) as secrets:
#' - `validation.flow.validate_url`
#' - `validation.flow.reject_url`
#'
#' ## SharePoint fields updated
#'
#' The flows patch the following fields on the SharePoint list item:
#' - **Validate**: `Gevalideerd` (UTC timestamp), `ValidationStatus`
#'   `"Gevalideerd"`
#' - **Reject**: `ValidationStatus` `"Afgewezen"`
#'
#' @importFrom httr GET POST add_headers stop_for_status content
#' @seealso [validate_file()], [authorise_file()]
#' @export
send_validation_email <- function(drive_item,
                                   email_to,
                                   validate_flow_url = read_secret("validation.flow.validate_url"),
                                   reject_flow_url   = read_secret("validation.flow.reject_url"),
                                   from              = read_secret("department.mail"),
                                   teams             = connect_teams()) {
  if (!inherits(drive_item, "ms_drive_item")) {
    stop("`drive_item` must be a Drive Item (ms_drive_item)")
  }
  if (is.null(validate_flow_url) || validate_flow_url == "") {
    stop("No validate flow URL configured. Set `validation.flow.validate_url` secret or pass `validate_flow_url`.")
  }
  if (is.null(reject_flow_url) || reject_flow_url == "") {
    stop("No reject flow URL configured. Set `validation.flow.reject_url` secret or pass `reject_flow_url`.")
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

  # Resolve SharePoint context needed by the Power Automate flows -------------
  # 1. SharePoint site URL (e.g. "https://certenl.sharepoint.com/sites/MedEpi")
  res_site <- GET(
    paste0("https://graph.microsoft.com/v1.0/sites/", site_id, "?$select=webUrl"),
    config = graph_headers
  )
  stop_for_status(res_site, task = "get SharePoint site URL")
  site_url <- content(res_site, as = "parsed")$webUrl

  # 2. SharePoint list GUID for the document library
  res_drive <- GET(
    paste0("https://graph.microsoft.com/v1.0/drives/", drive_id),
    config = graph_headers
  )
  stop_for_status(res_drive, task = "get drive SharePoint IDs")
  list_id <- content(res_drive, as = "parsed")$sharepointIds$listId

  # 3. Numeric list item ID (used by the SharePoint REST API)
  res_item <- GET(
    paste0("https://graph.microsoft.com/v1.0/drives/", drive_id,
           "/items/", item_id, "/listItem?$select=id"),
    config = graph_headers
  )
  stop_for_status(res_item, task = "get SharePoint list item ID")
  list_item_id <- content(res_item, as = "parsed")$id

  # Build action URLs ---------------------------------------------------------
  # Power Automate trigger URLs already contain a Microsoft SAS signature.
  # We append our SharePoint identifiers as additional query parameters.
  action_params <- paste0(
    "&site_url=",      URLencode(site_url,      reserved = TRUE),
    "&list_id=",       URLencode(list_id,        reserved = TRUE),
    "&list_item_id=",  URLencode(list_item_id,   reserved = TRUE),
    "&user=",          URLencode(email_to,        reserved = TRUE)
  )
  validate_url <- paste0(validate_flow_url, action_params)
  reject_url   <- paste0(reject_flow_url,   action_params)

  # HTML email body ------------------------------------------------------------
  # Table-based layout with inline styles for maximum email client support.
  html_body <- paste0(
    '<!DOCTYPE html>
<html>
<head><meta charset="utf-8"></head>
<body style="margin:0;padding:0;font-family:Calibri,Arial,sans-serif;font-size:14px;color:#333333;">
<div style="max-width:600px;margin:0 auto;padding:24px;">

  <p>Beste collega,</p>
  <p>Het volgende bestand staat klaar voor uw validatie:</p>

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
      <td><hr style="border:none;border-top:1px solid #e0e0e0;margin:4px 0;"></td>
    </tr>
    <tr>
      <td style="padding:12px 0 4px 0;">
        <a href="', validate_url, '"
           style="display:inline-block;padding:10px 20px;background-color:#107c10;
                  color:#ffffff;text-decoration:none;font-weight:bold;
                  font-size:13px;border-radius:4px;">
          &#10003;&nbsp; Valideren
        </a>
        &nbsp;&nbsp;
        <a href="', reject_url, '"
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

  # Send via Microsoft Graph API ----------------------------------------------
  body <- list(
    message = list(
      subject = paste0("Validatieverzoek: ", file_name),
      body    = list(contentType = "HTML", content = html_body),
      toRecipients = list(list(emailAddress = list(address = email_to)))
    ),
    saveToSentItems = TRUE
  )

  res <- POST(
    url    = paste0("https://graph.microsoft.com/v1.0/users/",
                    URLencode(from, reserved = FALSE), "/sendMail"),
    encode = "json",
    config = graph_headers,
    body   = body
  )
  stop_for_status(res, task = paste0("send validation email for '", file_name, "'"))
  message("Validatieverzoek per e-mail verstuurd voor '", file_name, "' naar ", email_to, ".")
  invisible(TRUE)
}
