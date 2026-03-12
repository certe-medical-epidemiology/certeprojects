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
#' Sends an HTML email with three action buttons to request file validation
#' from a team member. The **Validate** and **Reject** buttons are signed
#' hyperlinks that, when clicked, call a companion plumber webhook which
#' updates the file's SharePoint metadata via the Microsoft Graph API.
#'
#' @param drive_item a Drive Item object (`ms_drive_item`) of the file to validate
#' @param email_to email address of the recipient who must validate the file
#' @param base_url base URL of the validation webhook server, e.g.
#'   `"https://your-server.certe.nl:8080"`. Defaults to
#'   `read_secret("validation.webhook.url")`.
#' @param secret HMAC-SHA256 secret key for signing action URLs. Must match the
#'   secret configured in the companion plumber handler. Defaults to
#'   `read_secret("validation.webhook.secret")`.
#' @param from sender email address or shared mailbox. Defaults to
#'   `read_secret("department.mail")`.
#' @param teams a Teams account, e.g. outcome of [connect_teams()]. Used for
#'   the Microsoft Graph API OAuth token.
#' @param expires_in link expiry in seconds (default: 7 days = `604800`).
#' @details
#' ## How it works
#'
#' 1. `send_validation_email()` generates two **HMAC-SHA256 signed URLs** —
#'    one for *validate*, one for *reject* — embedding `site_id`, `drive_id`,
#'    `item_id`, the recipient's email, and an expiry timestamp. The signature
#'    prevents tampering.
#' 2. An HTML email with three buttons is sent via
#'    `POST /users/{from}/sendMail` (Microsoft Graph API).
#' 3. When the recipient clicks a button their browser opens the signed URL on
#'    the plumber webhook server (see `inst/plumber/validation_handler.R`).
#' 4. The webhook verifies the signature, checks expiry, and patches the
#'    SharePoint list item fields — reusing the same Graph API call as
#'    [validate_file()].
#'
#' ## Webhook server
#'
#' Start the companion webhook with:
#' ```r
#' plumber::pr(
#'   system.file("plumber/validation_handler.R", package = "certeprojects")
#' ) |> plumber::pr_run(port = 8080)
#' ```
#' The server must be reachable from the recipient's browser at `base_url`.
#' Set the `VALIDATION_WEBHOOK_SECRET` environment variable (or
#' `validation.webhook.secret` secret) to the same value on both sides.
#'
#' ## SharePoint fields updated
#'
#' - **Validate**: sets `Gevalideerd` (UTC timestamp) and
#'   `GevalideerddoorLookupId` (SharePoint user lookup ID of the recipient).
#' - **Reject**: sets `ValidationStatus` to `"Afgewezen"`.
#'
#' @importFrom httr POST add_headers stop_for_status
#' @importFrom digest hmac
#' @seealso [validate_file()], [authorise_file()]
#' @export
send_validation_email <- function(drive_item,
                                   email_to,
                                   base_url   = read_secret("validation.webhook.url"),
                                   secret     = read_secret("validation.webhook.secret"),
                                   from       = read_secret("department.mail"),
                                   teams      = connect_teams(),
                                   expires_in = 7 * 24 * 3600) {
  if (!inherits(drive_item, "ms_drive_item")) {
    stop("`drive_item` must be a Drive Item (ms_drive_item)")
  }
  if (is.null(base_url) || base_url == "") {
    stop("No webhook base URL configured. Set `validation.webhook.url` secret or pass `base_url`.")
  }
  if (is.null(secret) || secret == "") {
    stop("No webhook secret configured. Set `validation.webhook.secret` secret or pass `secret`.")
  }

  file_name <- drive_item$properties$name
  site_id   <- drive_item$properties$parentReference$siteId
  drive_id  <- drive_item$properties$parentReference$driveId
  item_id   <- drive_item$properties$id
  web_url   <- drive_item$properties$webUrl

  # HMAC-signed, expiring action URLs -----------------------------------------
  # The signature covers: action | item_id | drive_id | site_id | user | expires
  # This prevents any modification of parameters (including swapping validate/reject).
  expires <- as.integer(Sys.time()) + as.integer(expires_in)

  make_action_url <- function(action) {
    payload <- paste(action, item_id, drive_id, site_id, email_to, expires, sep = "|")
    sig <- digest::hmac(key = secret, object = payload, algo = "sha256", raw = FALSE)
    paste0(
      base_url, "/", action,
      "?site_id=",  URLencode(site_id,   reserved = TRUE),
      "&drive_id=", URLencode(drive_id,  reserved = TRUE),
      "&item_id=",  URLencode(item_id,   reserved = TRUE),
      "&user=",     URLencode(email_to,  reserved = TRUE),
      "&expires=",  expires,
      "&sig=",      sig
    )
  }

  validate_url  <- make_action_url("validate")
  reject_url    <- make_action_url("reject")
  expires_label <- format(
    as.POSIXct(expires, origin = "1970-01-01", tz = "Europe/Amsterdam"),
    "%d-%m-%Y"
  )

  # HTML email body ------------------------------------------------------------
  # Uses table-based layout and inline styles for broad email client support.
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
      <td style="padding:4px;">
        <hr style="border:none;border-top:1px solid #e0e0e0;margin:4px 0;">
      </td>
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
    De knoppen zijn geldig tot <strong>', expires_label, '</strong>.
    Reageer niet op dit bericht.
  </p>

</div>
</body>
</html>'
  )

  # Send via Microsoft Graph API -----------------------------------------------
  body <- list(
    message = list(
      subject = paste0("Validatieverzoek: ", file_name),
      body    = list(
        contentType = "HTML",
        content     = html_body
      ),
      toRecipients = list(
        list(emailAddress = list(address = email_to))
      )
    ),
    saveToSentItems = TRUE
  )

  res <- POST(
    url    = paste0("https://graph.microsoft.com/v1.0/users/",
                    URLencode(from, reserved = FALSE),
                    "/sendMail"),
    encode = "json",
    config = add_headers(
      Authorization  = paste(teams$token$credentials$token_type,
                             teams$token$credentials$access_token),
      `Content-type` = "application/json"
    ),
    body = body
  )
  stop_for_status(res, task = paste0("send validation email for '", file_name, "'"))
  message("Validatieverzoek per e-mail verstuurd voor '", file_name, "' naar ", email_to, ".")
  invisible(TRUE)
}
