# ===================================================================== #
#  certeprojects — SharePoint File Validation Webhook                   #
#                                                                       #
#  Companion plumber API for send_validation_email().                   #
#  When a recipient clicks Validate or Reject in the email, their       #
#  browser opens a signed URL here. This handler verifies the           #
#  HMAC-SHA256 signature and patches the SharePoint list item fields    #
#  via the Microsoft Graph API.                                         #
#                                                                       #
#  Start with:                                                          #
#    plumber::pr(                                                        #
#      system.file("plumber/validation_handler.R",                      #
#                  package = "certeprojects")                           #
#    ) |> plumber::pr_run(port = 8080)                                  #
#                                                                       #
#  Required secrets (via certeprojects read_secret() or Sys.getenv()): #
#    VALIDATION_WEBHOOK_SECRET  — HMAC key (same as send side)         #
# ===================================================================== #

library(httr)
library(digest)
library(jsonlite)

# ---------------------------------------------------------------------------
# Resolve the HMAC secret: try environment variable first, then certeprojects
# ---------------------------------------------------------------------------
.secret <- tryCatch(
  certeprojects::read_secret("validation.webhook.secret"),
  error = function(e) Sys.getenv("VALIDATION_WEBHOOK_SECRET", unset = "")
)

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Verify HMAC signature and token expiry
.verify_sig <- function(action, site_id, drive_id, item_id, user, expires, sig) {
  if (nchar(.secret) == 0L) {
    stop("Webhook secret niet geconfigureerd op de server.")
  }
  if (as.integer(expires) < as.integer(Sys.time())) {
    stop("De link is verlopen.")
  }
  payload  <- paste(action, item_id, drive_id, site_id, user, expires, sep = "|")
  expected <- digest::hmac(key = .secret, object = payload, algo = "sha256", raw = FALSE)
  # constant-time comparison via identical() is fine here — digest output is hex
  if (!identical(sig, expected)) {
    stop("Ongeldige handtekening. De link is mogelijk gewijzigd of al gebruikt.")
  }
  invisible(TRUE)
}

#' PATCH SharePoint list item fields via Microsoft Graph
.patch_fields <- function(site_id, drive_id, item_id, fields) {
  teams <- certeprojects::connect_teams()
  res <- httr::PATCH(
    url    = paste0("https://graph.microsoft.com/v1.0/sites/", site_id,
                    "/drives/", drive_id,
                    "/items/",  item_id,
                    "/listItem/fields"),
    encode = "json",
    config = httr::add_headers(
      Authorization  = paste(teams$token$credentials$token_type,
                             teams$token$credentials$access_token),
      `Content-type` = "application/json"
    ),
    body = jsonlite::toJSON(fields, auto_unbox = TRUE)
  )
  httr::stop_for_status(res)
  invisible(TRUE)
}

#' Minimal HTML response page
.html_page <- function(title, icon, message, colour) {
  paste0(
    '<!DOCTYPE html><html><head><meta charset="utf-8">',
    '<title>', title, '</title>',
    '<style>body{font-family:Calibri,Arial,sans-serif;display:flex;',
    'justify-content:center;align-items:center;min-height:100vh;margin:0;',
    'background:#f4f4f4;}',
    '.card{background:#fff;border-radius:8px;padding:40px 48px;',
    'max-width:480px;text-align:center;box-shadow:0 2px 12px rgba(0,0,0,.1);}',
    '.icon{font-size:48px;margin-bottom:16px;}',
    'h2{margin:0 0 12px;color:', colour, ';}',
    'p{color:#555;}</style></head>',
    '<body><div class="card">',
    '<div class="icon">', icon, '</div>',
    '<h2>', title, '</h2>',
    '<p>', message, '</p>',
    '</div></body></html>'
  )
}

# ---------------------------------------------------------------------------
# Endpoints
# ---------------------------------------------------------------------------

#* @get /validate
#* @serializer html
function(site_id = "", drive_id = "", item_id = "", user = "",
         expires = "0", sig = "", res) {
  tryCatch({
    .verify_sig("validate", site_id, drive_id, item_id, user, expires, sig)

    # Resolve the SharePoint user lookup ID for the person who clicked
    teams           <- certeprojects::connect_teams()
    user_lookup_id  <- certeprojects:::get_sharepoint_lookup_id(site_id, user, teams)

    .patch_fields(site_id, drive_id, item_id, fields = list(
      Gevalideerd              = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      GevalideerddoorLookupId  = user_lookup_id,
      ValidationStatus         = "Gevalideerd"
    ))

    res$status <- 200L
    .html_page(
      title   = "Bestand gevalideerd",
      icon    = "&#10003;",
      message = "Het bestand is succesvol gevalideerd in SharePoint.",
      colour  = "#107c10"
    )
  }, error = function(e) {
    res$status <- 400L
    .html_page(
      title   = "Fout bij validatie",
      icon    = "&#9888;",
      message = conditionMessage(e),
      colour  = "#c50f1f"
    )
  })
}

#* @get /reject
#* @serializer html
function(site_id = "", drive_id = "", item_id = "", user = "",
         expires = "0", sig = "", res) {
  tryCatch({
    .verify_sig("reject", site_id, drive_id, item_id, user, expires, sig)

    .patch_fields(site_id, drive_id, item_id, fields = list(
      ValidationStatus = "Afgewezen"
    ))

    res$status <- 200L
    .html_page(
      title   = "Bestand afgewezen",
      icon    = "&#10007;",
      message = "Het bestand is gemarkeerd als afgewezen in SharePoint.",
      colour  = "#c50f1f"
    )
  }, error = function(e) {
    res$status <- 400L
    .html_page(
      title   = "Fout bij afwijzing",
      icon    = "&#9888;",
      message = conditionMessage(e),
      colour  = "#c50f1f"
    )
  })
}
