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

#' Schedule Task (CRON-like)
#'
#' This will start an expression if requirements are met, using a CRON-like syntax (<https://cron.help>).
#' @param minute one or more values between 0-59, or `.` or `"*"` for each minute
#' @param hour one or more values between 0-23, or `.` or `"*"` for each hour
#' @param day one or more values between 1-31, or `.` or `"*"` for each day
#' @param month one or more values between 1-12, or `.` or `"*"` for each mpnth
#' @param weekday one or more values between 0-7 (Sunday is both 0 and 7; Monday is 1), or `.` or `"*"` for each weekday
#' @param user logged in user, must correspond with `Sys.info()["user"]`. Currently logged in user is "\Sexpr{certeprojects:::get_current_user()}". This can be any length > 1 if `check_sent_project` is set.
#' @param expr expression to run
#' @param log a [logical] to indicate whether this message should be printed: *Running scheduled task at...*
#' @param ref_time time to use for reference, defaults to [Sys.time()]
#' @param check_sent_project a project number to check if a certain project had a mail sent on the date of `ref_time`.
#' @param sent_delay delay in minutes. This will be multiplied by the position of the current user in `user` minus 1. For example, when `sent_delay = 15`, this will be `15` for user 2, and `30` for user 3.
#' @param sent_account Outlook account, to search sent mails
#' @param sent_folder Name of Outlook folder, to search sent mails
#' @details
#' The Windows Task Scheduler must be set up to use this function. Most convenient is to:
#' 
#' 1. Create an \R file such as `R_cron.R` with calls to [schedule_task()]
#' 2. Create a batch file such as `R_cron.bat` that runs `R_cron.R` with `R CMD BATCH`
#' 3. Set up a Task Scheduler task that runs `R_cron.bat` every minute
#' @export
#' @importFrom certestyle format2 colourpicker
#' @examples
#' something_to_run <- function() {
#'   1 + 1
#' }
#'
#' # units:      M  H  d  m  wd
#' schedule_task(., ., ., ., ., "user", something_to_run()) # every minute
#' schedule_task(0, ., ., ., ., "user", something_to_run()) # start of each hour
#' schedule_task(0, 7, ., ., ., "user", something_to_run()) # everyday at 7h00
#' schedule_task(0, 7, 1, ., ., "user", something_to_run()) # first day of month at 7h00
#' schedule_task(0, 7, ., 2, ., "user", something_to_run()) # everyday day in February at 7h00
#' schedule_task(0, 7, ., ., 1, "user", something_to_run()) # every Monday at 7h00
#' schedule_task(0, 7, 1, 2, ., "user", something_to_run()) # every 1st of February at 7h00
#' schedule_task(0, 7, ., 2, 1, "user", something_to_run()) # every Monday in February at 7h00
#' schedule_task(0, 7, 1, 2, 1, "user", something_to_run()) # each February 1st if it's a Monday at 7h00
#' schedule_task(0, 7,29, 2, ., "user", something_to_run()) # once every 4 years at 7h00
#' 
#' # examples of combinations
#' 
#' # everyday at 7h00 and 7h30
#' schedule_task(c(0, 30), 7,        .,       .,              ., "user", something_to_run())
#' # everyday at 7h00 and 15h00
#' schedule_task(0,        c(7, 15), .,       .,              ., "user", something_to_run())
#' # everyday at 7h00 and 7h30 and 15h00 and 15h30
#' schedule_task(c(0, 30), c(7, 15), .,       .,              ., "user", something_to_run())
#' # every second Monday of the month at 7h00:
#' schedule_task(0,        7,        c(8:14), .,              1, "user", something_to_run())
#' # every 15th of April at 8h30 and 16h30:
#' schedule_task(30,       c(8, 16), 15,      4,              ., "user", something_to_run())
#' # once per quarter at 8h00 on the first day of the month:
#' schedule_task(0,        8,        1,       c(1, 4, 7, 10), ., "user", something_to_run())
#' 
#' # fall-back for failed jobs
#' 
#' # this will run at 8h00 if current user is "user1"
#' # it will run again:
#' # - if current user is "user2"
#' # - if project 123 has no mail in Sent Items
#' # - at default 15 minutes later (so, 8h15)
#' schedule_task(0, 8, ., ., ., c("user1", "user2"), expr = something_to_run(), check_sent_project = 123)
schedule_task <- function(minute, hour, day, month, weekday,
                          user,
                          file,
                          project_number,
                          log = TRUE,
                          ref_time = Sys.time(),
                          account = connect_planner(),
                          check_sent_project = NULL,
                          sent_delay = 15,
                          sent_account = connect_outlook(),
                          sent_to = read_secret("mail.error_to"),
                          log_folder = read_secret("projects.log_folder")) {
  
  if (missing(user)) {
    stop("username not set with `user`")
  }
  
  if (!is.null(check_sent_project) && length(user) == 1) {
    stop("`user` must at least be length 2 if `check_sent_project` is set")
  }
  if (is.null(check_sent_project) && length(user) != 1) {
    stop("`user` must be length 1 if `check_sent_project` is not set`")
  }
  user <- as.character(user)
  if ((is.null(check_sent_project) && !get_current_user() == user[1]) || (!is.null(check_sent_project) && !get_current_user() %in% user)) {
    return(invisible())
  }
  
  if (deparse(substitute(minute)) == ".") minute <- "."
  if (deparse(substitute(hour)) == ".") hour <- "."
  if (deparse(substitute(day)) == ".") day <- "."
  if (deparse(substitute(month)) == ".") month <- "."
  if (deparse(substitute(weekday)) == ".") weekday <- "."
  
  # translate dots
  if (any(minute %in% c(".", "*"))) {
    minute <- c(0:59)
  }
  if (any(hour %in% c(".", "*"))) {
    hour <- c(0:23)
  }
  if (any(day %in% c(".", "*"))) {
    day <- c(1:31)
  }
  if (any(month %in% c(".", "*"))) {
    month <- c(1:12)
  }
  if (any(weekday %in% c(".", "*"))) {
    weekday <- c(0:6)
  }
  
  # force numbers
  minute <- as.integer(minute)
  hour <- as.integer(hour)
  day <- as.integer(day)
  month <- as.integer(month)
  weekday <- as.integer(weekday)
  weekday[weekday == 7] <- 0
  
  backup_user <- FALSE
  if (!is.null(check_sent_project) && get_current_user() != user[1]) {
    backup_user <- TRUE
    minute <- minute + (which(user == get_current_user()) - 1) * sent_delay
    # if the minute-delay went over the 59th minute, set hour to 1 later
    if (length(minute) == 1 & length(hour) > 1) {
      minute <- rep(minute, length(hour))
    }
    if (length(minute) > 1 & length(hour) == 1) {
      hour <- rep(hour, length(minute))
    }
    if (length(minute) > 1 & length(hour) > 1) {
      hour <- rep(hour, length(minute))
    }
    hour[minute > 59] <- hour[minute > 59] + 1
    minute[minute > 59] <- minute[minute > 59] - 60
  }
  
  if ("lubridate" %in% rownames(utils::installed.packages())) {
    # round time, since Windows Task Scheduler is sometimes 1-10 seconds off
    rounded_time <- lubridate::round_date(ref_time, unit = "1 minute")
  } else {
    rounded_time <- ref_time
  }
  rounded_time <- as.POSIXlt(rounded_time)
  
  minute <- formatC(minute, flag = 0, width = 2)
  hour <- formatC(hour, flag = 0, width = 2)
  
  if (length(minute) > 1 && length(hour) > 1) {
    hourminute <- sort(unname(unlist(lapply(hour, function(h) paste0(h, minute)))))
  } else {
    hourminute <- paste0(hour, minute)
  }
  if (any(hourminute == format2(rounded_time, "HHMM")) &
      any(day == rounded_time$mday) &
      any(month == rounded_time$mon + 1) & # "mon" is month in 0-11
      any(weekday == rounded_time$wday)) {
    
    if (!is.null(check_sent_project) && get_current_user() != user[1] && "certemail" %in% rownames(utils::installed.packages())) {
      if (is.null(check_sent_project) || !is.numeric(check_sent_project)) {
        stop("`check_sent_project` must be a project number, a numeric value")
      }
      mail_sent <- tryCatch(certemail::mail_is_sent(project_number = check_sent_project,
                                                    date = as.Date(ref_time),
                                                    account = sent_account,
                                                    sent_items = sent_folder),
                            error = function(e) {
                              message("Could not determine whether mail of project ", check_sent_project, " was sent, ignoring.\n", e)
                              NULL
                            })
      if (is.null(mail_sent) || isTRUE(mail_sent)) {
        return(invisible())
      }
      # check log file if previous user had error
      previous_user <- user[which(user == get_current_user()) - 1]
      if (!log_contains_error(user = previous_user, hour = hour, minute = minute, sent_delay - sent_delay, log_folder = log_folder)) {
        return(invisible())
      }
      message("!! Project p", check_sent_project, " was not sent, now retrying with user ", get_current_user(), "")
      proj_name <- search_project_first_local_then_planner(search_term = check_sent_project, as_title = TRUE, account = NULL)
      if (is.null(proj_name)) {
        proj_name <- paste0("p", check_sent_project)
      }
      certemail::mail(to = sent_account$properties$mail, cc = NULL, bcc = NULL,
                      subject = paste0("! Project niet verzonden: p", check_sent_project),
                      body = paste0("! Project **", proj_name,
                                    "** is eerder niet verzonden door gebruiker ", paste0("'", user[seq_len(which(user == get_current_user()) - 1)], "'", collapse = " en "),
                                    ", was gepland om ", format2(rounded_time - (which(user == get_current_user()) - 1) * sent_delay * 60, "h:MM"),
                                    " uur.\n\nNieuwe poging met gebruiker '", get_current_user(), "' om ",
                                    format2(rounded_time, "h:MM"), " uur."),
                      signature = FALSE,
                      background = colourpicker("certeroze3"),
                      identifier = FALSE,
                      account = sent_account)
    }
    
    if (isTRUE(log)) {
      message("Running scheduled task (project ", ") at ", format2(Sys.time(), "h:MM:ss"),
              ifelse(!is.null(check_sent_project) & isTRUE(backup_user),
                     paste0("\n*** Originally planned at ", format2(rounded_time - (which(user == get_current_user()) - 1) * sent_delay * 60, "h:MM:ss"), " ***"),
                     ""))
      message("`ref_time` was set as: ", format2(ref_time, "h:MM:ss"), ",\n",
              "   -> interpreting as: ", format2(rounded_time, "h:MM:ss"), ".\n")
    }
    project_file <- project_get_file(filename = file, project_number = project_number, account = account)
    tryCatch(source(project_file),
             error = function(e) {
               if (interactive()) {
                 stop(e)
               } else {
                 message("ERROR:\n", format_error(e))
                 if ("certemail" %in% rownames(utils::installed.packages())) {
                   certemail::mail(to = sent_to,
                                   cc = NULL,
                                   bcc = NULL,
                                   subject = paste0("! Fout in project: p", check_sent_project),
                                   body = paste0("! Project **", proj_name,
                                                 "** heeft een fout opgeleverd:\n\n",
                                                 "Gebruiker: ", get_current_user(), "\n",
                                                 "Datum/tijd: ", format2(ref_time, "dddd d mmmm yyyy HH:MM:SS"), "\n",
                                                 "Fout: ", format_error(e)),
                                   signature = FALSE,
                                   background = colourpicker("certeroze3"),
                                   identifier = FALSE,
                                   account = sent_account)
                 }
               }
             })
  } else {
    invisible()
  }
}

log_contains_error <- function(user, hour, minute, sent_delay, log_folder) {
  minute <- minute - sent_delay
  if (minute < 0) {
    hour <- hour - 1
    minute <- minute + 60
  }
  log_file <- list.files(path = log_folder, pattern = paste0(user, ".*", hour, "h", minute), full.names = TRUE, recursive = FALSE)
  if (length(log_file) == 0) {
    # no files, that's bad (if project runs, logs should be kept - or computer was even turned off)
    return(TRUE)
  }
  
}

#' @importFrom certestyle font_stripstyle
format_error <- function (e) {
  if (inherits(e, "rlang_error") && "rlang" %in% rownames(utils::installed.packages())) {
    txt <- rlang::cnd_message(e)
    txt <- font_stripstyle(txt)
    txt <- gsub(".*Caused by error[:](\n!)?", "", txt)
  }
  else {
    txt <- c(e$message, e$parent$message, e$parent$parent$message, 
             e$parent$parent$parent$message, e$call)
  }
  txt <- txt[txt %unlike% "^Problem while"]
  if (length(txt) == 0) {
    stop(e, call. = FALSE)
  }
  if (all(txt == "")) {
    txt <- "Unknown error"
  }
  txt <- trimws(txt)
  paste0(txt, collapse = "\n")
}
