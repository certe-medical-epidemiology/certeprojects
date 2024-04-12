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
#' This will [source][source()] a project file if time and user requirements are met, using a CRON-like syntax (<https://cron.help>).
#' @param minute one or more values between 0-59, or `.` or `"*"` for each minute
#' @param hour one or more values between 0-23, or `.` or `"*"` for each hour
#' @param day one or more values between 1-31, or `.` or `"*"` for each day
#' @param month one or more values between 1-12, or `.` or `"*"` for each mpnth
#' @param weekday one or more values between 0-7 (Sunday is both 0 and 7; Monday is 1), or `.` or `"*"` for each weekday
#' @param users logged in users, must correspond with `Sys.info()["user"]`. Currently logged in user is "\Sexpr{certeprojects:::get_current_user()}". This must be length > 1 if `check_mail` is `TRUE`.
#' @param file file name within the project, supports regular expression
#' @param project_number number of the project, must be numeric and exist in [planner_tasks_list()]
#' @param log a [logical] to indicate whether this message should be printed: *Running scheduled task at...*
#' @param account Planner account
#' @param check_mail a [logical] to indicate whether a project was sent by a previous user, by running [certemail::mail_is_sent()]
#' @param check_log a [logical] to indicate whether a log file exist for the project from a previous user
#' @param sent_to users to send error mail to
#' @param log_folder path that contains log files
#' @param ref_time time to use for reference, defaults to [Sys.time()]
#' @param sent_delay delay in minutes. This will be multiplied by the position of the current user in `users` minus 1. For example, when `sent_delay = 15`, this will be `15` for user 2, and `30` for user 3.
#' @param sent_account Outlook account, to search sent mails
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
#' schedule_task(., ., ., ., ., "user", "file", 123) # every minute
#' schedule_task(0, ., ., ., ., "user", "file", 123) # start of each hour
#' schedule_task(0, 7, ., ., ., "user", "file", 123) # everyday at 7h00
#' schedule_task(0, 7, 1, ., ., "user", "file", 123) # first day of month at 7h00
#' schedule_task(0, 7, ., 2, ., "user", "file", 123) # everyday day in February at 7h00
#' schedule_task(0, 7, ., ., 1, "user", "file", 123) # every Monday at 7h00
#' schedule_task(0, 7, 1, 2, ., "user", "file", 123) # every 1st of February at 7h00
#' schedule_task(0, 7, ., 2, 1, "user", "file", 123) # every Monday in February at 7h00
#' schedule_task(0, 7, 1, 2, 1, "user", "file", 123) # each February 1st if it's a Monday at 7h00
#' schedule_task(0, 7,29, 2, ., "user", "file", 123) # once every 4 years at 7h00
#' 
#' # examples of combinations
#' 
#' # everyday at 7h00 and 7h30
#' schedule_task(c(0, 30), 7,        .,       .,              ., "user", "file", 123)
#' # everyday at 7h00 and 15h00
#' schedule_task(0,        c(7, 15), .,       .,              ., "user", "file", 123)
#' # everyday at 7h00 and 7h30 and 15h00 and 15h30
#' schedule_task(c(0, 30), c(7, 15), .,       .,              ., "user", "file", 123)
#' # every second Monday of the month at 7h00:
#' schedule_task(0,        7,        c(8:14), .,              1, "user", "file", 123)
#' # every 15th of April at 8h30 and 16h30:
#' schedule_task(30,       c(8, 16), 15,      4,              ., "user", "file", 123)
#' # once per quarter at 8h00 on the first day of the month:
#' schedule_task(0,        8,        1,       c(1, 4, 7, 10), ., "user", "file", 123)
#' 
#' # fall-back for failed jobs
#' 
#' # this will run at 8h00 if current user is "user1"
#' schedule_task(0, 8, ., ., ., c("user1", "user2"), "file", 123)
#' # it will run again at default 15 minutes later (so, 8h15), if:
#' # - current user is "user2"
#' # - project 123 has no mail in Sent Items or log of "user1" contains errors
schedule_task <- function(minute, hour, day, month, weekday,
                          users,
                          file,
                          project_number,
                          log = TRUE,
                          ref_time = Sys.time(),
                          account = connect_planner(),
                          check_mail = length(users) > 1,
                          check_log = length(users) > 1,
                          sent_delay = 15,
                          sent_account = connect_outlook(),
                          sent_to = read_secret("mail.error_to"),
                          log_folder = read_secret("projects.log_path")) {
  
  if (isTRUE(check_mail) && length(users) == 1) {
    stop("`users` must at least be length 2 if `check_mail` is set")
  }
  if (isFALSE(check_mail) && length(users) != 1) {
    stop("`users` must be length 1 if `check_mail` is not set`")
  }
  users <- as.character(users)
  if ((isFALSE(check_mail) && !get_current_user() == users[1]) || (isTRUE(check_mail) && !get_current_user() %in% users)) {
    # message("User not required to run project, ignoring")
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
  if (isTRUE(check_mail) && get_current_user() %in% users[2:length(users)]) {
    backup_user <- TRUE
    minute <- minute + (which(users == get_current_user()) - 1) * sent_delay
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
    
    project_number <- as.integer(gsub("p", "", project_number))
    proj_name <- search_project_first_local_then_planner(search_term = project_number, as_title = TRUE, account = NULL)
    if (is.null(proj_name)) {
      proj_name <- paste0("p", project_number)
    }
    project_file <- project_get_file(filename = file, project_number = project_number, account = account)
    
    if (isTRUE(check_mail) && get_current_user() %in% users[2:length(users)] && "certemail" %in% rownames(utils::installed.packages())) {
      mail_sent <- tryCatch(certemail::mail_is_sent(project_number = project_number,
                                                    date = as.Date(ref_time),
                                                    account = sent_account),
                            error = function(e) {
                              message("Could not determine whether mail of project ", project_number, " was sent, ignoring.\n", e)
                              NULL
                            })
      if (isTRUE(mail_sent)) {
        message("Project was already sent by mail (", names(mail_sent), "), ignoring")
        return(invisible())
      }
      # check log file if previous user had error
      user1_has_log <- user_has_log(user = users[1], date = as.Date(ref_time),
                                    hour = hour, minute = minute,
                                    sent_delay = (which(users == get_current_user()) - 1) * sent_delay,
                                    log_folder = log_folder)
      log_error_user1 <- log_contains_error(user = users[1], date = as.Date(ref_time),
                                            hour = hour, minute = minute,
                                            sent_delay = (which(users == get_current_user()) - 1) * sent_delay,
                                            log_folder = log_folder)
      if (!user1_has_log) {
        message("No log file of user 1 (", users[1], ") found")
      } else if (user1_has_log && !log_error_user1) {
        message("Log file of user 1 (", users[1], ") found: ", names(user1_has_log), "\n",
                "This log file contains no error, ignoring")
        return(invisible())
      }
      if (length(users) > 2 && get_current_user() == users[3]) {
        # also check logs of user 2
        user2_has_log <- user_has_log(user = users[2], date = as.Date(ref_time),
                                      hour = hour, minute = minute,
                                      sent_delay = sent_delay, log_folder = log_folder)
        log_error_user2 <- log_contains_error(user = users[2], date = as.Date(ref_time),
                                              hour = hour, minute = minute,
                                              sent_delay = sent_delay, log_folder = log_folder)
        if (!user2_has_log) {
          message("No log file of user 2 (", users[2], ") found")
        } else if (user2_has_log && !log_error_user2) {
          message("Log file of user 2 (", users[2], ") found: ", names(user2_has_log), "\n",
                  "This log file contains no error, ignoring")
          return(invisible())
        }
      }
      message("! Project p", project_number, " was not sent, now retrying with user ", get_current_user())
      certemail::mail(to = sent_account$properties$mail, cc = NULL, bcc = NULL,
                      subject = paste0("! Project niet verzonden: p", project_number),
                      body = paste0("Project **", proj_name,
                                    "** is eerder niet verzonden door gebruiker ", paste0(users[seq_len(which(users == get_current_user()) - 1)], collapse = " en "),
                                    ", was gepland om ", format2(rounded_time - (which(users == get_current_user()) - 1) * sent_delay * 60, "h:MM"),
                                    " uur.\n\nNieuwe poging met gebruiker ", get_current_user(), " om ",
                                    format2(rounded_time, "h:MM"), " uur."),
                      signature = FALSE,
                      background = colourpicker("certeroze4"),
                      identifier = FALSE,
                      account = sent_account)
    }
    
    if (isTRUE(log)) {
      message("Running scheduled task (p", project_number, ", '", basename(project_file), "') at ", format2(Sys.time(), "h:MM:ss"),
              ifelse(isTRUE(check_mail) & isTRUE(backup_user),
                     paste0("\n*** Originally planned at ", format2(rounded_time - (which(users == get_current_user()) - 1) * sent_delay * 60, "h:MM:ss"), " ***"),
                     ""))
      message("`ref_time` was set as: ", format2(ref_time, "h:MM:ss"), ",\n",
              "   -> interpreting as: ", format2(rounded_time, "h:MM:ss"), ".\n")
    }
    tryCatch(source(project_file),
             error = function(e) {
               if ("certemail" %in% rownames(utils::installed.packages())) {
                 certemail::mail(to = sent_to,
                                 cc = NULL,
                                 bcc = NULL,
                                 subject = paste0("! Fout in project: p", project_number),
                                 body = paste0("Project **", proj_name,
                                               "** heeft een fout opgeleverd.\n\n",
                                               "# AVD-details\n\n",
                                               "Gebruiker: ", get_current_user(), "\n\n",
                                               "Datum/tijd: ", format2(ref_time, "dddd d mmmm yyyy HH:MM:SS"), "\n\n",
                                               "# Foutdetails\n\n",
                                               format_error(e)),
                                 signature = FALSE,
                                 background = colourpicker("certeroze4"),
                                 identifier = FALSE,
                                 account = sent_account)
               }
               message("ERROR: ", format_error(e))
             })
  } else {
    # message(paste0("User not required to run project, ignoring (ref_time ",
    #                paste0(format2(unique(rounded_time), "HH:MM"), collapse = "/"), " is not planned time ",
    #                paste0(gsub("(.*)(..)$", "\\1:\\2", unique(hourminute)), collapse = "/"), " or ", sent_delay, " minutes later)"))
    return(invisible())
  }
}

user_has_log <- function(user, date, hour, minute, sent_delay, log_folder) {
  minute <- as.integer(minute) - sent_delay
  hour <- as.integer(hour)
  
  hour[minute < 0] <- hour[minute < 0] - 1
  minute[minute < 0] <- minute[minute < 0] + 60
  minute <- formatC(minute, flag = 0, width = 2)
  hour <- formatC(hour, flag = 0, width = 2)
  
  path <- paste0(log_folder, format2(Sys.Date(), "yyyy/mm"), "/", user, "/")
  pattern <- paste0(user, ".*", format2(date, "yyyy[-]mm[-]dd"),
                    ".*(", paste0(hour, collapse = "|"), ")u(", paste0(minute, collapse = "|"),
                    ").*[.]Rout")
  log_file <- list.files(path = path,
                         pattern = pattern,
                         full.names = TRUE,
                         recursive = FALSE)
  stats::setNames(length(log_file) > 0,
                  paste0(log_file, collapse = ", "))
}

log_contains_error <- function(user, date, hour, minute, sent_delay, log_folder) {
  minute <- as.integer(minute) - sent_delay
  hour <- as.integer(hour)
  
  hour[minute < 0] <- hour[minute < 0] - 1
  minute[minute < 0] <- minute[minute < 0] + 60
  minute <- formatC(minute, flag = 0, width = 2)
  hour <- formatC(hour, flag = 0, width = 2)
  
  path <- paste0(log_folder, format2(Sys.Date(), "yyyy/mm"), "/", user, "/")
  pattern <- paste0(user, ".*", format2(date, "yyyy[-]mm[-]dd"),
                    ".*(", paste0(hour, collapse = "|"), ")u(", paste0(minute, collapse = "|"),
                    ").*[.]Rout")
  log_file <- list.files(path = path,
                         pattern = pattern,
                         full.names = TRUE,
                         recursive = FALSE)
  if (length(log_file) == 0) {
    # no files, that's bad (if project runs, logs should be kept - or computer was even turned off)
    return(TRUE)
  }
  failed <- logical(length(log_file))
  for (i in seq_len(length(log_file))) {
    lines <- readLines(log_file[i])
    failed[i] <- any(lines %like% "Could not connect", na.rm = TRUE) | any(lines %like_case% "Error|ERROR", na.rm = TRUE)
  }
  any(failed, na.rm = TRUE)
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
