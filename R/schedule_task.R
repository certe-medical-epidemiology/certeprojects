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
#' @param user logged in user, must correspond with `Sys.info()["user"]`. Currently logged in user is "\Sexpr{certeprojects:::get_current_user()}".
#' @param expr expression to run
#' @param log a [logical] to indicate whether this message should be printed: *Running scheduled task at...*
#' @param ref_time time to use for reference, defaults to [Sys.time()]
#' @details
#' The Windows Task Scheduler must be set up to use this function. Most convenient is to:
#' 
#' 1. Create an \R file such as `R_cron.R` with calls to [schedule_task()]
#' 2. Create a batch file such as `R_cron.bat` that runs `R_cron.R` with `R CMD BATCH`
#' 3. Set up a Task Scheduler task that runs `R_cron.bat` every minute
#' @export
#' @examples
#' count_it <- function() {
#'   1 + 1
#' }
#'
#' # units:      M  H  d  m  wd
#' schedule_task(., ., ., ., ., "user", count_it()) # every minute
#' schedule_task(0, ., ., ., ., "user", count_it()) # start of each hour
#' schedule_task(0, 7, ., ., ., "user", count_it()) # everyday at 7h00
#' schedule_task(0, 7, 1, ., ., "user", count_it()) # first day of month at 7h00
#' schedule_task(0, 7, ., 1, ., "user", count_it()) # everyday day in January at 7h00
#' schedule_task(0, 7, ., ., 1, "user", count_it()) # every Monday at 7h00
#'
#' # every second Monday of the month at 7h00
#' # (= alleen maandag tussen 8-14e dag in de maand)
#' schedule_task(0, 7, c(8:14), ., 1, "user", count_it())
#'
#' # every year on 15th of April at 8h30 and 16h30
#' schedule_task(30, c(8, 16), 15, 4, ., "user", count_it())
#'
#' # once per quarter at 8 on the first day of the month
#' schedule_task(0, 8, 1, c(1, 4, 7, 10), ., "user", count_it())
schedule_task <- function(minute, hour, day, month, weekday,
                          user,
                          expr,
                          log = TRUE,
                          ref_time = Sys.time()) {
  
  if (missing(user)) {
    stop("username not set with `user`")
  }
  
  if (deparse(substitute(minute)) == ".") minute <- "."
  if (deparse(substitute(hour)) == ".") hour <- "."
  if (deparse(substitute(day)) == ".") day <- "."
  if (deparse(substitute(month)) == ".") month <- "."
  if (deparse(substitute(weekday)) == ".") weekday <- "."
  expr_txt <- deparse(substitute(expr))
  
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
  
  if ("lubridate" %in% rownames(utils::installed.packages())) {
    # round time, since Windows Task Scheduler is sometimes 1-10 seconds off
    rounded_time <- lubridate::round_date(ref_time, unit = "1 minute")
  } else {
    rounded_time <- ref_time
  }
  rounded_time <- as.POSIXlt(rounded_time)
  
  if (any(minute == rounded_time$min) &
      any(hour == rounded_time$hour) &
      any(day == rounded_time$mday) &
      any(month == rounded_time$mon + 1) & # "mon" is month in 0-11
      any(weekday == rounded_time$wday)) {
    # run as planned
    if (user != get_current_user()) {
      return(invisible())
    }
    if (isTRUE(log)) {
      message("Running scheduled task at ", format2(Sys.time(), "h:MM:ss"), ";")
      message("`ref_time` was set as: ", as.character(ref_time), ",\n",
              "   -> interpreting as: ", as.character(rounded_time), ".\n")
    }
    write_task_has_started(usr = get_current_user(),
                           t = rounded_time,
                           expr = expr_txt)
    try(expr)
    
  } else if (any(minute + 1 == rounded_time$min) &
             any(hour == rounded_time$hour) &
             any(day == rounded_time$mday) &
             any(month == rounded_time$mon + 1) & # "mon" is month in 0-11
             any(weekday == rounded_time$wday)) {
    # check if task ran, otherwise run using other account
    if (user == get_current_user()) {
      return(invisible())
    }
    if (!task_has_run(user, rounded_time, expr_txt)) {
      if (isTRUE(log)) {
        message("Task did NOT run as planned for user '", user, "', scheduled task at ", as.character(ref_time), "h:MM:ss", ".\n",
                "Rerunning now as user '", get_current_user(), "'.\n")
      }
      write_task_has_started(usr = get_current_user(),
                             t = rounded_time,
                             expr = deparse(substitute(expr)))
      try(expr)
    }
    
  } else {
    # do nothing
    invisible()
  }
}

get_current_user <- function() {
  unname(Sys.info()["user"])
}

write_task_has_started <- function(usr, t, expr, path = paste0(read_secret("path.refmap"), "_cron_history.tsv")) {
  write.table(data.frame(datetime = t,
                         user = usr,
                         expression = expr),
              file = path,
              quote = FALSE,
              sep = "\t",
              col.names = !file.exists(path),
              row.names = FALSE,
              append = file.exists(path))
}

task_has_run <- function(usr, t, expr, path = paste0(read_secret("path.refmap"), "_cron_history.tsv")) {
  if (!file.exists(path)) {
    write_task_has_started(NA, NA, NA, path = path)
  }
  df <- read.table(path, sep = "\t", header = TRUE)
  any(df$datetime == t & df$user == usr & df$expression == expr, na.rm = TRUE)
}
