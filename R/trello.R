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

#' Work with Trello Cards
#' 
#' These functions call the [Trello API](https://api.trello.com). It was replaced with [Microsoft Planner functions][connect_planner()].
#' @param title card title
#' @param desc card description
#' @param requested_by name of requester
#' @param project_path path to project to which this card belongs to
#' @param member Trello member, full name
#' @param prio priority, must be label name
#' @param duedate deadline
#' @param checklist items for checklist, can be character vector
#' @param checklist_name name for the checklist
#' @param comments comment to put on card, can be character vector
#' @param attachments attachments to put on card
#' @param list Trello list 
#' @param board Trello board
#' @param key Trello API key
#' @param token Trello API token 
#' @param ... arguments passed on to methods
#' @rdname trello
#' @name trello
#' @importFrom httr POST stop_for_status content
#' @details The [trello_upload()] returns the newly created Trello card number if the upload was successful.
#' @export
trello_upload <- function(title,
                          desc = "",
                          requested_by = "",
                          project_path = "",
                          member = trello_credentials("membername"),
                          prio = read_secret("trello.default.prio"),
                          duedate = "",
                          checklist = "",
                          checklist_name = read_secret("trello.default.checklist"),
                          comments = "",
                          attachments = "",
                          list = read_secret("trello.default.list"),
                          board = read_secret("trello.default.board"),
                          key = trello_credentials("key"),
                          token = trello_credentials("token"),
                          ...) {
  
  prio <- prio[1]
  
  # get properties
  lists <- trello_get_lists(board = board, key = key, token = token)
  members <- trello_get_members(board = board, key = key, token = token)
  labels <- trello_get_labels(board = board, key = key, token = token)
  # customfield <- trello_get_customfields(board = board, key = key, token = token)
  
  # check if `list` is an id
  if (list %in% lists$id) {
    list_id <- list
  } else {
    if (!tolower(list) %in% tolower(lists$name)) {
      warning("List ", list, " does not exist on this Trello board. Adding card to the first available list.", call. = FALSE)
      list_id <- lists[1, "id"]
    }
    list_id <- lists[which(tolower(lists$name) == tolower(list)), "id"][1]
  }
  
  description <- ""
  if (is_empty(requested_by)) {
    requested_by <- ""
  }
  
  for (i in seq_len(length(requested_by))) {
    if (requested_by[i] %in% get_user(property = "id")) {
      job <- get_user(id == requested_by[i], property = "job")
      if (is.na(job)) {
        job <- ""
      } else {
        job <- paste0(" (", tolower(job), ")")
      }
      requested_by[i] <- paste0("[",
                                get_user(id == requested_by[i], property = "name"),
                                "](mailto:",
                                get_user(id == requested_by[i], property = "mail"),
                                "?subject=", utils::URLencode(title),
                                ")",
                                job)
    }
  }
  requested_by <- paste(requested_by, collapse = ", ")
  if (requested_by != "") {
    description <- paste0("*Aangevraagd door: ", requested_by, "*")
    title <- paste0(strsplit(requested_by, "( |,|/)")[[1]][1], " - ", title)
    title <- gsub("^\\[", "", title) # remove first [
  }
  
  if (project_path != "") {
    description <- c(description,
                     paste0("*Project path: [", basename(project_path), "](file://", utils::URLencode(project_path), ")*"))
  }
  if (desc != "") {
    description <- c(description,
                     "",
                     desc)
  }
  
  if (as.character(duedate) != "") {
    duedate <- paste(as.Date(duedate), "11:00:00") # Trello adds six hours for timezone difference, this will become 5 PM
  }
  
  # create card
  request_card <- POST(url = "https://api.trello.com/1/cards",
                       body = list(idList = list_id,
                                   name = title,
                                   # https://stackoverflow.com/a/45218244/4575331:
                                   desc = paste0(description, collapse = "\x0A"),
                                   pos = "top",
                                   due = duedate,
                                   key = key,
                                   token = token))
  card_id <- content(request_card, "parsed", "application/json")$id
  card_nr <- content(request_card, "parsed", "application/json")$idShort
  stop_for_status(request_card, task = paste("add card", title))
  
  # add comments
  if (is.null(comments)) {
    comments <- ""
  }
  if (comments != "") {
    request_comments <- POST(url = paste0("https://api.trello.com/1/cards/", card_id, "/actions/comments"),
                             body = list(text = comments,
                                         key = key,
                                         token = token))
    stop_for_status(request_comments, task = paste("add comment", comments))
  }
  
  # add attachment(s)
  if (!all(attachments == "")) {
    for (i in seq_len(length(attachments))) {
      request_attachment <- POST(url = paste0("https://api.trello.com/1/cards/", card_id, "/attachments"),
                                 body = list(url = attachments[i],
                                             key = key,
                                             token = token))
      stop_for_status(request_attachment, task = paste("add attachment", attachments[i]))
    }
  }
  
  # add member(s)
  if (!all(member == "")) {
    
    for (i in seq_len(length(member))) {
      member_id <- members[which(members$id %like% member[i]
                                 | members$fullName %like% member[i]
                                 | members$username %like% member[i]), "id"]
      if (length(member_id) == 0) {
        warning("Member ", member[i], " not found on this Trello board.", call. = FALSE)
      } else {
        request_member <- POST(url = paste0("https://api.trello.com/1/cards/", card_id, "/idMembers"),
                               body = list(value = member_id,
                                           key = key,
                                           token = token))
        stop_for_status(request_member, task = paste("add member", member[i]))
      }
    }
  }
  
  # add priority label
  if (prio %like% "^(Laag|Lage)") {
    prio <- labels[which(labels$name %like% "Prio.*Laa?g.*"), "id"]
  } else if (prio %like% "^(Normaal|Normale)") {
    prio <- labels[which(labels$name %like% "Prio.*Norma.*"), "id"]
  } else if (prio %like% "^(Hoog|Hoge)") {
    prio <- labels[which(labels$name %like% "Prio.*Hoo?g.*"), "id"]
  } else {
    warning('Label like "', prio, '" not found on this Trello board, adding first empty label instead.', call. = FALSE)
    prio <- labels[which(labels$name == ""), "id"][1]
  }
  if (length(prio) > 0) {
    request_labelPrio <- POST(url = paste0("https://api.trello.com/1/cards/", card_id, "/idLabels"),
                              body = list(value = prio[1],
                                          key = key,
                                          token = token))
    stop_for_status(request_labelPrio, task = paste("add label", prio))
  } else {
    warning("Label for priority not found on this Trello board.", call. = FALSE)
  }
  # add empty checklist
  request_checklist <- POST(url = "https://api.trello.com/1/checklists",
                            body = list(idCard = card_id,
                                        name = checklist_name,
                                        pos = "top",
                                        key = key,
                                        token = token))
  checklist_id <- content(request_checklist, "parsed", "application/json")$id
  stop_for_status(request_checklist, task = "add checklist")
  # add items
  if (is_empty(checklist)) {
    checklist <- ""
  }
  for (i in seq_len(length(checklist))) {
    checklist[i] <- trimws(checklist[i])
    if (checklist[i] != "") {
      request_checklist_add <- POST(url = paste0("https://api.trello.com/1/checklists/", checklist_id, "/checkItems"),
                                    body = list(name = checklist[i],
                                                pos = "bottom",
                                                key = key,
                                                token = token))
      stop_for_status(request_checklist_add, task = paste("add checklist item", checklist[i]))
    }
  }
  
  # return card number
  card_nr
}

#' @rdname trello
#' @export
trello_credentials <- function(x = c("member", "key", "token", "membername")) {
  setting <- paste0("trello.", Sys.info()["user"], ".", x[1])
  out <- suppressWarnings(read_secret(setting))
  if (out == "") {
    stop("Secret '", setting, "' not set", call. = FALSE)
  }
  out
}

#' @rdname trello
#' @param username Trello username
#' @export
trello_get_boards <- function(username = trello_credentials("member"),
                              key = trello_credentials("key"),
                              token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/members/", username, "/boards?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_get_board_name <- function(board = read_secret("trello.default.board"),
                                  username = trello_credentials("member"),
                                  key = trello_credentials("key"),
                                  token = trello_credentials("token")) {
  boards <- trello_get_boards(username = username, key = key, token = token)
  boards$name[which(boards$id == board | boards$shortLink == board)][1]
}

#' @rdname trello
#' @export
trello_get_lists <- function(board = read_secret("trello.default.board"),
                             key = trello_credentials("key"),
                             token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/boards/", board,
                "/lists?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_get_members <- function(board = read_secret("trello.default.board"),
                               key = trello_credentials("key"),
                               token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/boards/", board,
                "/members?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_get_cards <- function(board = read_secret("trello.default.board"),
                             key = trello_credentials("key"),
                             token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/boards/", board,
                "/cards?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_get_labels <- function(board = read_secret("trello.default.board"),
                              key = trello_credentials("key"),
                              token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/boards/", board,
                "/labels?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_get_customfields <- function(board = read_secret("trello.default.board"),
                                    key = trello_credentials("key"),
                                    token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/boards/", board,
                "/customFields?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_get_checklists <- function(board = read_secret("trello.default.board"),
                                  key = trello_credentials("key"),
                                  token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/boards/", board,
                "/checklists?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_get_comments <- function(card_id,
                                key = trello_credentials("key"),
                                token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/cards/", card_id,
                "/actions?filter=commentCard&key=", key, "&token=", token))
}

#' @rdname trello
#' @param to_console a [logical] to indicate whether the card data should be printed to the Console
#' @importFrom dplyr filter
#' @importFrom certestyle font_blue font_bold font_silver
#' @export
trello_my_cards <- function(board = read_secret("trello.default.board"),
                            username = trello_credentials("member"),
                            key = trello_credentials("key"),
                            token = trello_credentials("token"),
                            list = read_secret("trello.current.list"),
                            to_console = TRUE) {
  cards <- GET_df(paste0("https://api.trello.com/1/search?query=member:", username,
                         "&cards_limit=1000",
                         "&card_list=true",
                         "&key=", key,
                         "&card_fields=idShort,name,dateLastActivity",
                         "&token=", token))
  cards <- cards$cards |> 
    filter(list.name == list)
  
  if (isTRUE(to_console)) {
    cat("Projects of '", font_blue(username), "' in list '", font_blue(list), "':\n\n", sep = "")
    cat(paste0(font_bold(paste0("p", cards$idShort), collapse = NULL), " ",
               gsub("^[A-Za-z-]+ - ", "", cards$name),
               font_silver(paste0(" (changed: ", format2(cards$dateLastActivity, "ddd d mmm yyyy"), ")"), collapse = NULL),
               collapse = "\n"))
    cat("\n\n\n")
  } else {
    paste0("p", cards$idShort, " ", gsub("^[A-Za-z-]+ - ", "", cards$name))
  }
}

#' @rdname trello
#' @param x search string
#' @details The function [trello_search_any()] returns all cards data as a [data.frame] and is used internally by [trello_search_card()].
#' @export
trello_search_any <- function(x,
                              key = trello_credentials("key"),
                              token = trello_credentials("token")) {
  # this is the actual Trello search API (very fast):
  GET_df(paste0("https://api.trello.com/1/search",
                "?key=", key, "&token=", token,
                "&query=", gsub("( | and | AND | ?& ?)", "+", x),
                "&partial=true",
                "&card_board=true",
                "&board_organization=true", # this requires card_board=true
                "&board_fields=shortLink", # this requires card_board=true
                "&card_list=true",
                "&cards_limit=50"))$cards
}

#' @rdname trello
#' @param x search string
#' @param return_all a [logical] to indicate whether a named vector of short URLs must be returned (internally used by [project_add()]) instead of a [double] vector
#' @importFrom certestyle font_green font_blue font_bold font_silver font_red
#' @importFrom rstudioapi showPrompt
#' @importFrom dplyr filter
#' @export
trello_search_card <- function(x = NULL,
                               return_all = FALSE,
                               board = read_secret("trello.default.board"),
                               key = trello_credentials("key"),
                               token = trello_credentials("token")) {
  if (interactive() && !isTRUE(return_all)) {
    txt <- ""
    if (tryCatch(trello_credentials("member"), error = function(e) "") != "") {
      max_n <- 5
      cards <- GET_df(paste0("https://api.trello.com/1/search?query=member:", trello_credentials("member"),
                             "&cards_limit=100",
                             "&card_list=true",
                             "&key=", key,
                             "&token=", token))
      cards <- cards$cards |>
        filter(list.name != "Voltooid")
      cards <- cards[seq_len(min(nrow(cards), max_n)), , drop = FALSE]
      cards$name[cards$name %like% "^[a-zA-Z-]+ - "] <- gsub("^([a-zA-Z-]+) - (.*)", "\\2 (\\1)", cards$name[cards$name %like% "^[a-zA-Z-]+ - "])
      # replace accents in vowels etc.:
      cards$name <- iconv(cards$name, from = "Latin1", to = "ASCII//TRANSLIT")
      txt <- paste0("My Last ", max_n, " Open Projects:\n\n",
                    paste0("p", cards$idShort, " ", cards$name, collapse = "\n"),
                    "\n\n",
                    "-----------------------------\n",
                    "\n")
    }
    x <- showPrompt(title = "Search Project",
                    message = paste0(txt, "Search for Project (search in title, description, project number, ...):"),
                    default = x)
  }
  if (is.null(x) || x %in% c("", NA)) {
    return(NULL)
  }
  
  # strip "p" for project identifiers
  x <- gsub("^p([0-9]+)", "\\1", x)
  if (x %like% "^[0-9]+$" & !isTRUE(return_all)) {
    return(as.double(x))
  }
  
  # search using the Trello API 
  search <- trello_search_any(x = x, key = key, token = token)
  # only keep current board
  if (NROW(search) > 0) {
    search <- search[which(search$board.shortLink == board), , drop = FALSE]
  }
  
  if (!isTRUE(return_all)) {
    if (NROW(search) == 1) {
      return(search$idShort) # also in non-interactive mode
    } else if (!interactive()) {
      return(NULL)
    } else if (NROW(search) == 0) {
      message("No projects found.")
      return(NULL)
    }
  }
  
  # return named vector with shortUrls
  if (isTRUE(return_all)) {
    # used by Shiny app in project_add()
    if (NROW(search) == 0) {
      return(character(0))
    }
    urls <- search$shortUrl
    names(urls) <- paste0(trimws(search$name), " [p", search$idShort, ", ", search$list.name, "]")
    return(urls)
  }
  titles <- trimws(search$name)
  titles <- gsub("^[A-Z][a-z]+ - ", "", titles)
  lists <- search$list.name
  lists[lists == "Voltooid"] <- font_green("Voltooid")
  lists[lists == "Wachten op een ander"] <- font_red("Wachten op een ander")
  lists[lists == "Bezig"] <- font_blue("Bezig")
  lists[!lists %in% c("Voltooid", "Wachten op een ander", "Bezig")] <- font_silver(lists[!lists %in% c("Voltooid", "Wachten op een ander", "Bezig")], collapse = NULL)
  card_id <- utils::menu(title = paste0("Projects found with text \"", x, "\" (0 for Cancel):"),
                         graphics = FALSE,
                         choices = paste0(titles, " - ",
                                          font_bold(paste0("p", search$idShort), collapse = NULL),
                                          " (", lists, ")"))
  if (is.null(card_id)) {
    return(NULL)
  } else {
    return(as.double(search$idShort[card_id]))
  }
}

#' @rdname trello
#' @export
trello_open_board <- function(board = read_secret("trello.default.board")) {
  utils::browseURL(paste0("https://trello.com/b/", board))
}

#' @rdname trello
#' @export
trello_open_card <- function() {
  id <- project_get_current_id(ask = TRUE)
  if (is.null(id)) {
    return(invisible())
  }
  content <- trello_get_card_property(id, "url")
  utils::browseURL(content)
}

#' @rdname trello
#' @param property property to take from card
#' @importFrom dplyr filter pull
#' @export
trello_get_card_property <- function(card_number,
                                     property,
                                     board = read_secret("trello.default.board"),
                                     key = trello_credentials("key"),
                                     token = trello_credentials("token")) {
  trello_get_cards(board, key, token) |> filter(idShort == card_number) |> pull(property)
}

#' @rdname trello
#' @param card_number Trello card number
#' @importFrom dplyr filter pull
#' @export
trello_get_card_id <- function(card_number,
                               board = read_secret("trello.default.board"),
                               key = trello_credentials("key"),
                               token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/search",
                "?key=", key, "&token=", token,
                "&query=", card_number,
                "&partial=false",
                "&card_board=false",
                "&cards_limit=1"))$cards$id
}

#' @rdname trello
#' @export
trello_get_creation_datetime <- function(card_number,
                                         board = read_secret("trello.default.board"),
                                         key = trello_credentials("key"),
                                         token = trello_credentials("token")) {
  card_number <- as.double(card_number)
  if (card_number %in% seq_len(9999)) {
    id <- trello_get_card_id(card_number = card_number,
                             board = board,
                             key = key,
                             token = token)
    if (is.null(id)) {
      return(as.POSIXct(NA))
    }
    # first 8 hexadecimal characters of Trello card ID are the creation date
    as.POSIXct(as.double(paste0("0x", substr(id, 1, 8))),
               origin = "1970-01-01")
  } else {
    as.POSIXct(NA)
  }
}

#' @rdname trello
#' @param card_id Trello card ID (*not* the card number)
#' @param comment the comment to set, supports markdown
#' @importFrom httr POST stop_for_status
#' @export
trello_set_comment <- function(card_id,
                               comment,
                               key = trello_credentials("key"),
                               token = trello_credentials("token")) {
  
  request_comments <- POST(url = paste0("https://api.trello.com/1/cards/", card_id, "/actions/comments"),
                           body = list(text = comment,
                                       key = key,
                                       token = token))
  stop_for_status(request_comments, task = paste("add comment to card", card_id))
}

#' @rdname trello
#' @importFrom dplyr filter pull 
#' @importFrom httr POST stop_for_status DELETE
#' @export
trello_set_members <- function(card_id,
                               member,
                               board = read_secret("trello.default.board"),
                               key = trello_credentials("key"),
                               token = trello_credentials("token")) {
  
  members <- trello_get_members(board = board, key = key, token = token)
  members_new <- members |>
    filter(fullName %in% member | username %in% member) |>
    pull(id) |>
    unlist()
  members_old <- trello_get_cards() |>
    filter(id == card_id) |>
    pull(idMembers) |>
    unlist()
  
  members_to_add <- members_new[!members_new %in% members_old]
  members_to_delete <- members_old[!members_old %in% members_new]
  
  for (member_id in members_to_add) {
    add_members <- POST(url = paste0("https://api.trello.com/1/cards/", card_id, "/idMembers"),
                        body = list(value = member_id,
                                    key = key,
                                    token = token))
    stop_for_status(add_members, task = paste("add member", members[which(members$id == member_id),]$fullName))
  }
  for (member_id in members_to_delete) {
    remove_members <- DELETE(url = paste0("https://api.trello.com/1/cards/", card_id, "/idMembers/", member_id),
                             body = list(idMember = member_id,
                                         value = member_id,
                                         key = key,
                                         token = token))
    # stop_for_status(remove_members, task = paste("remove member", members[which(members$id == member_id),]$fullName))
  }
}

#' @rdname trello
#' @param duedate a [Date] object
#' @param duecomplete a [logical] to indicate whether due date is already completed
#' @importFrom httr PUT stop_for_status
#' @export
trello_set_deadline <- function(card_id,
                                duedate,
                                duecomplete = FALSE,
                                key = trello_credentials("key"),
                                token = trello_credentials("token")) {
  
  if (!is.null(duedate)) {
    duedate <- paste(as.Date(duedate), "11:00:00") # Trello adds 6 hours due to time zone difference Europe vs USA, this will become 5 PM
  } else {
    duedate <- "null"
  }
  request_card <- PUT(url = paste0("https://api.trello.com/1/cards/", card_id),
                      body = list(due = duedate,
                                  dueComplete = duecomplete,
                                  key = key,
                                  token = token),
                      encode = "json") # required for PUT()
  
  stop_for_status(request_card, task = paste("change deadline of card", card_id))
}

#' @rdname trello
#' @param new_items character vector of checklist items
#' @param checklist_name name of the checklist
#' @importFrom httr POST stop_for_status
#' @importFrom dplyr filter
#' @export
trello_add_task <- function(card_id,
                            new_items = NULL,
                            checklist_name = "Taken",
                            board = read_secret("trello.default.board"),
                            key = trello_credentials("key"),
                            token = trello_credentials("token")) {
  
  checklist_id <- trello_get_checklists() |> filter(idCard == card_id)
  # does checklist exist?
  if (NROW(checklist_id) == 0 | checklist_id$name[1L] != checklist_name) {
    # create checklist
    request_checklist <- POST(url = "https://api.trello.com/1/checklists",
                              body = list(idCard = card_id,
                                          name = checklist_name,
                                          pos = "top",
                                          key = key,
                                          token = token))
    checklist_id <- content(request_checklist, "parsed", "application/json")$id
    stop_for_status(request_checklist, task = "add checklist")
  } else {
    checklist_id <- checklist_id$id[1L]
  }
  
  # add checklist items
  if (is_empty(new_items)) {
    new_items <- ""
  }
  for (i in seq_len(length(new_items))) {
    new_items[i] <- trimws(new_items[i])
    if (new_items[i] != "") {
      request_checklist_add <- POST(url = paste0("https://api.trello.com/1/checklists/", checklist_id, "/checkItems"),
                                    body = list(name = new_items[i],
                                                pos = "bottom",
                                                key = key,
                                                token = token))
      stop_for_status(request_checklist_add, task = paste("add checklist item", new_items[i]))
    }
  }
}

#' @rdname trello
#' @param checkitem_id checkitem ID of item in checklist
#' @param new_value the new value to set: a [logical] to indicate the 'complete' state of the checklist item
#' @importFrom httr PUT stop_for_status
#' @export
trello_set_task_state <- function(card_id,
                                  checkitem_id,
                                  new_value = TRUE,
                                  key = trello_credentials("key"),
                                  token = trello_credentials("token")) {
  
  request_checkitem <- PUT(url = paste0("https://api.trello.com/1/cards/", card_id, "/checkItem/", checkitem_id),
                           body = list(state = ifelse(new_value == TRUE, "complete", "incomplete"),
                                       key = key,
                                       token = token),
                           encode = "json") # required for PUT()
  stop_for_status(request_checkitem, task = paste("change state of checkitem", checkitem_id))
}

#' @rdname trello
#' @param list_id ID of the Trello board list
#' @importFrom httr PUT stop_for_status
#' @export
trello_move_card <- function(card_id,
                             list_id,
                             key = trello_credentials("key"),
                             token = trello_credentials("token")) {
  
  request_comments <- PUT(url = paste0("https://api.trello.com/1/cards/", card_id),
                          body = list(idList = list_id,
                                      pos = "top", # always to top
                                      key = key,
                                      token = token),
                          encode = "json") # required for PUT()
  stop_for_status(request_comments, task = paste("move card", card_id))
}

#' @importFrom jsonlite fromJSON
#' @importFrom httr GET stop_for_status content
GET_df <- function(x) {
  result <- GET(x)
  stop_for_status(result)
  result |>
    content(type = "text", encoding = "UTF-8") |>
    fromJSON(flatten = TRUE)
}
