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
#' This functions all call the [Trello api](https://api.trello.com).
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
#' @export
trello_upload <- function(title,
                          desc = "",
                          requested_by = "",
                          project_path = "",
                          member = Sys.getenv("R_USERNAME"),
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
  lists <- trello_getlists(board = board, key = key, token = token)
  members <- trello_getmembers(board = board, key = key, token = token)
  labels <- trello_getlabels(board = board, key = key, token = token)
  # customfield <- trello_getcustomfields(board = board, key = key, token = token)
  
  # check if `list` is an id
  if (list %in% lists$id) {
    list_id <- list
  } else {
    if (!tolower(list) %in% tolower(lists$name)) {
      warning("List ", list, " does not exist on this Trello board. Adding card to the first available list.", call. = FALSE)
      list_id <- lists[1, 'id']
    }
    list_id <- lists[which(tolower(lists$name) == tolower(list)), 'id'][1]
  }
  
  description <- ""
  if (all(is.null(requested_by)) | length(requested_by) == 0) {
    requested_by <- ""
  }
  # for (i in 1:length(requested_by)) {
  #   if (requested_by[i] != get_certe_user(requested_by[i])) {
  #     job <- get_certe_user(requested_by[i], 'job')
  #     if (is.na(job)) {
  #       job <- ''
  #     } else {
  #       job <- paste0(' (', tolower(job), ')')
  #     }
  #     requested_by[i] <- paste0("[",
  #                               get_certe_user(requested_by[i], 'name'),
  #                               "](mailto:",
  #                               get_certe_user(requested_by[i], 'mail'),
  #                               "?subject=", URLencode(title),
  #                               ")",
  #                               job)
  #   }
  # }
  # requested_by <- concat(get_certe_user(requested_by), ", ")
  # if (requested_by != "") {
  #   description <- paste0("*Aangevraagd door: ", requested_by, '*')
  #   title <- paste0(strsplit.select(requested_by, 1, "( |,|/)"), " - ", title)
  #   title <- gsub("^\\[", "", title) # eerste [ verwijderen bij naam met maillink
  # }
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
    duedate <- paste(as.Date(duedate), "11:00:00") # Trello add six hours for timezone difference, this will become 5 PM
  }
  
  # create card
  request_card <- POST(url = "https://api.trello.com/1/cards",
                       body = list(idList = list_id,
                                   name = title,
                                   # https://stackoverflow.com/a/45218244/4575331:
                                   desc = paste0(description, collapse = "\x0A"),
                                   pos = 'top',
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
    for (i in 1:length(attachments)) {
      request_attachment <- POST(url = paste0("https://api.trello.com/1/cards/", card_id, "/attachments"),
                                 body = list(url = attachments[i],
                                             key = key,
                                             token = token))
      stop_for_status(request_attachment, task = paste("add attachment", attachments[i]))
    }
  }
  
  # add member(s)
  if (!all(member == "")) {
    
    for (i in 1:length(member)) {
      member_id <- members[which(members$id %like% member[i]
                                 | members$fullName %like% member[i]
                                 | members$username %like% member[i]), 'id']
      if (length(member_id) == 0) {
        warning('Member ', member[i], ' not found on this Trello board.', call. = FALSE)
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
    prio <- labels[which(labels$name %like% 'Prio.*Laa?g.*'), 'id']
  } else if (prio %like% "^(Normaal|Normale)") {
    prio <- labels[which(labels$name %like% 'Prio.*Norma.*'), 'id']
  } else if (prio %like% "^(Hoog|Hoge)") {
    prio <- labels[which(labels$name %like% 'Prio.*Hoo?g.*'), 'id']
  } else {
    warning('Label like "', prio, '" not found on this Trello board, adding first empty label instead.', call. = FALSE)
    prio <- labels[which(labels$name == ""), 'id'][1]
  }
  if (length(prio) > 0) {
    request_labelPrio <- POST(url = paste0("https://api.trello.com/1/cards/", card_id, "/idLabels"),
                              body = list(value = prio[1],
                                          key = key,
                                          token = token))
    stop_for_status(request_labelPrio, task = paste("add label", prio))
  } else {
    warning('Label for priority not found on this Trello board.', call. = FALSE)
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
  if (all(is.null(checklist)) | length(checklist) == 0) {
    checklist <- ""
  }
  for (i in 1:length(checklist)) {
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
trello_credentials <- function(x = c("member", "key", "token")) {
  read_secret(paste0("trello.", Sys.info()["user"], ".", x[1]))
}

#' @rdname trello
#' @param username Trello username
#' @export
trello_getboards <- function(username = trello_credentials("member"),
                             key = trello_credentials("key"),
                             token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/members/", username, "/boards?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_getboardname <- function(board = read_secret("trello.default.board"),
                                username = trello_credentials("member"),
                                key = trello_credentials("key"),
                                token = trello_credentials("token")) {
  boards <- trello_getboards(username = username, key = key, token = token)
  boards$name[which(boards$id == board | boards$shortLink == board)][1]
}

#' @rdname trello
#' @export
trello_getlists <- function(board = read_secret("trello.default.board"),
                            key = trello_credentials("key"),
                            token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/boards/", board,
                "/lists?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_getmembers <- function(board = read_secret("trello.default.board"),
                              key = trello_credentials("key"),
                              token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/boards/", board,
                "/members?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_getcards <- function(board = read_secret("trello.default.board"),
                            key = trello_credentials("key"),
                            token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/boards/", board,
                "/cards?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_getlabels <- function(board = read_secret("trello.default.board"),
                             key = trello_credentials("key"),
                             token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/boards/", board,
                "/labels?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_getcustomfields <- function(board = read_secret("trello.default.board"),
                                   key = trello_credentials("key"),
                                   token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/boards/", board,
                "/customFields?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_getchecklists <- function(board = read_secret("trello.default.board"),
                                 key = trello_credentials("key"),
                                 token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/boards/", board,
                "/checklists?key=", key, "&token=", token))
}

#' @rdname trello
#' @export
trello_getcomments <- function(card_id,
                               key = trello_credentials("key"),
                               token = trello_credentials("token")) {
  GET_df(paste0("https://api.trello.com/1/cards/", card_id,
                "/actions?filter=commentCard&key=", key, "&token=", token))
}

#' @rdname trello
#' @param x search string
#' @importFrom dplyr `%>%` transmute pull left_join mutate filter
#' @export
trello_searchcard <- function(x,
                              board = read_secret("trello.default.board"),
                              key = trello_credentials("key"),
                              token = trello_credentials("token")) {
  
  cards <- trello_getcards(board = board, key = key, token = token)
  lists <- trello_getlists(board = board, key = key, token = token)
  checklists <- trello_getchecklists(board = board, key = key, token = token)
  
  if (length(cards) == 0) {
    return(character(0))
  }
  
  total <- cards %>% left_join(lists, by = c("idList" = "id"), suffix = c(".card", ".list"))
  if (length(checklists) > 0) {
    total <- total %>%
      left_join(checklists, by = c("id" = "idCard"), suffix = c(".card", ".checklist")) %>%
      mutate(checklistitems = "")
    # add checklist items
    for (i in 1:nrow(total)) {
      if (is.list(total[i, 'checkItems'])) {
        total[i, 'checklistitems'] <- concat(total[i, 'checkItems'][[1]]$name, " ")
      }
    }
  } else {
    checklistitems <- ""
  }
  
  total <- total %>%
    transmute(url = shortUrl,
              title = paste0(name.card, ' [', name.list, ']'),
              text = paste(name.card, desc, checklistitems, sep = " "))
  
  if (!is.null(x)) {
    total <- total %>% filter(text %like% x)
  }
  
  cards <- total %>% pull(url)
  names(cards) <- total %>% pull(title)
  
  cards
}

#' @rdname trello
#' @export
trello_openboard <- function(board = read_secret("trello.default.board")) {
  utils::browseURL(paste0("https://trello.com/b/", board))
}

#' @rdname trello
#' @export
trello_opencard <- function() {
  content <- trello_get_card_property(project_get_current_id(ask = TRUE), "url")
  utils::browseURL(content)
}

#' @rdname trello
#' @param property property to take from card
#' @importFrom dplyr `%>%` filter pull
#' @export
trello_get_card_property <- function(card_number,
                                     property,
                                     board = read_secret("trello.default.board"),
                                     key = trello_credentials("key"),
                                     token = trello_credentials("token")) {
  trello_getcards(board, key, token) %>% filter(idShort == card_number) %>% pull(property)
}

#' @rdname trello
#' @param card_number Trello card number
#' @importFrom dplyr `%>%` filter pull
#' @export
trello_get_card_id <- function(card_number,
                               board = read_secret("trello.default.board"),
                               key = trello_credentials("key"),
                               token = trello_credentials("token")) {
  trello_getcards(board, key, token) %>% filter(idShort == card_number) %>% pull(id)
}

#' @rdname trello
#' @param card_id Trello card ID (*not* the card number)
#' @param comment the comment to set, supports markdown
#' @importFrom httr POST stop_for_status
#' @export
trello_setcomment <- function(card_id,
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
#' @importFrom dplyr `%>%` filter pull 
#' @importFrom httr POST stop_for_status DELETE
#' @export
trello_setmembers <- function(card_id,
                              member,
                              board = read_secret("trello.default.board"),
                              key = trello_credentials("key"),
                              token = trello_credentials("token")) {
  
  members <- trello_getmembers(board = board, key = key, token = token)
  members_new <- members %>%
    filter(fullName %in% member | username %in% member) %>%
    pull(id)
  members_old <- trello_getcards() %>%
    filter(id == card_id) %>%
    pull(idMembers) %>%
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
    stop_for_status(remove_members, task = paste("remove member", members[which(members$id == member_id),]$fullName))
  }
}

#' @rdname trello
#' @param duedate a [Date] object
#' @param duecomplete a [logical] to indicate whether due date is already completed
#' @importFrom httr PUT stop_for_status
#' @export
trello_setdeadline <- function(card_id,
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
#' @importFrom dplyr `%>%` filter
#' @export
trello_addtask <- function(card_id,
                           new_items = NULL,
                           checklist_name = "Taken",
                           board = read_secret("trello.default.board"),
                           key = trello_credentials("key"),
                           token = trello_credentials("token")) {
  
  checklist_id <- trello_getchecklists() %>% filter(idCard == card_id)
  # bestaat checklist al?
  if (NROW(checklist_id) == 0 | checklist_id$name[1L] != checklist_name) {
    # checklist maken
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
  
  # checklist items toevoegen
  if (all(is.null(new_items)) | length(new_items) == 0) {
    new_items <- ""
  }
  for (i in 1:length(new_items)) {
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
trello_settask_state <- function(card_id,
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
trello_movecard <- function(card_id,
                            list_id,
                            key = trello_credentials("key"),
                            token = trello_credentials("token")) {
  
  request_comments <- PUT(url = paste0("https://api.trello.com/1/cards/", card_id),
                          body = list(idList = list_id,
                                      pos = 'top', # always to top
                                      key = key,
                                      token = token),
                          encode = "json") # required for PUT()
  stop_for_status(request_comments, task = paste("move card", card_id))
}

#' @importFrom jsonlite fromJSON
#' @importFrom httr GET stop_for_status
#' @importFrom dplyr `%>%`
GET_df <- function(x) {
  result <- GET(x)
  stop_for_status(result)
  result %>%
    content(type = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE)
}
