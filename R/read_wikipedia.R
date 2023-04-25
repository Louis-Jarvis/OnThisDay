#' Check the wiki_date is valid
#'
#' @param wiki_date string, giving date of desired article.
#
#' @return multiline string
#' 
check_date_input <- function(wiki_date) {
  wiki_date <- stringr::str_to_title(wiki_date)
  num_str <- stringr::str_extract(wiki_date, "[[:digit:]]+")
  mnth_str <-
    stringr::str_extract(wiki_date, paste(month.name, collapse = "|"))
  
  date_str <-
    try(as.Date(paste(num_str, mnth_str), format = "%d %B"))
  
  if ("try-error" %in% class(date_str) || is.na(date_str)) {
    stop(glue::glue("Invalid Input: \'{wiki_date}'\ is not a real date!"))
  }
  
  return(list(
    num_str = num_str,
    mnth_str = stringr::str_to_title(mnth_str)
  ))
}

#' Make request to Wikipedia and return page as a string
#'
#' @inheritParams check_date_input
#
#' @return multiline string
#' 
make_request <- function(wiki_date = NULL) {

  BASE_URL <- "https://en.wikipedia.org/wiki"
  
  if (!is.null(wiki_date)) {
    
    dt <- check_date_input(wiki_date)
    
    cli::cli_alert_info("Retrieving info for: {.field {dt$num_str} {dt$mnth_str}}")
    main_page <-
      rvest::read_html(
        glue::glue(
          "{BASE_URL}/Wikipedia:Selected_anniversaries/{dt$mnth_str}_{dt$num_str}"
        )
      )
    
  } else {
    main_page <- main_page <- rvest::read_html(glue::glue("{BASE_URL}/Main_Page"))
  }

  return(main_page)
}


#' Read in Wikipedia html and convert to string
#' @param main_page xmlnode_tree, output from [rvest::read_html()]
#' @param archive logical, whether or not we are fetching events from an archived page
#'
#' @return character vector with all the wikipedia contents
#' @export
read_wiki_html <- function(main_page, archive) {

  if (archive) {
    
    CSS_SELECTOR = ".mw-body-content.mw-content-ltr .mw-parser-output"
    
    today_list_str <- purrr::map_chr(
      .x = c("p", "ul", ".hlist"),
      .f = function(el) {
        el_str <- main_page %>%
          rvest::html_element(glue::glue("{CSS_SELECTOR} > {el}")) %>%
          rvest::html_text()
      }
    ) %>%
      paste0(collapse = "") %>%
      stringi::stri_enc_toascii()
    
  } else {
    
    today_list_str <- main_page %>% 
      rvest::html_element("#mp-otd") %>% # use css selectors to obtain div
      rvest::html_text() %>%
      stringi::stri_enc_toascii()
  }
  
  return(today_list_str)
}

#' Grab daily facts from "https://en.wikipedia.org/wiki/Main_Page" and print out
#' 
#' @param wiki_date str, date of article we are looking for.
#' @return NULL
#' @export
#'
get_daily_facts <- function(wiki_date = NULL) {
  
  is_archive <- !is.null(wiki_date)
 
  events_list <- make_request(wiki_date) %>% 
    read_wiki_html(is_archive) %>% 
    text_to_event_list()
  
  if (is_archive) {
    h1_cyan(glue::glue("Guess What Happened on {wiki_date}!"))
  } else {
    h1_cyan("Guess What Happened Today!")
    italic_date()
  }
  
  # National Holidays
  national_days <- stringr::str_split_i(events_list[1], ":", 2)
  
  if(!is.na(national_days)) {
    
    h2_cyan("Festivals / National Days of Importance / Holidays")
    nat_days_list <- stringr::str_split_i(national_days, ";", 1) %>% trimws()
    pretty_list(nat_days_list, function(txt) cli::cli_li(italic_li(txt)))
  }
    
  ## Historical Events
  events_tbl <- events_list %>% 
    create_events_table() %>%
    tbl_to_cli_output(header_text = "On This Day...",
                      title_col = "Year",
                      text_col = "Details")

  ## Famous Figures
  famous_ppl_tbl <- events_list %>% 
    create_births_deaths_table() %>%
    tbl_to_cli_output(header_text = "Famous Births/Deaths",
                      title_col = "Person",
                      text_col = "Event")
  
  #TODO replace this with the archived page
  cli::cli_text("See for yourself at {.url https://en.wikipedia.org/wiki/Main_Page}")
  
  d <- cli::cli_div(theme = list(rule = list(color = "cyan")))
  cli::cli_rule("")
  cli::cli_end(d)
  
  return(invisible(NULL))
}


#' Convert a tibble into cli list output
#'
#' @param event_tbl tibble, table of events with a date and event string.
#' @param header_text character, text for section header.
#' @param title_col character, name of col that forms the header.
#' @param text_col character, name of event description column.
#'
#' @return NULL
tbl_to_cli_output <- function(event_tbl, header_text, title_col = "Year", text_col = "Details"){
  
  if (nrow(event_tbl) > 0) h2_cyan(header_text)
  
  pretty_list(seq_len(nrow(event_tbl)), function(i, events_tbl) {
    
    yr <- event_tbl[i, title_col]
    txt <- event_tbl[i, text_col]
    
    cli::cli_li(italic_li(paste0("\t", yr)))
    cli::cli_text(txt)
    cli::cli_par()
    
  }, event_tbl)
}

