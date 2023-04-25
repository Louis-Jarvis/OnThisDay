

#TODO give option to only print one fact at a time
#TODO use global config to set defaults
#TODO wrap in a try execpt

#' Make request to Wikipedia and return page as a string
#'
#' @param wiki_date string, giving date of desired article.
#
#' @return multiline string
#' 
make_request <- function(wiki_date = NULL) {
  
  if (!is.null(wiki_date)) {
    
    num_str <- stringr::str_extract(wiki_date, "[[:digit:]]+")
    mnth_str <- stringr::str_extract(stringr::str_to_title(wiki_date), paste(month.name, collapse="|"))
  
    if(isFALSE(mnth_str %in% month.name) && isFALSE(mnth_str %in% month.abb)) {
        cli::cli_alert_danger("Invalid Month")
    }
    
    num_str <- as.integer(num_str)
    mnth_str <- stringr::str_to_title(mnth_str)
    
    # see https://stackoverflow.com/questions/13450360/how-to-validate-date-in-r
    d <- try(as.Date(paste(num_str, mnth_str), format="%d %B"))
    if("try-error" %in% class(d) || is.na(d)) {
      cli::cli_alert_danger("Invalid Input: {num_str} {mnth_str} is not a real date!")
    }
    
    cli::cli_text("Retrieving info for: {.field {num_str} {mnth_str}}")
    main_page <- rvest::read_html(glue::glue("https://en.wikipedia.org/wiki/Wikipedia:Selected_anniversaries/{mnth_str}_{num_str}")) 
  
  } else {
    main_page <- main_page <- rvest::read_html("https://en.wikipedia.org/wiki/Main_Page")    
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

    #TODO simplify this into one function
    
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
#' @param wiki_date str, date of article we are looking for.
#' @return NULL
#' @export
#'
get_daily_facts <- function(wiki_date = NULL) {
  
  is_archive <- !is.null(wiki_date)
 
  events_list <-  make_request(wiki_date) %>% 
    read_wiki_html(is_archive) %>% 
    text_to_event_list()
  
  # Current Date
  # cli::cli_h1(cli::col_green(
  #   glue::glue("Guess What Happened On {format(Sys.Date(),'%B %d')}!")
  # ))
  h1_cyan("Guess What Happened Today!")
  italic_date()
  # cli::cli_text(paste("Today's Date:", cli::style_italic(format(
  #   Sys.Date(), "%A %d %B %Y"
  # ))))
  
  # National Holidays
  national_days <- stringr::str_split_i(events_list[1], pattern = ":", i = 2) #[[1]][2]
  
  if(!is.na(national_days)) {
    #cli::cli_h2(cli::col_cyan("Festivals / National Days of Importance / Holidays"))
    h2_cyan("Festivals / National Days of Importance / Holidays")
    
    nat_days_list <- stringr::str_split_i(national_days, pattern = ";", i = 1) %>% #[[1]] 
      stringr::str_squish()
    
    ulid <- cli::cli_ul()
    for (day in nat_days_list) {
      cli::cli_li(day) 
    }
    cli::cli_end(ulid)
  }
  
  ## Historical Events
  events_tbl <- events_list %>% 
    create_events_table() %>%
    tbl_to_cli_output( 
      header_text = "On This Day...", 
      title_col = "Year", 
      text_col = "Details"
      )

  ## Famous Figures
  famous_ppl_tbl <- events_list %>% 
    create_births_deaths_table() %>%
    tbl_to_cli_output(
      header_text = "Famous Births/Deaths", 
      title_col = "Person", 
      text_col = "Event"
      )
  
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
  
  if (nrow(event_tbl) > 0) cli::cli_h2(cli::col_cyan(header_text))
  
  lid <- cli::cli_ul()
  
  for (i in seq_len(nrow(event_tbl))) {
    
    yr <- event_tbl[i, title_col]
    txt <- event_tbl[i, text_col]
    
    #cli::cli_li(cli::style_italic(cli::col_yellow(paste0("\t", yr))))
    cli::cli_li(
      cli::style_italic(
        cli::col_yellow(
          paste0("\t", yr)
          )
        )
      )
    cli::cli_text(txt)
    cli::cli_par()
  }
  
  cli::cli_end(lid)
}

