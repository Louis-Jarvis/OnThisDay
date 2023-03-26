##TODO 
# date today
# yyyy - event description as a tibble
# Famours births and deaths
# Return the url

#TODO give option to only print one fact at a time
##TODO use global config to set defaults


#TODO write a function to get historic ones

#TODO wrap in a try execpt

#' Read in Wikipedia html and convert to string
#' @param wiki_date str in the format %d %B specifying the date of the archived article to retrieve
#'
#' @return character vector with all the wikipedia contents
#' @export
read_wiki_html <- function(wiki_date = NULL) {
  
  if (!is.null(wiki_date)) {
    num_str <- stringr::str_extract(wiki_date, "[[:digit:]]+")
    mnth_str <- stringr::str_extract(wiki_date, paste(month.name, collapse="|"))
    
   if(isFALSE(stringr::str_to_title(mnth_str) %in% month.name) && isFALSE(stringr::str_to_title(mnth_str) %in% month.abb)) {
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
    days_str <- main_page %>% rvest::html_element(".mw-body-content.mw-content-ltr .mw-parser-output > p") %>% rvest::html_text()
    events_str <- main_page %>% rvest::html_element(".mw-body-content.mw-content-ltr .mw-parser-output > ul") %>% rvest::html_text()
    ppl_str <- main_page %>% rvest::html_elements(".mw-body-content.mw-content-ltr .mw-parser-output > .hlist") %>% rvest::html_text()
    
    today_list_str <- c(days_str, events_str, ppl_str) %>%
      paste0(collapse="") %>%
      stringi::stri_enc_toascii()
    
  } else {
    
    main_page <- rvest::read_html("https://en.wikipedia.org/wiki/Main_Page")
    
    today_list_str <- main_page %>% 
      rvest::html_element("#mp-otd") %>% # use css selectors to obtain div
      rvest::html_text() %>%
      stringi::stri_enc_toascii()
  }
  
  return(today_list_str)
}

#TODO add args to specify which outputs are wanted
#TODO convert dfs into cli_ unordered list


#' Grab daily facts from "https://en.wikipedia.org/wiki/Main_Page" and print out
#' @inheritParams read_wiki_html
#' @return NULL
#' @export
#'
get_daily_facts <- function(wiki_date = NULL) {
  
  events_list <- read_wiki_html(wiki_date) %>% text_to_event_list()
  
  # Current Date
  cli::cli_h1(cli::col_green(glue::glue("Guess What Happened On {format(Sys.Date(),'%B %d')}!")))
  cli::cli_text(paste("Today's Date:", cli::style_italic(format(Sys.Date(),"%A %d %B %Y"))))
  
  ## National Holidays
  national_days <- stringr::str_split(events_list[1], pattern = ":")[[1]][2]
  
  if(!is.na(national_days)) {
    cli::cli_h2(cli::col_cyan("Festivals / National Days of Importance / Holidays"))  
    
    nat_days_list <- stringr::str_split(national_days, pattern = ";")[[1]] %>% 
      stringr::str_squish()
    
    ulid <- cli::cli_ul()
    for (day in nat_days_list) {
      cli::cli_li(day) 
    }
    cli::cli_end(ulid)
  }
  
  ## Historiocal Events
  events_tbl <- events_list %>% create_events_table()
  tbl_to_ul_output(
    events_tbl, 
    header_text = "On This Day...", 
    title_col = "Year", 
    text_col = "Details"
    )

  ## Famous Figures
  famous_ppl_tbl <- events_list %>% create_births_deaths_table()
  tbl_to_ul_output(
    famous_ppl_tbl, 
    header_text = "Famous Births/Deaths", 
    title_col = "Person", 
    text_col = "Event"
    )
  
  cli::cli_text("See for yourself at {.url https://en.wikipedia.org/wiki/Main_Page}")
  
  d <- cli::cli_div(theme = list(rule = list(color = "cyan")))
  cli::cli_rule("")
  cli::cli_end(d)
  
  return(invisible(NULL))
}

#TODO add skull or baby emoji for birth/death

tbl_to_ul_output <- function(event_tbl, header_text, title_col = "Year", text_col = "Details"){
  
  if(nrow(event_tbl) > 0) {
    cli::cli_h2(cli::col_cyan(header_text))
  }
  
  lid <- cli::cli_ul()
  
  for (i in seq_len(nrow(event_tbl))) {
    yr = event_tbl[i, title_col]
    txt = event_tbl[i, text_col]
    
    cli::cli_li(cli::style_italic(cli::col_yellow(paste0("\t",yr))))
    cli::cli_text(txt)
    cli::cli_par()
  }
  
  cli::cli_end(lid)
}

