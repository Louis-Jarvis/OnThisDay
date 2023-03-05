##TODO 
# date today
# yyyy - event description as a tibble
# Famours births and deaths
# Return the url

#TODO give option to only print one fact at a time
##TODO use global config to set defaults




#TODO wrap in a try execpt

#' Read in Wikipedia html and convert to string
#'
#' @return character vector with all the wikipedia contents
#' @export
read_wiki_html <- function() {
  main_page <- rvest::read_html("https://en.wikipedia.org/wiki/Main_Page")
  
  today_list_str <- main_page %>% 
    rvest::html_element("#mp-otd") %>% # use css selectors to obtain div
    rvest::html_text() 
  
  return(today_list_str)
}

#TODO add args to specify which outputs are wanted
#TODO convert dfs into cli_ unordered list


#' Grab daily facts from "https://en.wikipedia.org/wiki/Main_Page" and print out
#'
#' @return NULL
#' @export
#'
get_daily_facts <- function() {
  
  events_list <- read_wiki_html() %>% text_to_event_list()
  
  cli::cli_h1("Guess What Happened On This Day!")
  ##print(glue::glue("{events_list[1]} {substr(Sys.Date(), 1, 4)}"))
  
  cli::cli_h2("Festivals / National Days of Importance / Holidays")
  cli::cli_text(events_list[1])
  ##TODO print the festival string
  
  cli::cli_h2("On This Day...")
  events_tbl <- events_list %>% create_events_table() 
  
  print(events_tbl)
  cli::cli_text("")
  
  cli::cli_h2("Famous People")
  famous_ppl_tbl <- events_list %>% create_births_deaths_table()
  
  print(famous_ppl_tbl)
  
  #TODO return URL as clickable link
  
  return(invisible(NULL))
}
  

