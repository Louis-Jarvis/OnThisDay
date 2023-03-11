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
  
  cli::cli_h1(cli::col_green(glue::glue("Guess What Happened On {format(Sys.Date(),'%B %d')}!")))
  cli::cli_text(paste("Today's Date:", cli::style_italic(format(Sys.Date(),"%A %d %B %Y"))))
  
  national_days <- stringr::str_split(events_list[1], pattern = ":")[[1]][2]
  if(!is.na(national_days)) {
    cli::cli_h2(cli::col_cyan("Festivals / National Days of Importance / Holidays"))  
    
    nat_days_list <- stringr::str_split(national_days, pattern = ";")[[1]] %>% stringr::str_squish()
    ulid <- cli::cli_ul()
    
    for (day in nat_days_list) {
      cli::cli_li(day) 
    }
    cli::cli_end(ulid)
    
  }
  
  events_tbl <- events_list %>% create_events_table()
  
  if(nrow(events_tbl) > 0) {
    cli::cli_h2(cli::col_cyan("On This Day..."))
    tbl_to_ul_output(events_tbl, title_col = "Year", text_col = "Details")
  }

  famous_ppl_tbl <- events_list %>% create_births_deaths_table()
  
  if(nrow(famous_ppl_tbl) > 0) {
    cli::cli_h2(cli::col_cyan("Famous Births/Deaths"))
    tbl_to_ul_output(famous_ppl_tbl, title_col = "Person", text_col = "Event")
  }
  
  cli::cli_text("See for yourself at {.url https://en.wikipedia.org/wiki/Main_Page}")
  
  d <- cli::cli_div(theme = list(rule = list(color = "cyan")))
  cli::cli_rule("")
  cli::cli_end(d)
  
  return(invisible(NULL))
}

#TODO add skull or baby emoji for birth/death

tbl_to_ul_output <- function(event_tbl, title_col = "Year", text_col = "Details"){
  
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

