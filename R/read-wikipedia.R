##TODO 
# date today
# yyyy - event description as a tibble
# Famours births and deaths
# Return the url

#TODO give option to only print one fact at a time
##TODO use global config to set defaults

#TODO coloured output for df

#' Convert the yyyy - event pairs into tibble rows
#'
#' @param event_str list of character vectors, each containing a yyyy - event pair
#' e.g. 2012 – A series of blasts occurred at an arms dump ...
#'
#' @return a dataframe with columns: `Year` `and Details``
#' @export
events_to_table <- function(event_str) {
  
  event_row <- tibble::tibble(
    Year = stringr::str_extract(event_str, pattern = '(\\d{4})'), 
    Details = stringr::str_split(event_str, pattern = " – ")[[1]][2]
  )
  
  return(event_row)
}

#' Convert list of strings into a tibble of events and details
#'
#' @param events_list list of characters
#'
#' @return tbl
#' @export
create_events_table <- function(events_list) {
  
  event_tbl <- events_list %>%
    base::Filter(f = function(x) stringr::str_detect(x, pattern = "\\d{4} – ")) %>%
    purrr::map_df(.f = events_to_table) %>%
    dplyr::bind_rows() 
  
  return(event_tbl)
}

extract_birthdays <- function(events_list) {
  
  #browser()
  last_pos <- length(events_list)
  
  # convert the string into a list of birth and deaths
  bday_str <- events_list[last_pos] %>% 
    stringr::str_replace_all(pattern = "\\)", replacement = "\\)\\n") %>%
    stringr::str_split(pattern = "\\n")
  
  # # extract the birth/death date
  # stringr::str_extract("Stephen III of Hungary  \\(b. 1172", pattern = "[bd]. \\d{4}") #TODO fix
  # 
  # # determine birth or death
  # birth_or_death <- ifelse(grepl(pattern = ".b"), "Born On This Day", "Died On This Day")
  # 
  # name <- stringr::str_split(s, "\\(") %>% 
  #   purrr::pluck(1) %>% 
  #   stringr::str_trim()
  # 
  # tibble::tibble(
  #   Person = name,
  #   Event = birth_or_death
  # )
  
  return(bday_str)
}

birthdays_to_table <- function(event_str) {
  # extract the birth/death date
  stringr::str_extract(event_str, pattern = "[bd]. \\d{4}") #TODO fix
  
  # determine birth or death
  birth_or_death <- ifelse(
    grepl(event_str, pattern = ".b"), 
    "Born On This Day", 
    "Died On This Day"
    )
  
  name <- stringr::str_split(event_str, "\\(") %>% 
    purrr::pluck(1) %>% 
    stringr::str_trim()
  
  bday_tbl <- tibble::tibble(
    Person = name,
    Event = birth_or_death
  )
  
  return(bday_tbl)
}

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

# remove the "More anniversaries" onwards section
##str_end <- stringr::str_length(today_list_str)


# drop empty string elements ("")

# pull out the date
#date_day <- today_list_vec[1]

#TODO add args to specify which outputs are wanted
get_daily_facts <- function() {
  events_list <- read_wiki_html() %>%
    text_to_vec()
  
  cli::cli_h1("Todays's date")
  date_day <- events_list[1]
  
  cli::cli_h2("On This Day...")
  events_tbl <- events_list %>% 
    create_events_table() 
  
  print(events_tbl)
  cli::cli_par()
  
  cli::cli_h1("Famous Births and Deaths")
}
  

