##TODO 
# date today
# yyyy - event description as a tibble
# Famours births and deaths
# Return the url


#' Drop all text starting at "More anniversaries"
#'
#' @param paragraph_str character vector, the wikipedia page content as a single string
#'
#' @return a string with all text between "More anniversaries" and the end excluded
#' @export
drop_more_anniveraries <- function(paragraph_str) {
  
  anni_start <- paragraph_str %>% 
    stringr::str_locate(pattern = "More anniversaries") %>% 
    purrr::pluck(1)
  
  str_stripped <- paragraph_str %>% 
    stringr::str_sub(start = 1, end = anni_start-1) 
  
  return(str_stripped)
}


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



#' split large string into list of sentences
#'
#' @param today_list_str character vector
#'
#' @return list of characters, each a different sentence
#' @export
text_to_vec <- function(today_list_str) {
  
  # this stops multiple sentences being put on the same line e.g. 
  # foo.Bar -- > foo.\nBar, which can then be split into multiple lines
  
  today_list_vec <- today_list_str %>%
    drop_more_anniveraries() %>%
    stringr::str_replace(
      string = .,
      pattern = "(?<=[a-z])\\.(?=[A-Z])",
      replacement = ".\n"
    ) %>%
    stringr::str_split(pattern = "\n") %>%
    purrr::pluck(1) %>%
    base::Filter(f = function(line) dplyr::ifelse(line == "", F, T), x = .) 
  
  return(today_list_vec)
}


# drop empty string elements ("")

# pull out the date
#date_day <- today_list_vec[1]

