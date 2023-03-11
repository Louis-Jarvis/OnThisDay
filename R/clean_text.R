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

#' split large string into list of sentences
#'
#' @param today_list_str character vector
#'
#' @return list of characters, each a different sentence
#' @export
text_to_event_list <- function(today_list_str) {
  
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
    base::Filter(f = function(line) dplyr::if_else(line == "", F, T), x = .) 
  
  return(today_list_vec)
}

#TODO coloured output for df

#' Convert the yyyy - event pairs into tibble rows
#'
#' @param event_str list of character vectors, each containing a yyyy - event pair
#' e.g. 2012 - A series of blasts occurred at an arms dump ...
#'
#' @return a dataframe with columns: `Year` `and Details``
events_as_row <- function(event_str) {
 
  event_row <- tibble::tibble(
    Year = stringr::str_extract(event_str, pattern = '(\\d{4})'), 
    Details = stringr::str_split(event_str, pattern = " \032 ")[[1]][2]
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
    base::Filter(f = function(x) stringr::str_detect(x, pattern = "\\d{4} ")) %>%
    purrr::map_df(.f = events_as_row) %>%
    dplyr::bind_rows() 
  
  return(event_tbl)
}

#' Extract out the births/deaths string as a list - one element per person + date
#'
#' @param events_list list of characters
#'
#' @return list of characters
to_bd_list <- function(events_list) {
  last_pos <- length(events_list)
  
  # convert the string into a list of birth and deaths
  persons_list <- events_list[last_pos] %>% 
    stringr::str_split(pattern = "\\)") %>% 
    purrr::pluck(1) %>% 
    as.list() %>%
    base::Filter(f = function(line) dplyr::if_else(line == "", F, T), x = .) 
  
  return(persons_list)
}

#' Convert birth/death event to tibble row
#'
#' @param event_str character vector with format Person (b./d. YYYY)
#'
#' @return tibble with columns Person, Event e.g.
#'   ```R  
#'   Person             Event           
#'   Gerardus           Mercator Born On This Day`
#'   Alessandro Volta   Died On This Day
#'  ````
bd_as_row <- function(event_str) {
  
  # extract the birth/death date
  #stringr::str_extract(event_str, pattern = "[bd]. \\d{4}") #TODO fix
  
  # determine birth or death
  birth_or_death <- ifelse(
    grepl(event_str, pattern = ".b"), 
    cli::col_br_green("Born"), 
    cli::col_br_red("Died On This Day")
  )
  
  dob <- stringr::str_extract(event_str, pattern = "\\d{4}")
  
  #TODO determine year or birth/death
  
  name <- stringr::str_split(event_str, "\\(") %>% 
    purrr::pluck(1,1) %>% 
    stringr::str_trim()
  
  bday_tbl <- tibble::tibble(
    Person = name,
    Event = paste(birth_or_death, "-", dob)
  )
  
  return(bday_tbl)
}


#' create a table of all the people who have been boren/ died on this day.
#'
#' @param events_list list of birth/death event strings
#'
#' @return tibble of births and deaths on this day
#' @export
create_births_deaths_table <- function(events_list) {
  
  bd_tbl <- events_list %>% 
    to_bd_list() %>%
    purrr::map(.f = bd_as_row) %>%
    dplyr::bind_rows() 
  
  return(bd_tbl)
}