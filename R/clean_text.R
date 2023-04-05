#' Drop all text between start of word and end of the string
#'
#' @param paragraph_str character vector, the wikipedia page content as a single string
#' @param pattern character expression to drop
#'
#' @return a string with all text between "More anniversaries" and the end excluded
#' @export
#' 
drop_substring <- function(paragraph_str, pattern) {
  
  anni_start <- paragraph_str %>% 
    stringr::str_locate(pattern) %>% 
    purrr::pluck(1)
  
  if(is.na(anni_start)) return(paragraph_str)
  
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
    drop_substring(pattern = "More anniversaries") %>%
    drop_substring(pattern = "Archive") %>%
    stringr::str_replace(
      string = .,
      pattern = "(?<=[a-z])\\.(?=[A-Z])",
      replacement = ".\n"
    ) %>%
    stringr::str_split(pattern = "\n") %>%
    unlist() %>%
    base::Filter(f = function(line) dplyr::if_else(line == "", F, T), x = .) 
  
  return(today_list_vec)
}

#' Extract out the births/deaths string as a list - one element per person + date
#'
#' @param events_list list of characters
#'
#' @return list of characters
text_to_birth_death_list <- function(events_list) {
  last_pos <- length(events_list)
  
  # convert the string into a list of birth and deaths
  persons_list <- events_list[last_pos] %>% 
    stringr::str_split(pattern = "\\)") %>% 
    purrr::pluck(1) %>% 
    as.list() %>%
    base::Filter(f = function(line) dplyr::if_else(line == "", F, T), x = .) 
  
  return(persons_list)
}

#' Convert list of strings into a tibble of events and details
#'
#' @param events_list list of characters
#'
#' @return tbl
#' @export
create_events_table <- function(events_list) {
  
  # inner
  to_row <- function(event_str) {
    event_row <- tibble::tibble(
      Year = stringr::str_extract(event_str, pattern = '(\\d{4})'),
      Details = stringr::str_split(event_str, pattern = " \032 ")[[1]][2]
    )
    return(event_row)
  }
  
  event_tbl <- events_list %>%
    base::Filter(
      f = function(x)
        stringr::str_detect(x, pattern = "\\d{4} ")
    ) %>%
    purrr::map_df(.f = to_row) %>%
    dplyr::bind_rows()
  
  return(event_tbl)
}


#' create a table of all the people who have been boren/ died on this day.
#'
#' @param events_list list of birth/death event strings
#'
#' @return tibble of births and deaths on this day
#' @export
create_births_deaths_table <- function(events_list) {
  
  to_row <- function(event_str) {
    
    # determine birth or death
    birth_or_death <- ifelse(
      grepl(event_str, pattern = ".b"), 
      cli::col_br_green("Born"), 
      cli::col_br_red("Died")
    )
    
    dob <- stringr::str_extract(event_str, pattern = "\\d{4}")
    
    name <- stringr::str_split(event_str, "\\(") %>% 
      purrr::pluck(1,1) %>% 
      stringr::str_trim()
    
    bday_tbl <- tibble::tibble(
      Person = name,
      Event = paste(birth_or_death, "-", dob)
    )
    
    return(bday_tbl)
  }
  
  bd_tbl <- events_list %>% 
    text_to_birth_death_list() %>%
    purrr::map(.f = to_row) %>%
    dplyr::bind_rows() 
  
  return(bd_tbl)
}
