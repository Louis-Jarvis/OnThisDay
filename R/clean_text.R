#' Remove all text after "More anniversaries
#'
#' @param article_txt character vector of article text.
#'
#' @return cleaned string
#' 
remove_footer <- function(article_txt) {
  
  pos <- stringr::str_locate(article_txt, "More anniversaries")
  
  if (!all(is.na(pos))) {
    res <- substr(article_txt, 1, pos[1]-1)
  } else {
    res <- article_txt
  }
  return(res)
}

#' Remove substring from article text
#'
#' @param article_txt character vector of article text.
#' @param substr pattern to remove.
#'
#' @return cleaned string
#' 
remove_substr <- function(article_txt, substr) {
  return(gsub(pattern = substr, replacement = "", x = article_txt))
}

#' split large string into list of sentences
#'
#' @param today_list_str character vector
#'
#' @return list of characters, each a different sentence
text_to_event_list <- function(today_list_str) {
  
  period_bw_letters <- "(?<=[a-z])\\.(?=[A-Z]|[a-z)])"
  
  # this stops multiple sentences being put on the same line e.g. 
  # foo.Bar -- > foo.\nBar, which can then be split into multiple lines
  today_list_vec <- today_list_str %>%
    remove_footer() %>%
    remove_substr("\\(pictured\\)") %>%
    stringr::str_replace(
      pattern = period_bw_letters,
      replacement = ".\n"
    ) %>%
    stringr::str_split(pattern = "\n") %>%
    unlist() %>%
    base::Filter(f = function(line) dplyr::if_else(line == "", F, T), x = .) 
  
  return(today_list_vec)
}

#' Extract out the births/deaths string as a list - one element per person + date
#'
#' @param events_list list of character vectors - each a representing a different paragraph.
#'
#' @return list of characters
text_to_birth_death_list <- function(events_list) {
  last_pos <- length(events_list)
  
  # convert the string into a list of birth and deaths
  persons_list <- events_list[last_pos] %>% 
    stringr::str_split(pattern = "\\)", simplify = T) %>% 
    as.list() %>%
    base::Filter(f = function(line) dplyr::if_else(line == "", F, T), x = .) 
  
  return(persons_list)
}

#' Convert list of strings into a tibble of events and details
#'
#' @param events_list list of characters
#'
#' @return tbl
create_events_table <- function(events_list) {
  
  utf8_hyphen <- " \032 "
  
  # inner
  to_row <- function(event_str) {
    
    event_row <- tibble::tibble(
      Year = stringr::str_extract(event_str, pattern = '(\\d{1,4})'),
      Details = stringr::str_split_i(event_str, pattern = utf8_hyphen, i = 2)
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


#' create a table of all the people who have been boren/ died on this day
#'
#' @param events_list list of birth/death event strings
#'
#' @return tibble of births and deaths on this day
create_births_deaths_table <- function(events_list) {
  
  to_row <- function(event_str) {
    
    # determine birth or death
    birth_or_death <- ifelse(
      grepl(event_str, pattern = ".b"), 
      cli::col_br_green("Born"), 
      cli::col_br_red("Died")
    )
    
    dob <- stringr::str_extract(event_str, pattern = "\\d{1,4}")
    
    name <- stringr::str_split_i(event_str, "\\(", i = 1) %>% 
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
