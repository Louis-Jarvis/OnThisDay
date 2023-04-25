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
  
  SPACE_BW_LETTERS <- "(?<=[a-z])\\.(?=[A-Z]|[a-z)])" # foo.Bar -- > foo.\nBar
  
  today_list_vec <- today_list_str %>%
    remove_footer() %>%
    remove_substr("\\(pictured\\)") %>%
    stringr::str_replace(SPACE_BW_LETTERS,".\n") %>%
    stringr::str_split("\n") %>%
    unlist() %>%
    base::Filter(f = function(line) ifelse(line == "", F, T)) 
  
  return(today_list_vec)
}

#' Extract out the births/deaths string as a list - one element per person + date
#'
#' @param events_list list of character vectors - each a representing a different paragraph.
#'
#' @return list of characters
text_to_birth_death_list <- function(events_list) {
  last_pos <- length(events_list)
  
  persons_list <- events_list[last_pos] %>% 
    stringr::str_split("\\)", simplify = T) %>% 
    as.list() %>%
    base::Filter(f = function(line) ifelse(line == "", F, T)) 
  
  return(persons_list)
}

#' Convert list of strings into a tibble of events and details
#'
#' @param events_list list of characters
#'
#' @return tbl
create_events_table <- function(events_list) {
  
  UTF8_HYPHEN <- " \032 "
  YEAR_REGEX <- "\\d{1,4}"
  
  to_row <- function(event_str) {
    event_row <- tibble::tibble(
      Year = stringr::str_extract(event_str, paste0("(", YEAR_REGEX, ")")),
      Details = stringr::str_split_i(event_str, UTF8_HYPHEN, 2)
    )
    return(event_row)
  }
  
  event_tbl <- events_list %>%
    base::Filter(f = function(x) stringr::str_detect(x, paste0(YEAR_REGEX, " "))) %>%
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
  
  YEAR_REGEX <- "\\d{1,4}"
  
  to_row <- function(event_str) {
    is_birth_or_death <- ifelse(stringr::str_detect(event_str, ".b"),
                                cli::col_br_green("Born"),
                                cli::col_br_red("Died"))
    
    dob <- stringr::str_extract(event_str, YEAR_REGEX)
    name <- stringr::str_split_i(event_str, "\\(", 1) %>% trimws()
    bday_tbl <- tibble::tibble(Person = name,
                               Event = paste(is_birth_or_death, "-", dob))
    return(bday_tbl)
  }
  
  bd_tbl <- events_list %>% 
    text_to_birth_death_list() %>%
    purrr::map(.f = to_row) %>%
    dplyr::bind_rows() 
  
  return(bd_tbl)
}
