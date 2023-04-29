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

print_people_table <- function(events_list) {
  famous_ppl_tbl <- events_list %>% 
    create_births_deaths_table() %>%
    tbl_to_cli_output(header_text = "Famous Births/Deaths",
                      title_col = "Person",
                      text_col = "Event")
  
  return(invisible(famous_ppl_tbl))
}