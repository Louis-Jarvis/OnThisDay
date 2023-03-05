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
    base::Filter(f = function(line) dplyr::if_else(line == "", F, T), x = .) 
  
  return(today_list_vec)
}