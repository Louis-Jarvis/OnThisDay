##TODO 
# date today
# yyyy - event description as a tibble
# Famours births and deaths
# Return the url


#TODO wrap in a try execpt
main_page <- rvest::read_html("https://en.wikipedia.org/wiki/Main_Page")

today_list_str <- main_page %>% 
  rvest::html_element("#mp-otd") %>% # use css selectors to obtain div
  rvest::html_text() #[A-za-z0-9]

# remove the "More anniversaries" onwards section
##str_end <- stringr::str_length(today_list_str)

drop_more_anniveraries <- function(paragraph_str) {
  
  anni_start <- paragraph_str %>% 
    stringr::str_locate(pattern = "More anniversaries") %>% 
    purrr::pluck(1)
  
  str_stripped <- paragraph_str %>% 
    stringr::str_sub(start = 1, end = anni_start-1) 
  
  return(str_stripped)
}

today_list_vec <- today_list_str %>%
  drop_more_anniveraries() %>%
  stringr::str_replace(. , pattern = "(?<=[a-z])\\.(?=[A-Z])", replacement = ".\n") %>% # this stops multiple sentences being put on the same line e.g. foo.Bar -- > foo.\nBar
  stringr::str_split(pattern = "\n") %>%
  purrr::pluck(1) %>%
  base::Filter(f = function(line) ifelse(line == "", F, T), x = .)

# drop empty string elements ("")

# pull out the date
date_day <- today_list_vec[1]

events_to_table <- function(event_str) {
  
   ##if (stringr::str_detect(pattern = "[0-9+] – ")) {
     
     event_row <- tibble::tibble(
       Year = stringr::str_extract(event_str, pattern = '(\\d{4})'),
       Details = stringr::str_split(event_str, pattern = " – ")[[1]][2]
     )
  
  return(event_row)
}

# today_list_vec %>%
#   base::Filter(f = function(x) stringr::str_detect(x, pattern = "\\d{4} – ")) %>%
#   purrr::map_df(.f = events_to_table) %>%
#   dplyr::bind_rows()
