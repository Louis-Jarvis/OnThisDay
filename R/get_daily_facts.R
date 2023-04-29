#' Grab daily facts from "https://en.wikipedia.org/wiki/Main_Page" and print out
#' 
#' @param date_str str, date of article we are looking for.
#' @return NULL
#' @export
#'
get_daily_facts <- function(date_str = NULL) {
  
  is_archive <- !is.null(date_str)
  url_str <- get_url_from_date(date_str)
  
  events_list <- url_str %>%
    rvest::read_html() %>%
    read_wiki_html(is_archive) %>% 
    text_to_event_list()
  
  if (is_archive) {
    h1_cyan(glue::glue("Guess What Happened on {date_str}!"))
  } else {
    h1_cyan("Guess What Happened Today!")
    italic_date()
  }
  
  print_national_days(events_list)
  print_events_tbl(events_list)
  print_people_table(events_list)
  
  footer(url_str)
  
  return(invisible(NULL))
}
