#' Grab daily facts from "https://en.wikipedia.org/wiki/Main_Page" and print out
#' 
#' @param wiki_date str, date of article we are looking for.
#' @return NULL
#' @export
#'
get_daily_facts <- function(wiki_date = NULL) {
  
  is_archive <- !is.null(wiki_date)
  
  #dt <- check_date_input(wiki_date)
  # url_str <- get_url_from_date()
  
  
  events_list <- wiki_date %>% 
    make_request() %>% 
    read_wiki_html(is_archive) %>% 
    text_to_event_list()
  
  if (is_archive) {
    h1_cyan(glue::glue("Guess What Happened on {wiki_date}!"))
  } else {
    h1_cyan("Guess What Happened Today!")
    italic_date()
  }
  
  print_national_days(events_list)
  print_events_tbl(events_list)
  print_events_tbl(events_list)
  
  footer()
  
  return(invisible(NULL))
}
