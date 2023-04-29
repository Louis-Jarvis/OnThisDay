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

print_events_tbl <- function(events_list) {
  events_tbl <- events_list %>% 
    create_events_table() %>%
    tbl_to_cli_output(header_text = "On This Day...",
                      title_col = "Year",
                      text_col = "Details")
  
  return(invisible(events_tbl))
}