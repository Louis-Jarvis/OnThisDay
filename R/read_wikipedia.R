#' Check the wiki_date is valid
#'
#' @param wiki_date string, giving date of desired article.
#
#' @return multiline string
#' 
#' TODO fix return 
check_date_input <- function(wiki_date) {
  wiki_date <- stringr::str_to_title(wiki_date)
  num_str <- stringr::str_extract(wiki_date, "[[:digit:]]+")
  mnth_str <-
    stringr::str_extract(wiki_date, paste(month.name, collapse = "|"))
  
  date_str <-
    try(as.Date(paste(num_str, mnth_str), format = "%d %B"))
  
  if ("try-error" %in% class(date_str) || is.na(date_str)) {
    stop(glue::glue("Invalid Input: \'{wiki_date}'\ is not a real date!"))
  }
  
  return(list(
    num_str = num_str,
    mnth_str = stringr::str_to_title(mnth_str)
  ))
}

get_url_from_date <- function(date_string) {
  dt <- check_date_input(date_string)
  
  BASE_URL <- "https://en.wikipedia.org/wiki"
  
  if (!is.null(date_string)) {
    cli::cli_alert_info("Retrieving info for: {.field {dt$num_str} {dt$mnth_str}}")
    url_str <- glue::glue("{BASE_URL}/Wikipedia:Selected_anniversaries/{dt$mnth_str}_{dt$num_str}")
  } else {
    url_str <- glue::glue("{BASE_URL}/Main_Page")
  }
 
  return(url_str) 
}

#' Make request to Wikipedia and return page as a string
#'
#' @inheritParams check_date_input
#
#' @return multiline string
#' 
make_request <- function(wiki_date = NULL) {

  BASE_URL <- "https://en.wikipedia.org/wiki"
  
  if (!is.null(wiki_date)) {
    
    dt <- check_date_input(wiki_date)
    
    cli::cli_alert_info("Retrieving info for: {.field {dt$num_str} {dt$mnth_str}}")
    main_page <-
      rvest::read_html(
        glue::glue(
          "{BASE_URL}/Wikipedia:Selected_anniversaries/{dt$mnth_str}_{dt$num_str}"
        )
      )
    
  } else {
    main_page <- rvest::read_html(glue::glue("{BASE_URL}/Main_Page"))
  }

  return(main_page)
}


#' Read in Wikipedia html and convert to string
#' @param main_page xmlnode_tree, output from [rvest::read_html()]
#' @param archive logical, whether or not we are fetching events from an archived page
#'
#' @return character vector with all the wikipedia contents
#' @export
read_wiki_html <- function(main_page, archive) {

  if (archive) {
    
    CSS_SELECTOR = ".mw-body-content.mw-content-ltr .mw-parser-output"
    
    today_list_str <- purrr::map_chr(
      .x = c("p", "ul", ".hlist"),
      .f = function(el) {
        el_str <- main_page %>%
          rvest::html_element(glue::glue("{CSS_SELECTOR} > {el}")) %>%
          rvest::html_text()
      }
    ) %>%
      paste0(collapse = "") %>%
      stringi::stri_enc_toascii()
    
  } else {
    
    today_list_str <- main_page %>% 
      rvest::html_element("#mp-otd") %>% # use css selectors to obtain div
      rvest::html_text() %>%
      stringi::stri_enc_toascii()
  }
  
  return(today_list_str)
}

