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
