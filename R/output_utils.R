#' Factory function to create pretty printing cli functions
#'
#' @param cli_header cli text object.
#' @param cli_col cli colour.
#'
#' @return function to pretty print text to console
#' @seealso headers; [cli::cli_h1()], [cli::cli_h2()], [cli::cli_text()]; colours
#' [cli::col_green()], [cli::col_cyan()]
#' 
make_pretty_output <- function(cli_header, cli_col) {
  
  inner <- function(text) {
    cli_header(cli_col(text))
  }
  
  return(inner)
}

pretty_list <- function(items, f, ...) {
  ulid <- cli::cli_ul()
  lapply(items, function(x, ...) f(x), ...)
  cli::cli_end(ulid)
}

footer <- function(url_str = "https://en.wikipedia.org/wiki/Main_Page") {
  cli::cli_text("See for yourself at {.url {url_str}}")
  hline <- cli::cli_div(theme = list(rule = list(color = "cyan")))
  cli::cli_rule("")
  cli::cli_end(hline)
}

# Styled output functions ------------------------------------------------------

h1_cyan <- make_pretty_output(cli::cli_h1, cli::col_cyan)
h1_green <- make_pretty_output(cli::cli_h1, cli::col_green)
h2_cyan <- make_pretty_output(cli::cli_h2, cli::col_cyan)

italic_li <- make_pretty_output(cli::style_italic, cli::col_yellow)
italic_date <- make_pretty_output(cli::cli_text,
                                  function(txt)
                                    paste("Today's Date:", cli::style_italic(format(
                                      Sys.Date(), "%A %d %B %Y"
                                    ))))

# Format tables-----------------------------------------------------------------

#' Convert a tibble into cli list output
#'
#' @param event_tbl tibble, table of events with a date and event string.
#' @param header_text character, text for section header.
#' @param title_col character, name of col that forms the header.
#' @param text_col character, name of event description column.
#'
#' @return NULL
tbl_to_cli_output <- function(event_tbl,
                              header_text,
                              title_col = "Year",
                              text_col = "Details") {
    if (nrow(event_tbl) > 0)
      h2_cyan(header_text)
    
    pretty_list(seq_len(nrow(event_tbl)), function(i, events_tbl) {
      yr <- event_tbl[i, title_col]
      txt <- event_tbl[i, text_col]
      
      cli::cli_li(italic_li(paste0("\t", yr)))
      cli::cli_text(txt)
      cli::cli_par()
      
    }, event_tbl)
  }