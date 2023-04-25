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
    cli_header(cli_col(glue::glue(text)))
  }
  
  return(inner)
}

# Styled output functions ------------------------------------------------------

h1_cyan <- make_pretty_output(cli::cli_h1, cli::col_cyan)
h1_green <- make_pretty_output(cli::cli_h1, cli::col_green)
h2_cyan <- make_pretty_output(cli::cli_h2, cli::col_cyan)

italic_date <- make_pretty_output(cli::cli_text,
                                  function(txt)
                                    paste("Today's Date:", cli::style_italic(format(
                                      Sys.Date(), "%A %d %B %Y"
                                    ))))
