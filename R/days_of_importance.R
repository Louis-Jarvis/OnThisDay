
print_national_days <- function(events_list) {
  national_days <- stringr::str_split(events_list[1], ":|;")[[1]]
  
  if(!all(is.na(national_days))) {
    nat_days_list <- national_days[ 2 : length(national_days)]
    
    h2_cyan("Festivals / National Days of Importance / Holidays")
    
    pretty_list(nat_days_list, function(txt)
      cli::cli_li(italic_li(txt)))
  }
  
  return(invisible(NULL))
}