
print_national_days <- function(events_list) {
  # National Holidays
  national_days <- stringr::str_split_i(events_list[1], ":", 2)
  
  if(!is.na(national_days)) {
    h2_cyan("Festivals / National Days of Importance / Holidays")
    nat_days_list <- stringr::str_split_i(national_days, ";", 1) %>% trimws()
    pretty_list(nat_days_list, function(txt) cli::cli_li(italic_li(txt)))
  }
}