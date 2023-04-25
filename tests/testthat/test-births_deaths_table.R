events_list <- list(
  "Not an event",
  "Important national holiday",
  "2020 - some fake fact",
  "John Doe (b. 684)Jane Doe (d.1984)"
)

test_that("Test that text_to_birth_death_list", {
  
  exp_list <- list("John Doe (b. 684","Jane Doe (d.1984")
  
  res_list <- text_to_birth_death_list(events_list)
  
  testthat::expect_equal(exp_list, res_list)
})

test_that("Test that create_births_deaths_table works as expected", {
  
  exp_bd_tbl <- tibble::tribble(
    ~Person, ~Event,
    "John Doe", paste(cli::col_br_green("Born"), "- 1886"),
    "Jane Doe", paste(cli::col_br_red("Died"), "- 1788")
  )
  
  input_list <- list("1600 - Foo", "1700 - Far", "John Doe (b. 1886)Jane Doe(d. 1788)")
  input_w_spaces <- list("1600 - Foo", "1700 - Far", "John Doe (b. 1886)  Jane Doe(d. 1788)")
  
  # need to write conditions
  input_end_str <- list("1600 - Foo", "1700 - Far", "John Doe (b. 1886)Jane Doe(d. 1788)", "not correct") #FIXME
  input_no_bd <- list("1600 - Foo", "1700 - Far", "", "", "wrrong str") #FIXME
  input_w_empty <- list("1600 - Foo", "1700 - Far", "", "", "John Doe (b. 1886)Jane Doe(d. 1788)")
  
  res_bd_tbl_norm <- create_births_deaths_table(input_list)
  res_bd_tbl_space <- create_births_deaths_table(input_w_spaces)
  
  res_bd_tbl_end_str <- create_births_deaths_table(input_end_str) #FIXME
  res_bd_tbl_no_bd <- create_births_deaths_table(input_no_bd) #FIXME
  
  expect_equal(exp_bd_tbl, res_bd_tbl_norm)
  expect_equal(exp_bd_tbl,res_bd_tbl_space)
  
})


test_that("Test that print_people_table works.", {
  testthat::local_edition(3)
  testthat::expect_snapshot(print_people_table(events_list))
})
