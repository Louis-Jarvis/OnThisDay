test_that("Test events_as_row works as expected", {
  
  # Normal Case
  exp_normal <- tibble::tibble(Year='1600', Details="foo")
  res_normal <- events_as_row("1600 \032 foo")
  
  expect_equal(exp_normal, res_normal)
})

# test_that("test bd_as_row works as expected", {
#   
#   exp_born <- tibble::tibble(
#     Person = "John Doe", 
#     Event = paste(cli::col_br_green("Born"), "- 1788")
#     )
#   
#   exp_dead <- tibble::tibble(
#     Person = "John Doe", 
#     Event = paste(cli::col_br_red("Died"), "- 1788")
#   )
#   
#   res_brn <- bd_as_row("John Doe (b. 1788)")
#   res_dead <- bd_as_row("John Doe (d. 1788)")
#   
#   expect_equal(exp_born, res_brn)
#   expect_equal(exp_dead, res_dead)
# })

test_that("Test that create_births_deaths_table works as expected",{
  
  exp_bd_tbl <- tibble::tribble(
    ~Person, ~Event,
    "John Doe", paste(cli::col_br_green("Born"), "- 1886"),
    "Jane Doe", paste(cli::col_br_red("Died"), "- 1788")
  )
  
  input_list <- list("1600 - Foo", "1700 - Far", "John Doe (b. 1886)Jane Doe(d. 1788)")
  input_w_spaces <- list("1600 - Foo", "1700 - Far", "John Doe (b. 1886)  Jane Doe(d. 1788)")
  
  # need to write condition
  input_end_str <- list("1600 - Foo", "1700 - Far", "John Doe (b. 1886)Jane Doe(d. 1788)", "not correct") #FIXME
  input_no_bd <- list("1600 - Foo", "1700 - Far", "", "", "wrrong str") #FIXME
  input_w_empty <- list("1600 - Foo", "1700 - Far", "", "", "John Doe (b. 1886)Jane Doe(d. 1788)")
  
  res_bd_tbl_norm <- create_births_deaths_table(input_list)
  res_bd_tbl_space <- create_births_deaths_table(input_w_spaces)
  
  res_bd_tbl_end_str <- create_births_deaths_table(input_end_str) #FIXME
  res_bd_tbl_no_bd <- create_births_deaths_table(input_no_bd)#FIXME
  
  expect_equal(exp_bd_tbl, res_bd_tbl_norm)
  expect_equal(exp_bd_tbl,res_bd_tbl_space)
  
})

