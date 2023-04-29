utf8_hyphen <- "\032"

events_list <- list(
  "Not an event",
  "Important national holiday",
  glue::glue("2020 {utf8_hyphen} some fake fact"),
  glue::glue("John Doe (b. 684) ")
)

test_that("Test that create_events_table", {
  
  exp_ans <- tibble::tibble(Year="2020", Details="some fake fact")
  ans <- create_events_table(events_list)
  
  testthat::expect_identical(exp_ans, ans)
})

# Snapshot testing --------------------------------------

cli::test_that_cli("Test that ", {
  testthat::local_edition(3)
  testthat::expect_snapshot(print_events_tbl(events_list))
})