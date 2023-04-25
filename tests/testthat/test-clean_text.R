

test_that("Test that text_to_event_list works as expected", {
  article_str <- "This is a list of events that have happened.\n1984 \032 Some guy wrote a book.\n2020 \032 there was a pandemic.John Doe (b. 884).Jane Doe."
  
  exp_list <- c(
    "This is a list of events that have happened.",
    "1984 \032 Some guy wrote a book.",
    "2020 \032 there was a pandemic.",
    "John Doe (b. 884).Jane Doe."
  )
  
  res_list <- text_to_event_list(article_str)
  
  expect_identical(res_list, exp_list)
})
