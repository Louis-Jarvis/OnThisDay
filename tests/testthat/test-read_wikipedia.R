html_snippet <- rvest::minimal_html(
  "
<div class='mw-parser-output'>
  <div id='mp-otd' class='mp-contains-float'>
    <p><b><a href='/wiki/April_29' title='April 29'>April 29</a></b></p>
    <!-- Pic: do not select this -->
    <div id='mp-otd-img'>
      <div class='thumbinner mp-thumb'>
        <a href='pic' class='image' title='pic'>
        <img src='foo' </a>
          <div class='thumbcaption'>an img</div>
      </div>
    </div>
    <!-- Events list we care about -->
    <ul>
      <li> 1970 – Something happened. </li>
      <li> 1984 – A fake event occured. </li>
    </ul>
    <!-- births deaths list-->
    <div class='hlist inline nowraplinks'>
        <ul>
            <li><b>Joe Mama</b> (<abbr title='died'>d.</abbr>&nbsp;1707)</li>
            <li><b>Jane Doe</b> (<abbr title='born'>b.</abbr>&nbsp;1894)</li>
        </ul>
    </div>
    <div>
        <!--Anything beyond this point is irrelevant-->
        More anniversaries:
        <div class='hlist inline nowraplinks'>
            <ul>
                <li>April 28</li>
                <li>April 29</li>
                <li>April 30</li>
            </ul>
        </div>
    </div>
    <div class='hlist otd-footer noprint'>
        <ul>
            <li>Archive</li>
            <li><b>By email</li>
            <li><List of days of the</li>
        </ul>
    </div>
</div>
  "
)

# expected_events_list <- list(
#   "April 29",
#   "an img",
#   "1970 \032 Something happened.",
#   "1984 \032 A fake event occured.",
#   "Joe Mama (d. 1707)",
#   "Jane Doe (b. 1894)"
# )

testthat::test_that("test that read_wiki_html works_properly", {
  exp_string <-
    "April 29\n\nan img\n1970 \032 Something happened.\n1984 \032 A fake event occured.\nJoe Mama (d. 1707)\nJane Doe (b. 1894)\nMore anniversaries:\nApril 28\nApril 29\nApril 30\nArchive\nBy email"
  ans_str <- read_wiki_html(html_snippet, archive = F)
  
  testthat::expect_equal(exp_string, ans_str)
})

#TODO move to its own file (along with the date functions)
testthat::test_that("Test that check_date_input works correctly", {
  check_sublists <- function(input_list, ans_list) {
    lapply(seq_along(length(input_list)), function(i) {

      output <- check_date_input(input_list[[i]])
      
      testthat::expect_equal(output$num_str, ans_list[[i]]$num_str)
      testthat::expect_equal(output$mnth_str, ans_list[[i]]$mnth_str)
    })
  }
  
  valid_dates <- list(
    date_1 = "March 1st",
    date_2 = "1 March",
    date_3 = "1st of march",
    date_4 = "1 mar",
    date_5 = "Sep 21st",
    date_6 = "SEP 1"
  )
  
  valid_ans <- list(
    date_1 = list(num_str = "1", mnth_str = "March"),
    date_2 = list(num_str = "1", mnth_str = "March"),
    date_3 = list(num_str = "1", mnth_str = "March"),
    date_4 = list(num_str = "1", mnth_str = "March"),
    date_5 = list(num_str = "21", mnth_str = "September"),
    date_6 = list(num_str = "1", mnth_str = "September")
  )
  
  ans_list <- lapply(valid_dates, function(x) {
    check_date_input(x)
  })
  
  check_sublists(valid_dates, valid_ans)
  
  inv_list <- list(
    inv_date_1 = "First march",
    inv_date_2 = "First MARCH",
    inv_date_3 = "2022-02-04",
    inv_date_4 = "Feb 31"
  )
  
  lapply(inv_list, function(date_str)
    testthat::expect_error(check_date_input(date_str)))
})

testthat::test_that("Test that get_url_from_date works correctly", {
  exp_main_url <- "https://en.wikipedia.org/wiki/Main_Page"
  exp_archive_url <-
    "https://en.wikipedia.org/wiki/Wikipedia:Selected_anniversaries/March_1"
  
  url_main <- get_url_from_date(NULL)
  url_archive <- get_url_from_date("march 1")
  
  testthat::expect_equal(exp_main_url, url_main)
  testthat::expect_equal(exp_archive_url, url_archive)
  testthat::expect_error(get_url_from_date("foo"))
})
