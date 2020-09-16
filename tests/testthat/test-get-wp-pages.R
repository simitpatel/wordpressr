context("check-output")  # Our file is called "test-check_output.R"
library(testthat)        # load testthat package
library(wordpressr)       # load our package

# Test whether the output is a data frame
test_that("returns a data frame when page count argument not provided", {
  output_table <- get_wp_pages(root_url = 'altmediauncensored.com',100)
  expect_is(output_table, "data.frame")
})

# In reality, our function is more complex and aggregates your input if you have duplicates in your id-time units -- this is why the following two tests were essential for us
## Test whether the output contains the right number of rows
test_that("returns a data frame when page count argument is provided", {
  output_table <- get_wp_pages(root_url = 'altmediauncensored.com')
  expect_is(output_table, "data.frame")
})

#testthat::test_file('/Users/simit.patel/Documents/wordpressr/tests/testthat/test-get-wp-pages.R')
