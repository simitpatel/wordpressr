context("check-output")  # Our file is called "test-check_output.R"
library(testthat)        # load testthat package
library(wordpressr)       # load our package

# Test whether the output is a data frame
test_that("get_wp_authors() returns a data frame", {
  output_table <- get_wp_authors(root_url = 'altmediauncensored.com')
  expect_is(output_table, "data.frame")
})

# In reality, our function is more complex and aggregates your input if you have duplicates in your id-time units -- this is why the following two tests were essential for us
## Test whether the output contains the right number of rows
test_that("get_wp_authors() returns a dataframe with 4 columns", {
  output_table <- get_wp_authors(root_url = 'altmediauncensored.com')
  expect_equal(ncol(output_table), 4)
})
