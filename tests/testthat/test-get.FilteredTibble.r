# test-get.FilteredTibble.r

library(testthat)
library(dplyr)
library(lubridate)
library(tibble)
library(mtrxrlib)
library(devtools)

fileName <- 'test.csv'
# Mock helper functions

convertNoneToNA <- function(x) {
  ifelse(x == "None", NA, x)
}

toDate <- function(date_str, dateFormat) {
  as.Date(date_str, format = dateFormat)
}

# Test cases
test_that("get.FilteredTibble handles valid input correctly", {
  result <- get.FilteredTibble(
    fileName, 
    date_from = as.Date("2022-01-01"))
  
  # Check structure
  expect_s3_class(result, "tbl_df")
  
  # Check filtered rows
  expect_equal(nrow(result), 9) # Only 2 rows have Created >= 2022-01-01
  
  # Check column names
  expect_true(all(c("Created", "Updated", "Closed", "crdt", "updt", "cldt") %in% colnames(result)))
  
  # Check that "Closed" is converted correctly
  expect_true(is.na(result$Closed[2])) # "None" should be converted to NA
})


# Test case for missing file
test_that("get.FilteredTibble handles missing file", {
  expect_error(get.FilteredTibble("missing.csv"), 
               "The file 'missing.csv' does not exist. Please check the file path.")
})

# Test case for no rows matching filter
test_that("get.FilteredTibble handles no matching rows", {
  result <- get.FilteredTibble(fileName, date_from = as.Date("2026-01-01"))
  
  # Check structure
  expect_s3_class(result, "tbl_df")
  
  # Expect empty tibble
  expect_equal(nrow(result), 0)
})

# Test case for invalid date format
test_that("get.FilteredTibble handles invalid date format", {
  expect_error(
    get.FilteredTibble(
      fileName, date_from = as.Date("-01")),
              "character string is not in a standard unambiguous format")
})
