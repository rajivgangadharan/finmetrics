
test_that("Age is 0 for equal created date and closed date", {
  # Happy path
  expect_equal(getAgeInDays(as.Date("2021-01-01"),
                            as.Date("2021-01-01")),
               0)
})
#> Test passed 😀

test_that("Test if getAgeInDays() throws error on character inputs", {
  # Check for non date inputs
  expect_error(getAgeInDays("2021-01-01", as.Date("2021-01-01")))
  expect_error(getAgeInDays(as.Date("2021-01-01"), "2021-01-01"))
  expect_error(getAgeInDays("2021-01-01", "2021-01-01"))
})
#> Test passed 😀
