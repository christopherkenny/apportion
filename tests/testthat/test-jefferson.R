test_that("Jefferson Method works", {
  # based on https://www.census.gov/history/www/reference/apportionment/methods_of_apportionment.html
  pops <- c(2560, 3315, 995, 5012)
  apprt <- app_jefferson(20, pops)

  expect_equal(apprt, c(4, 6, 1, 9))
})
