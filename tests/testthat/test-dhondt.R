test_that("D'Hondt Method works", {
  # based on https://en.wikipedia.org/wiki/D%27Hondt_method#Jefferson_and_D'Hondt
  votes <- c(100000, 80000, 30000, 20000)
  apprt <- app_dhondt(8, votes)

  expect_equal(apprt, c(4, 3, 1, 0))
})

