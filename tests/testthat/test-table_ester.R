

test_that("missing values are rejected", {

  expect_error(table_ester(estimate = NA, error = 1),
               regexp = 'Missing values')

})

test_that("type errors are rejected", {

  expect_error(table_ester(estimate = "1", error = 1),
               regexp = 'estimate should have')

  expect_error(table_ester(estimate = 1, error = "1"),
               regexp = 'error should have')

})

test_that("examples are correct", {

  example_one <- table_ester(estimate = 72.17986,
                             error = 9.364132,
                             form = "{estimate} ({error})")

  example_two <- table_ester(estimate = 72347.23,
                             error = 23994.06,
                             form = "{estimate} ({error})")

  expect_equal(example_one, "72 (9.4)")

  expect_equal(example_two, "70,000 (24,000)")

})

test_that("estimate intervals example is correct",{

  # should be rounded to 10's place b/c conf interval width is 5
  expect_equal(
    table_estin(estimate = 72.345, lower = 62.345, upper = 82.345),
    "72 (62, 82)"
  )

})
