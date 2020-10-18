

test_that("missing values are rejected", {

  expect_error(table_ester(estimate = NA, error = 1),
               regexp = 'Missing values')

  expect_error(table_estin(estimate = NA, lower = 1, upper = 2),
               regexp = 'Missing values')

})

test_that("type errors are rejected", {

  expect_error(table_ester(estimate = "1", error = 1),
               regexp = 'estimate should have')

  expect_error(table_ester(estimate = 1, error = "1"),
               regexp = 'error should have')

})

test_that('length errors are rejected', {

  expect_error(
    table_ester(estimate = c(1,2), error = c(1)),
    'do not match'
  )

  expect_error(
    table_estin(estimate = c(1,2), lower = c(1), upper = c(2,3)),
    'lengths of `estimate` and `lower` do not match'
  )

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

test_that('majority rule works', {

  expect_equal(
    table_ester(estimate = c(72.17986, 72.17986, 72347.23),
                error    = c(9.364132, 9.364132, 23994.06),
                form = '{estimate} ({error})',
                majority_rule = TRUE),
    c("72.18 (9.364)", "72.18 (9.364)", "72,347.23 (23,994.060)")
  )

  expect_equal(
    table_estin(estimate = c(72.17986, 72.17986, 72347.23),
                lower = c(71.1234, 71.1234, 71123.4567),
                upper = c(73.4321, 73.4321, 73321.1234),
                majority_rule = TRUE),
    c("72.18 (71.12, 73.43)",
      "72.18 (71.12, 73.43)",
      "72,347.23 (71,123.46, 73,321.12)")
  )

})
