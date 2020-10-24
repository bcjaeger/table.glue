

test_that("examples work", {

  tbl_value <- "12.1 (95% CI: 9.1, 15.1)"

  expect_equal(ci_drop(tbl_value), "12.1")
  expect_equal(ci_extract(tbl_value, drop_bracket = TRUE), "95% CI: 9.1, 15.1")
  expect_equal(ci_lower_bound(tbl_value), "9.1")
  expect_equal(ci_upper_bound(tbl_value), "15.1")

})

test_that("weird brackets are okay", {

  tbl_value <- "12.1 {95% CI: 9.1, 15.1]"

  expect_equal(ci_drop(tbl_value,
                       bracket_left = '{',
                       bracket_right = ']'),
               expected = "12.1")
  expect_equal(ci_extract(tbl_value,
                          drop_bracket = TRUE,
                          bracket_left = '{',
                          bracket_right = ']'),
               expected = "95% CI: 9.1, 15.1")
  expect_equal(ci_lower_bound(tbl_value,
                              bracket_left = '{',
                              bracket_right = ']'),
               expected = "9.1")
  expect_equal(ci_upper_bound(tbl_value,
                              bracket_left = '{',
                              bracket_right = ']'),
               expected = "15.1")

})

test_that("units after values are okay", {

  tbl_value <- "12.1 years {95% CI: 9.1, 15.1]"

  expect_equal(ci_drop(tbl_value,
                       bracket_left = '{',
                       bracket_right = ']'),
               expected = "12.1")
  expect_equal(ci_extract(tbl_value,
                          drop_bracket = TRUE,
                          bracket_left = '{',
                          bracket_right = ']'),
               expected = "95% CI: 9.1, 15.1")
  expect_equal(ci_lower_bound(tbl_value,
                              bracket_left = '{',
                              bracket_right = ']'),
               expected = "9.1")
  expect_equal(ci_upper_bound(tbl_value,
                              bracket_left = '{',
                              bracket_right = ']'),
               expected = "15.1")

})

test_that("% symbols are okay", {

  tbl_value <- "12.1% {95% CI: 9.1%, 15.1%]"

  expect_equal(ci_drop(tbl_value,
                       bracket_left = '{',
                       bracket_right = ']'),
               expected = "12.1")
  expect_equal(ci_extract(tbl_value,
                          drop_bracket = TRUE,
                          bracket_left = '{',
                          bracket_right = ']'),
               expected = "95% CI: 9.1%, 15.1%")
  expect_equal(ci_lower_bound(tbl_value,
                              bracket_left = '{',
                              bracket_right = ']'),
               expected = "9.1")
  expect_equal(ci_upper_bound(tbl_value,
                              bracket_left = '{',
                              bracket_right = ']'),
               expected = "15.1")

})
