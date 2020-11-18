

test_that("examples work", {

  tbl_value <- "12.1 (95% CI: 9.1, 15.1)"

  expect_equal(bracket_drop(tbl_value), "12.1")
  expect_equal(bracket_extract(tbl_value, drop_bracket = TRUE), "95% CI: 9.1, 15.1")
  expect_equal(bracket_lower_bound(tbl_value), "9.1")
  expect_equal(bracket_upper_bound(tbl_value), "15.1")

  expect_equal(bracket_drop(tbl_value),
               bracket_point_estimate(tbl_value))

})

test_that("weird brackets are okay", {

  tbl_value <- "12.1 {95% CI: 9.1, 15.1]"

  expect_equal(bracket_drop(tbl_value,
                       bracket_left = '{',
                       bracket_right = ']'),
               expected = "12.1")
  expect_equal(bracket_extract(tbl_value,
                          drop_bracket = TRUE,
                          bracket_left = '{',
                          bracket_right = ']'),
               expected = "95% CI: 9.1, 15.1")
  expect_equal(bracket_lower_bound(tbl_value,
                              bracket_left = '{',
                              bracket_right = ']'),
               expected = "9.1")
  expect_equal(bracket_upper_bound(tbl_value,
                              bracket_left = '{',
                              bracket_right = ']'),
               expected = "15.1")

})

test_that("units after values are okay", {

  tbl_value <- "12.1 years {95% CI: 9.1, 15.1]"

  expect_equal(bracket_drop(tbl_value,
                       bracket_left = '{',
                       bracket_right = ']'),
               expected = "12.1")
  expect_equal(bracket_extract(tbl_value,
                          drop_bracket = TRUE,
                          bracket_left = '{',
                          bracket_right = ']'),
               expected = "95% CI: 9.1, 15.1")
  expect_equal(bracket_lower_bound(tbl_value,
                              bracket_left = '{',
                              bracket_right = ']'),
               expected = "9.1")
  expect_equal(bracket_upper_bound(tbl_value,
                              bracket_left = '{',
                              bracket_right = ']'),
               expected = "15.1")

})

test_that("% symbols are okay", {

  tbl_value <- "12.1% {95% CI: 9.1%, 15.1%]"

  expect_equal(bracket_drop(tbl_value,
                       bracket_left = '{',
                       bracket_right = ']'),
               expected = "12.1")
  expect_equal(bracket_extract(tbl_value,
                          drop_bracket = TRUE,
                          bracket_left = '{',
                          bracket_right = ']'),
               expected = "95% CI: 9.1%, 15.1%")
  expect_equal(bracket_lower_bound(tbl_value,
                              bracket_left = '{',
                              bracket_right = ']'),
               expected = "9.1")
  expect_equal(bracket_upper_bound(tbl_value,
                              bracket_left = '{',
                              bracket_right = ']'),
               expected = "15.1")

})

test_that("bracket inserts are okay", {

  tbl_value <- "12.1% {9.1%, 15.1%]"

  expect_equal(
    bracket_insert_left(tbl_value, string = '95% CI: ', bracket_left = '{'),
    "12.1% {95% CI: 9.1%, 15.1%]"
  )

  expect_equal(
    bracket_insert_right(tbl_value, string = ' units', bracket_right = ']'),
    "12.1% {9.1%, 15.1% units]"
  )

  expect_equal(
    tbl_value %>%
      bracket_insert_left(string = '95% CI: ', bracket_left = '{') %>%
      bracket_insert_right(string = ' units', bracket_right = ']'),
    "12.1% {95% CI: 9.1%, 15.1% units]"
  )



})
