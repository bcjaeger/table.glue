

test_that("construction works", {

  empty_spec <- round_spec()

  expect_is(empty_spec, 'rounding_specification')

})

test_that('inherit errors are thrown correctly', {

  expect_error(round_half_up('char'),
               'inherits from <character>')

})


test_that('length errors are thrown correctly', {

  expect_error(
    round_using_decimal(round_spec(), digits = c(2, 1, 4)),
    'has length <3>'
  )

})

