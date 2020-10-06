

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

test_that('formatting works', {

  rspec <- round_spec() %>%
    format_missing('okay') %>%
    format_big('big') %>%
    format_small('oh noes', interval = 2) %>%
    round_using_decimal(digits = 5) %>%
    format_decimal('decimal!')

  expect_equal(
    table_value(1000.234567, rspec),
    "1big000decimal!23oh noes45oh noes7"
  )

})
