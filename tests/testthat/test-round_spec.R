


test_that("construction works", {

  empty_spec <- round_spec()

  expect_is(empty_spec, 'rounding_specification')

  rspec_even_decimal <- round_spec() %>%
    round_half_even() %>%
    round_using_decimal(digits = 0)

  rspec_up_decimal <- round_spec() %>%
    round_half_up() %>%
    round_using_decimal(digits = 0)

  expect_equal(
    round_apply(rspec_even_decimal, x = c(1.5, 2.5)),
    expected = c(2,2)
  )

  expect_equal(
    round_apply(rspec_up_decimal, x = c(1.5, 2.5)),
    expected = c(2,3)
  )

  rspec_even_signif <- round_spec() %>%
    round_half_even() %>%
    round_using_signif(digits = 1)

  rspec_up_signif <- round_spec() %>%
    round_half_up() %>%
    round_using_signif(digits = 1)

  expect_equal(
    round_apply(rspec_even_signif, x = c(1.5, 2.5)),
    expected = c(2,2)
  )

  expect_equal(
    round_apply(rspec_up_signif, x = c(1.5, 2.5)),
    expected = c(2,3)
  )


})
