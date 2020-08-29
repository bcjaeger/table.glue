

test_that("construction works", {

  empty_spec <- round_spec()

  expect_is(empty_spec, 'rounding_specification')

})

