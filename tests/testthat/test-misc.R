

test_that("error is thrown for small number", {
  expect_error(find_smallest_10(x = 1e-20), 'too small')
})
