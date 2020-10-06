
rspec <- round_spec()

test_that("unnamed inputs are caught", {

  expect_error(table_glue("A", rspec, pi), 'non-character objects')

  x <- 1
  df <- data.frame(x = 2)

  expect_equal(table_glue("x is {x}"), 'x is 1.0')
  expect_equal(table_glue("x is {x}", .envir = df), 'x is 2.0')

})
