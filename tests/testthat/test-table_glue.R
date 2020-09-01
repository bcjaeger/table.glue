
rspec <- round_spec()


test_that("unnamed inputs are caught", {
  expect_error(table_glue("A", rspec, pi), 'non-character objects')
})
