
pvals_ama <- c(0.157, 0.037, 0.047, 0.003, 0.0003, 0.00003, 0.000003)
pvals_missing <- c(NA_real_, pvals_ama, NA_real_)

test_that("missings are handled correctly", {

  expect_equal(
    table_pvalue(pvals_missing),
    c(
      "--",
      ".16",
      ".04",
      ".047",
      ".003",
      "<.001",
      "<.001",
      "<.001",
      "--"
    )
  )



})

test_that("example values are correct", {

  expect_equal(

    table_pvalue(pvals_ama,
                 round_half_to = 'even',
                 decimals_outer = 3L,
                 decimals_inner = 2L,
                 alpha = 0.05,
                 bound_inner_low = 0.01,
                 bound_inner_high = 0.99,
                 bound_outer_low = 0.001,
                 bound_outer_high = 0.999,
                 miss_replace = '--',
                 drop_leading_zero = TRUE),

    c(".16", ".04", ".047", ".003", "<.001", "<.001", "<.001")

  )

})

test_that("boundary p-values are not made insignificant", {

  pvals_close <- c(0.04998, 0.05, 0.050002) + 0.1

  expect_equal(
    table_pvalue(pvals_close, alpha = 0.15),
    c(".14998", ".15", ".15")
  )

})

test_that("boundaries can be moved", {

  pvals_boundary <- c(0.095, 0.96)

  expect_equal(
    table_pvalue(pvals_boundary,
                 bound_inner_low = 0.10,
                 bound_inner_high = 0.97,
                 decimals_outer = 4,
                 decimals_inner = 2),
    c('.0950', '.96'),

  )

})
