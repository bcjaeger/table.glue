


test_that(
  desc = "miscellaneous special cases work",
  code = {

    expect_invisible(
      check_arg_length(arg_value = c(1,2,3),
                       arg_name = 'a',
                       expected_length = NULL)
    )

    expect_invisible(
      check_arg_bounds(arg_value = 1,
                       arg_name = 'a',
                       bound_lwr = 0,
                       bound_upr = 2)
    )

    expect_error(
      check_arg_bounds(arg_value = 2,
                       arg_name = 'a',
                       bound_lwr = 0,
                       bound_upr = 1),
      "a = 2 should be <= 1"
    )

    expect_error(
      check_arg_bounds(arg_value = -1,
                       arg_name = 'a',
                       bound_lwr = 0,
                       bound_upr = 1),
      "a = -1 should be >= 0"
    )

    expect_invisible(
      check_arg_is_valid(arg_value = ('x'),
                         arg_name = 'a',
                         valid_options = c("x"))
    )

    expect_error(
      check_arg_is_valid(arg_value = ('x'),
                         arg_name = 'a',
                         valid_options = c("y", "z")),
      "a should be <y or z>"
    )


  }
)
