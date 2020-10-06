

# testing vectors
x <- c(1.2345,
       12.345,
       123.45,
       1234.5)

y <- c(-1.25,
       -2.5,
       -3.5,
       -4.05)

test_that("rounding specifications match intent", {

  rspec_sig_1_even <- round_spec() %>%
    round_using_signif(digits = 1) %>%
    round_half_even()

  rspec_mag_even <- round_spec() %>%
    round_using_magnitude(digits = c(2, 2, 1, 0),
                          breaks = c(1, 10, 100, Inf)) %>%
    round_half_even()

  rspec_dig_1_even <- round_spec() %>%
    round_using_decimal(digits = 1) %>%
    round_half_even()

  expect_equal(table_value(x, rspec_sig_1_even),
               c("1", "10", "100", "1,000"))

  expect_equal(table_value(y, rspec_sig_1_even),
               c('-1', '-2', '-4', '-4'))

  expect_equal(table_value(x, rspec_mag_even),
               c("1.23", "12.3", "123", "1,234"))

  expect_equal(table_value(x, rspec_dig_1_even),
               c("1.2", "12.3", "123.4", "1,234.5"))

  #table_value(y, rspec_dig_1_even)

  rspec_sig_1_up <- round_spec() %>%
    round_using_signif(digits = 1) %>%
    round_half_up()

  rspec_mag_up <- round_spec() %>%
    round_using_magnitude(digits = c(2, 2, 1, 0),
                          breaks = c(1, 10, 100, Inf)) %>%
    round_half_up()

  rspec_dig_1_up <- round_spec() %>%
    round_using_decimal(digits = 1) %>%
    round_half_up()

  expect_equal(table_value(x, rspec_sig_1_up),
               c("1", "10", "100", "1,000"))

  expect_equal(table_value(x, rspec_mag_up),
               c("1.23", "12.3", "123", "1,235"))

  expect_equal(table_value(x, rspec_dig_1_up),
               c("1.2", "12.3", "123.5", "1,234.5"))


})

test_that("rounding on boundaries matches specification", {

  rspec_mag_up <- round_spec() %>%
    round_using_magnitude(digits = c(2, 1, 0, 0),
                          breaks = c(1, 10, 100, Inf)) %>%
    round_half_up()

  x <- c(0.995, 9.95)

  expect_equal(table_value(x, rspec_mag_up), c('1.0', '10'))

})

test_that("base rounding functions work", {

  expect_equal(r_signif_even(x = 0.45, digits = 1), 0.4)
  expect_equal(r_decimal_even(0.45, digits = 1), 0.4)
  expect_equal(r_signif_up(x = 0.45, digits = 1), 0.5)
  expect_equal(r_decimal_up(0.45, digits = 1), 0.5)

})
