


#' Rounding specification
#'
#' `round_spec()` creates a rounding specification object with default
#'   settings. The settings of a rounding specification object can be
#'   updated using functions in the `round_` (see [round_half_up],
#'   [round_half_even], [round_using_signif], [round_using_decimal],
#'   and [round_using_magnitude]) and `format_` (see [format_missing],
#'   [format_big], [format_small], and [format_decimal]) families.
#'
#'   Rounding specifications are meant to be passed into the [table_glue]
#'   and [table_value] functions. The specification can also be passed into
#'   `table_` functions implicitly by saving a rounding specification into
#'   the global options.
#'
#'   The `round_spec()` function intentionally uses no input arguments.
#'   This is to encourage users to develop rounding specifications
#'   using the `round_` and `format_` families in conjunction with
#'   the pipe (`%>%`) operator.
#'
#' @return an object of class `rounding_specification`.
#' @export
#'
#' @examples
#'
#' rspec <- round_spec()
#'
#' table_value(x = pi, rspec)
#'
round_spec <- function() {

  rspec <- list(
    round_half = 'even',
    round_using = 'magnitude',
    digits = c(2, 1, 0),
    breaks = c(1, 10, Inf),
    miss_replace = '--',
    big_mark = ',',
    big_interval = 3L,
    small_mark = '',
    small_interval = 5L,
    decimal_mark = getOption('OutDec'),
    zero_print = NULL
  )

  class(rspec) <- 'rounding_specification'

  rspec

}

#' Format missing values
#'
#' `format_missing()` updates a `rounding_specification` object so that
#'   missing values are printed as the user specifies.
#'
#' @param rspec a `rounding_specification` object (see [round_spec]).
#' @param replace_na_with a character value that replaces missing values.
#'
#' @inherit round_spec return
#'
#' @export
#'
#' @examples
#'
#' rspec <- round_spec()
#' rspec <- format_missing(rspec, 'oh no!')
#' table_value(x = c(pi, NA), rspec)
#'

format_missing <- function(rspec, replace_na_with){

  check_input(arg_name  = 'rspec',
              arg_value = rspec,
              expected  = list(class = 'rounding_specification'))

  check_input(arg_name = 'replace_na_with',
              arg_value = replace_na_with,
              expected = list(type = 'character', length = 1))

  rspec$miss_replace <- replace_na_with

  rspec

}

#' Format values left of decimal
#'
#' Values to the left of the decimal are generally called 'big' since they
#'   are larger than values to the right of the decimal. `format_big()`
#'   lets you update the settings of a `rounding_specification` object
#'   (see [round_spec]) so that values left of the decimal will be printed
#'   with a specific format (see examples).
#'
#' @inheritParams format_missing
#'
#' @param mark a character value used to separate number groups to the
#'   left of the decimal point. See [prettyNum] for more details on this.
#'   Set this input to '' to negate it's effect.
#'
#' @param interval a numeric value indicating the size of number groups
#'   for numbers left of the decimal.
#'
#' @inherit round_spec return
#'
#' @export
#'
#' @examples
#'
#' big_x <- 1234567
#'
#' rspec <- format_big(round_spec(), mark = '|', interval = 3)
#'
#' table_value(big_x, rspec) # returns "1|234|567"
#'
format_big <- function(rspec, mark = ',', interval = 3L){

  check_input(arg_name  = 'rspec',
              arg_value = rspec,
              expected  = list(class = 'rounding_specification'))

  check_input(arg_name = 'mark',
              arg_value = mark,
              expected = list(type = 'character', length = 1))

  check_input(arg_name = 'interval',
              arg_value = interval,
              expected = list(type = 'numeric', length = 1, lwr = 1))

  rspec$big_mark <- mark
  rspec$big_interval <- interval
  rspec

}


#' Format values right of decimal
#'
#' Values to the right of the decimal are generally called 'small' since they
#'   are smaller than values to the left of the decimal. `format_small()`
#'   lets you update the settings of a `rounding_specification` object
#'   (see [round_spec]) so that values right of the decimal will be printed
#'   with a specific format (see examples).
#'
#' @inheritParams format_missing
#'
#' @param mark a character value used to separate number groups to the
#'   right of the decimal point. See [prettyNum] for more details on this.
#'   Set this input to '' to negate it's effect.
#'
#' @param interval a numeric value indicating the size of number groups
#'   for numbers left of the decimal.
#'
#' @inherit round_spec return
#'
#' @export
#'
#' @examples
#'
#' small_x <- 0.1234567
#'
#' rspec <- round_spec()
#' rspec <- round_using_decimal(rspec, digits = 7)
#' rspec <- format_small(rspec, mark = '*', interval = 1)
#'
#' table_value(small_x, rspec)
#'
format_small <- function(rspec, mark = '', interval = 5L){

  check_input(arg_name  = 'rspec',
              arg_value = rspec,
              expected  = list(class = 'rounding_specification'))

  check_input(arg_name = 'mark',
              arg_value = mark,
              expected = list(type = 'character', length = 1))

  check_input(arg_name = 'interval',
              arg_value = interval,
              expected = list(type = 'numeric', length = 1, lwr = 1))

  rspec$small_mark <- mark
  rspec$small_interval <- interval
  rspec
}


#' Format decimal symbol
#'
#' The decimal symbol can be update.
#'
#' @inheritParams format_missing
#'
#' @param mark a character value used to separate number groups to the
#'   left of the decimal point. See [prettyNum] for more details on this.
#'   Set this input to '' to negate it's effect.
#'
#' @param interval a numeric value indicating the size of number groups
#'   for numbers left of the decimal.
#'
#' @inherit round_spec return
#'
#' @export
#'
#' @examples
#'
#' small_x <- 0.1234567
#'
#' rspec <- round_spec()
#' rspec <- round_using_decimal(rspec, digits = 7)
#' rspec <- format_small(rspec, mark = '*', interval = 1)
#'
#' table_value(small_x, rspec)
#'
format_decimal <- function(rspec, mark = '.'){

  check_input(arg_name  = 'rspec',
              arg_value = rspec,
              expected  = list(class = 'rounding_specification'))

  check_input(arg_name = 'mark',
              arg_value = mark,
              expected = list(type = 'character', length = 1))

  rspec$decimal_mark <- mark

  rspec


}

.round_half <- function(rspec, set_to){

  check_input(arg_name  = 'rspec',
              arg_value = rspec,
              expected  = list(class = 'rounding_specification'))

  rspec$round_half <- set_to

  rspec

}

round_half_up <- function(rspec){

  .round_half(rspec, 'up')

}

round_half_even <- function(rspec){
  .round_half(rspec, 'even')
}

.round_using <- function(rspec, set_to, digits, breaks){

  check_input(arg_name  = 'rspec',
              arg_value = rspec,
              expected  = list(class = 'rounding_specification'))

  if(!is.null(breaks))
    check_input(
      arg_name  = 'breaks',
      arg_value = breaks,
      expected  = list(
        type = 'numeric',
        length = c(1, 3, length(digits)),
        lwr = 0,
        upr = NULL
      )
    )

  check_input(
    arg_name  = 'digits',
    arg_value = digits,
    expected  = list(
      type = 'numeric',
      length = c(1, 3, length(breaks)),
      lwr = -20,
      upr = 20
    )
  )

  rspec$round_using <- set_to

  rspec$digits <- digits
  rspec$breaks <- breaks

  rspec

}

round_using_signif <- function(rspec, digits){
  .round_using(rspec, 'signif', digits = digits, breaks = NULL)
}

round_using_decimal <- function(rspec, digits){
  .round_using(rspec, 'decimal', digits = digits, breaks = NULL)
}

round_using_magnitude <- function(rspec, digits, breaks){
  .round_using(rspec, 'magnitude', digits = digits, breaks = breaks)
}


