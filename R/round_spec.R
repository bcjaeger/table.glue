


#' Make a rounding specification
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
    round_half   = getOption("table.glue.round_half", default = 'even'),
    round_using  = getOption("table.glue.round_using", default = 'magnitude'),
    digits       = getOption("table.glue.digits", default = c(2,1,0)),
    breaks       = getOption("table.glue.breaks", default = c(1,10,Inf)),
    miss_replace = getOption("table.glue.miss_replace", default = '--'),
    big_mark     = getOption("table.glue.big_mark", default = ','),
    big_interval = getOption("table.glue.big_interval", default = 3L),
    small_mark   = getOption("table.glue.small_mark", default = ''),
    small_interval = getOption("table.glue.small_interval", default = 5L),
    decimal_mark = getOption("table.glue.decimal_mark", default = '.'),
    zero_print   = getOption("table.glue.zero_print", default = NULL)
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
#' @family format helpers
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
#' @family formatting helpers
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
#' `format_decimal()` lets you update the settings of a
#'   `rounding_specification` object (see [round_spec]) so that
#'   the decimal is represented by a user-specified mark.
#'
#' @inheritParams format_missing
#'
#' @param mark a character value used to represent the decimal point.
#'
#' @inherit round_spec return
#'
#' @export
#'
#' @family formatting helpers
#'
#' @examples
#'
#' small_x <- 0.1234567
#'
#' rspec <- round_spec()
#' rspec <- round_using_decimal(rspec, digits = 7)
#' rspec <- format_decimal(rspec, mark = '*')
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



#' Set rules for rounding ties
#'
#' Rounding a number x to the nearest integer requires some tie-breaking
#'  rule for those cases when x is exactly half-way between two integers,
#'  that is, when the fraction part of x is exactly 0.5. The
#'  `round_half_up()` function implements a tie-breaking rule that
#'  consistently rounds half units upward. Although this creates a slight
#'  bias toward larger rounded outputs, it is widely used in many disciplines.
#'  The `round_half_even()` function breaks ties by rounding to the nearest
#'  even unit.
#'
#' @inheritParams format_missing
#'
#' @inherit round_spec return
#'
#' @export
#'
#' @family rounding helpers
#'
#' @examples
#'
#' # note base R behavior rounds to even:
#' round(0.5) # --> 0
#' round(1.5) # --> 2
#' round(2.5) # --> 2
#'
#' # make rspec that rounds up
#' rspec <- round_half_up(round_spec())
#' rspec <- round_using_decimal(rspec, digits = 0)
#'
#' # check
#' table_value(0.5, rspec) # --> 1
#' table_value(1.5, rspec) # --> 2
#' table_value(2.5, rspec) # --> 3
#'
#' # make rspec that rounds even
#' rspec <- round_half_even(round_spec())
#' rspec <- round_using_decimal(rspec, digits = 0)
#'
#' # check
#' table_value(0.5, rspec) # --> 0
#' table_value(1.5, rspec) # --> 2
#' table_value(2.5, rspec) # --> 2

round_half_up <- function(rspec){

  .round_half(rspec, 'up')

}

#' @rdname round_half_up
#' @export
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


#' Set rules for rounding numbers
#'
#' These functions update a `rounding_specification` object (see
#'  [round_spec]) so that a particular approach to rounding is applied:
#'  - round to a dynamic decimal place based on magnitude
#'    of the rounded number (`round_using_magnitude()`)
#'  - round to a specific number of significant
#'    digits (`round_using_signif()`)
#'  - round to a specific decimal place (`round_using_decimal()`)
#'
#' @inheritParams format_missing
#'
#' @param digits for `round_using_decimal()` and `round_using_signif`,
#'  a numeric value specifying the number of decimal places and
#'  significant digits to round to, respectively.
#'  For `round_using_magnitude()`, `digits` should be a numeric vector
#'  of equal length to `breaks` that indicates how many decimals to
#'  round to in the numeric range designated by `breaks`.
#'  (see notes for example).
#'
#' @param breaks (only relevant if rounding based on magnitude)
#'   a positive, monotonically increasing numeric vector
#'   designating rounding boundaries.
#'
#' @details
#'
#' `digits` and `breaks` must be used in coordination with each other
#'   when rounding based on magnitude. For example, using
#'   `breaks = c(1, 10, Inf)` and `decimals = c(2, 1, 0)`,
#'   - numbers whose absolute value is < 1 are rounded to 2 decimal places,
#'   - numbers whose absolute value is >= 1 and < 10 are rounding to 1
#'     decimal place, and
#'   - numbers whose absolute value is >= 10 are rounding to 0 decimal places.
#' The use of magnitude to guide rounding rules is extremely flexible and
#'   can be used for many different applications (e.g., see [table_pvalue]).
#'   Rounding by magnitude is similar in some ways to rounding to a set
#'   number of significant digits but not entirely the same (see examples).
#'
#'
#' @inherit round_spec return
#'
#' @export
#'
#' @family rounding helpers
#'
#' @examples
#'
#' x <- c(pi, exp(1))
#' x <- c(x, x*10, x*100, x*1000)
#'
#' # make one specification using each rounding approach
#' specs <- list(
#'   magnitude = round_using_magnitude(round_spec()),
#'   decimal = round_using_decimal(round_spec()),
#'   signif = round_using_signif(round_spec())
#' )
#'
#' # apply all three rounding specifications to x
#' # notice how the rounding specifications are in agreement
#' # for smaller values of x but their answers are different
#' # for larger values of x.
#'
#' sapply(specs, function(rspec) table_value(x, rspec))
#'
#' # output:
#' #  magnitude   decimal    signif
#' # [1,] "3.1"     "3.1"     "3.1"
#' # [2,] "2.7"     "2.7"     "2.7"
#' # [3,] "31"      "31.4"    "31.0"
#' # [4,] "27"      "27.2"    "27.0"
#' # [5,] "314"     "314.2"   "310.0"
#' # [6,] "272"     "271.8"   "270.0"
#' # [7,] "3,142"   "3,141.6" "3,100.0"
#' # [8,] "2,718"   "2,718.3" "2,700.0"
round_using_magnitude <- function(rspec,
                                  digits = c(2, 1, 0),
                                  breaks = c(1, 10, Inf)){
  .round_using(rspec, 'magnitude', digits = digits, breaks = breaks)
}

#' @rdname round_using_magnitude
#' @export

round_using_signif <- function(rspec, digits = 2){
  .round_using(rspec, 'signif', digits = digits, breaks = NULL)
}

#' @rdname round_using_magnitude
#' @export

round_using_decimal <- function(rspec, digits = 1){
  .round_using(rspec, 'decimal', digits = digits, breaks = NULL)
}



#' Set the default rounding specification
#'
#' If you ever get tired of having to write
#'   `table_glue(..., rspec = your_rspec)` and would rather just write
#'   `table_glue(...)`, you can use the `default_rounder_set()` function
#'   on `your_rspec` to temporarily update `options` in your current R
#'   session and update the default rounding specification to match
#'   whatever your current rounding specification is.
#'   Naturally, mistakes will be made when `default_rounder_set()` is used.
#'   To reset the rounding specifications to factory defaults,
#'   use `default_rounder_reset()`.
#'
#' @inheritParams format_missing
#'
#' @return `default_rounder_set()` changes global options and does
#'   not return a value.
#'
#' @details Setting a default rounding specification will impact
#'   all functions in the `table helpers` family. Use with caution! Also,
#'   please make all use of `default_rounder_set()` __explicit__
#'   in your code so that it can be reproduced. It is highly recommended
#'   that you __do not__ use `default_rounder_set()` in your .Rprofile.
#'
#' @export
#'
#' @examples
#'
#' your_rspec <- round_using_decimal(round_spec(), digits = 10)
#' your_rspec <- format_small(your_rspec, mark = '--')
#'
#' # now your_rspec is the default.
#' default_rounder_set(your_rspec)
#'
#' # so it is applied whenever you use table_value(),
#' # unless you specify otherwise.
#' table_value(rnorm(5))
#'
#'
default_rounder_set <- function(rspec){
  names(rspec) <- paste('table.glue', names(rspec), sep = '.')
  options(rspec)
  invisible()
}

#' @rdname default_rounder_set
#' @export
default_rounder_reset <- function() {

  op <- options()

  op.table.glue <- list(
    table.glue.round_half = 'even',
    table.glue.round_using = 'magnitude',
    table.glue.digits = c(2, 1, 0),
    table.glue.breaks = c(1, 10, Inf),
    table.glue.miss_replace = '--',
    table.glue.big_mark = ',',
    table.glue.big_interval = 3L,
    table.glue.small_mark = '',
    table.glue.small_interval = 5L,
    table.glue.decimal_mark = getOption('OutDec'),
    table.glue.zero_print = NULL
  )

  toset <- !(names(op.table.glue) %in% names(op))

  if(any(toset)) options(op.table.glue[toset])

  invisible()

}
