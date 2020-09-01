
#' Round estimates and their corresponding errors
#'
#' Though they are not easy to find in print, there are some general
#'   conventions for rounding numbers. When rounding a summary statistic
#'   such as the mean or median, the number of rounded digits shown
#'   should be governed by the precision of the statistic. For instance,
#'   authors are usually asked to present means plus or minus standard
#'   deviations in published research, or regression coefficients plus
#'   or minus the standard error. The convention applied here is to
#'    1. find place of the first significant digit of the error
#'    2. round the estimate to that place
#'    3. round the error to 1 additional place
#'    4. present the combination in a form such as estimate (error) or estimate +/- error
#'
#'
#' @param estimate a numeric vector of estimate values.
#' @param error a numeric vector of error values. All errors should be >0.
#' @param lower the lower-bound of an interval for the estimate.
#' @param upper the upper-bound of an interval for the estimate.
#' @param form a character value that indicates how the error and estimate
#'   should be formatted together. Users can specify anything they like
#'   as long as they use the terms `estimate` and `error` to refer to the
#'   estimate and error values, respectively, and encapsulate those terms
#'   inside of curly brackets, i.e., { } . For instance, if estimate = 1.23
#'   and error = 0.45, then `form` = "{estimate} ({error})" will return
#'   "1.2 (0.45)", a common format used in  presentation of the point and
#'   error combination. The default `form` gives output in the form of
#'   1.2 +/- 0.45.
#' @param majority_rule a logical value. If `TRUE`, then the most common
#'  digit used for rounding will be used to round every number given.
#'  Within a single table, consistency in saving digits may be desirable,
#'  so all numbers may be rounded to the place  indicated by the majority
#'  of the numbers. Notably, if a user wants to exercise more control
#'  over the number of decimals shown, they should use [table_glue()]
#'  with a customized rounding specification (see [round_spec]).
#'
#' @return a character vector
#'
#' @export
#'
#' @family table helpers
#'
#' @references Blackstone, Eugene H. "Rounding numbers" (2016):
#' _The Journal of Thoracic and Cardiovascular Surgery_.
#' DOI: https://doi.org/10.1016/j.jtcvs.2016.09.003
#'
#' @examples
#'
#' # ---- examples are taken from Blackstone, 2016 ----
#'
#' # Example 1: ----
#' # Mean age is 72.17986, and the standard deviation (SD) is 9.364132.
#' ## Steps:
#' ## - Nine is the first significant figure of the SD.
#' ## - Nine is in the ones place. Thus...
#' ##   + round the mean to the ones place (i.e., round(x, digits = 0))
#' ##   + round the SD to the tenths place (i.e., round(x, digits = 1))
#' table_ester(estimate = 72.17986, error = 9.364132)
#' # > [1] 72 +/- 9.4
#'
#' # an estimated lower and upper bound for 95% confidence limits
#' lower <- 72.17986 - 1.96 * 9.364132
#' upper <- 72.17986 + 1.96 * 9.364132
#' table_estin(estimate = 72.17986, lower = lower, upper = upper,
#'             form = "{estimate} (95% CI: {lower}, {upper})")
#' # > [1] "72 (95% CI: 54, 91)"
#'
#' # Example 2: ----
#' # Mean cost is $72,347.23, and the standard deviation (SD) is $23,994.06.
#' ## Steps:
#' ## - Two is the first significant figure of the SD.
#' ## - Nine is in the ten thousands place. Thus...
#' ##   + round mean to the 10-thousands place (i.e., round(x, digits = -4))
#' ##   + round SD to the thousands place (i.e., round(x, digits = -3))
#' table_ester(estimate = 72347.23, error = 23994.06)
#' # > [1] "70,000 +/- 24,000"
#'
#' # an estimated lower and upper bound for 95% confidence limits
#' lower <- 72347.23 - 1.96 * 23994.06
#' upper <- 72347.23 + 1.96 * 23994.06
#' table_estin(estimate = 72347.23, lower = lower, upper = upper,
#'             form = "{estimate} (95% CI: {lower} - {upper})")
#' # > [1] "70,000 (95% CI: 30,000 - 120,000)"
#'
#'

table_ester <- function(estimate,
                        error,
                        form = "{estimate} \u00B1 {error}",
                        majority_rule = FALSE) {

  if(any(is.na(estimate)) || any(is.na(error))) stop(
    "Missing values are present in `estimate` and/or `error`.",
    "\nMissing values are not compatible with estimate +/- error rounding",
    call. = FALSE
  )

  check_call(
    match.call(),
    expected = list(
      'estimate' = list(type = 'numeric'),
      'error' = list(type = 'numeric', lwr = 0),
      'form' = list(type = 'character', length = 1),
      'majority_rule' = list(type = 'logical', length = 1)
    )
  )

  if(length(estimate) != length(error)) stop(
    "The lengths of `estimate` and `error` do not match.",
    call. = FALSE
  )

  dig <- find_rounding_digit(error)

  if(majority_rule){
    tb <- table(dig)
    dig[] <- tb[which.max(tb)]
  }

  .estimate <- .error <- rep(
    x = getOption('table.glue.miss_replace', default = '--'),
    times = length(estimate)
  )

  big.mark       = getOption("table.glue.big_mark",       default = ',')
  big.interval   = getOption("table.glue.big_interval",   default = 3L)
  small.mark     = getOption("table.glue.small_mark",     default = '')
  small.interval = getOption("table.glue.small_interval", default = 5L)
  decimal.mark   = getOption("table.glue.decimal_mark",   default = '.')
  zero.print     = getOption("table.glue.zero_print",     default = NULL)

  for(i in seq_along(estimate)){

    .estimate[i] <- format(
      round(estimate[i], dig[i]),
      nsmall = max(0, dig[i]),
      big.mark = big.mark,
      big.interval = big.interval,
      small.mark = small.mark,
      small.interval = small.interval,
      decimal.mark = decimal.mark,
      zero.print = zero.print,
      trim = TRUE
    )

    .error[i] <- format(
      round(error[i], dig[i] + 1),
      nsmall = max(0, dig[i] + 1),
      big.mark = big.mark,
      big.interval = big.interval,
      small.mark = small.mark,
      small.interval = small.interval,
      decimal.mark = decimal.mark,
      zero.print = zero.print,
      trim = TRUE
    )

  }

  .envir <- list(estimate = .estimate, error = .error)

  as.character(
    do.call(glue::glue, args = list(form, .envir = .envir))
  )

}

#' @rdname table_ester
#' @export
table_estin <- function(estimate,
                        lower,
                        upper,
                        form = "{estimate} ({lower}, {upper})",
                        majority_rule = FALSE) {

  if (any(is.na(estimate)) || any(is.na(lower)) || any(is.na(upper)) )
    stop(
      "Missing values are present in `estimate`, `lower`, or `upper`.",
      "\nMissing values are not compatible with estimate interval rounding",
      call. = FALSE
    )

  if(length(estimate) != length(lower)) stop(
    "The lengths of `estimate` and `lower` do not match.",
    call. = FALSE
  )

  if(length(estimate) != length(upper)) stop(
    "The lengths of `estimate` and `upper` do not match.",
    call. = FALSE
  )

  check_call(
    match.call(),
    expected = list(
      'estimate' = list(type = 'numeric'),
      'lower' = list(type = 'numeric'),
      'upper' = list(type = 'numeric'),
      'form' = list(type = 'character', length = 1),
      'majority_rule' = list(type = 'logical', length = 1)
    )
  )

  # A rough approximation to the standard error
  error <- abs(upper - lower) / (2 * 1.959963984540054)

  dig <- find_rounding_digit(error)

  if(majority_rule){
    tb <- table(dig)
    dig[] <- tb[which.max(tb)]
  }

  .estimate <- .lower <- .upper <- rep(
    x = getOption('table.glue.miss_replace', default = '--'),
    times = length(estimate)
  )

  big.mark       = getOption("table.glue.big_mark",       default = ',')
  big.interval   = getOption("table.glue.big_interval",   default = 3L)
  small.mark     = getOption("table.glue.small_mark",     default = '')
  small.interval = getOption("table.glue.small_interval", default = 5L)
  decimal.mark   = getOption("table.glue.decimal_mark",   default = '.')
  zero.print     = getOption("table.glue.zero_print",     default = NULL)

  for(i in seq_along(estimate)){

    .estimate[i] <- format(
      round(estimate[i], dig[i]),
      nsmall = max(0, dig[i]),
      big.mark = big.mark,
      big.interval = big.interval,
      small.mark = small.mark,
      small.interval = small.interval,
      decimal.mark = decimal.mark,
      zero.print = zero.print,
      trim = TRUE
    )

    .lower[i] <- format(
      round(lower[i], dig[i]),
      nsmall = max(0, dig[i]),
      big.mark = big.mark,
      big.interval = big.interval,
      small.mark = small.mark,
      small.interval = small.interval,
      decimal.mark = decimal.mark,
      zero.print = zero.print,
      trim = TRUE
    )

    .upper[i] <- format(
      round(upper[i], dig[i]),
      nsmall = max(0, dig[i]),
      big.mark = big.mark,
      big.interval = big.interval,
      small.mark = small.mark,
      small.interval = small.interval,
      decimal.mark = decimal.mark,
      zero.print = zero.print,
      trim = TRUE
    )

  }

  .envir <- list(estimate = .estimate, lower = .lower, upper = .upper)

  as.character(
    do.call(glue::glue, args = list(form, .envir = .envir))
  )

}

find_rounding_digit <- function(x){

  .x <- abs(x)

  out <- rep(NA_integer_, length(.x))

  for(i in seq_along(.x)){


    power <- 128

    .xx <- .x[i]

    if(.xx < Inf) repeat {
      .xx <- .x[i] %% 10 ^ (power)
      if(.xx < .x[i]) break
      power <- power - 1
      if(power == -128) stop("error is too small", call. = FALSE)
    } else {
      power <- 0
    }

    out[i] <- power * (-1)

  }

  out


}
