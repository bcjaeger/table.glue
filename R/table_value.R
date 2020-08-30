



#' Table values
#'
#' `table_value()` casts numeric vectors into character vectors.
#'   The main purpose of `table_value()` is to round and format
#'   numeric data for presentation.
#'
#' @param x a vector of numeric values.
#'
#' @param rspec a `rounding_specification` object. If no `rspec`
#'   is given, a default setting will round values to decimal places
#'   based on the magnitude of the values.
#'
#' @return a vector of character values (rounded numbers).
#'
#' @export
#'
#' @family table helpers
#'
#' @examples
#'
#' table_value(0.123)
#' table_value(1.23)
#' table_value(12.3)
#'
#' with(mtcars, table_value(disp))
#'
table_value <- function(x, rspec = NULL){

  # integer types need not be rounded to a decimal place,
  # but can still be formatted nicely.
  if(is.integer(x)){
    rspec$digits <- 0L
    return(fr_dispatch(x, rspec, r_fun = function(x, ...) x))
  }

  # find the most immediate rounding specification.
  .rspec <- if(!is.null(rspec)) rspec else round_spec()

  check_input(arg_name  = 'rspec',
              arg_value = .rspec,
              expected  = list(class = 'rounding_specification'))

  # use the format(round()) combination dictated by .rspec
  switch(glue::glue("{round_using}_{round_half}", .envir = .rspec),
         "decimal_up"     = fr_decimal_up(x, .rspec),
         "decimal_even"   = fr_decimal_even(x, .rspec),
         "signif_up"      = fr_signif_up(x, .rspec),
         "signif_even"    = fr_signif_even(x, .rspec),
         "magnitude_up"   = fr_magnitude(x, .rspec),
         "magnitude_even" = fr_magnitude(x, .rspec))

}

fr_dispatch <- function(x, .rspec, r_fun){

  nsmall <- if(.rspec$round_using == 'signif') 0 else .rspec$digits

  format(
    x = r_fun(x, digits = .rspec$digits, breaks = .rspec$breaks),
    nsmall = safe_nsmall(nsmall),
    big.mark = .rspec$big_mark,
    big.interval = .rspec$big_interval,
    small.mark = .rspec$small_mark,
    small.interval = .rspec$small_interval,
    decimal.mark = .rspec$decimal_mark,
    zero.print = .rspec$zero_print,
    trim = TRUE
  )
}

r_decimal_up <- function(x, digits = 0, breaks = NULL){

  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg

}

fr_decimal_up <- function(x , .rspec){
  fr_dispatch(x, .rspec, r_decimal_up)
}

r_decimal_even <- function(x, digits = 0, breaks = NULL){
  round(x, digits = digits)
}

fr_decimal_even <- function(x , .rspec){
  fr_dispatch(x, .rspec, r_decimal_even)
}

r_signif_up <- function(x, digits = 6, breaks = NULL){
  signif(x + 1*10^(-digits-1), digits = digits)
}

fr_signif_up <- function(x , .rspec){
  fr_dispatch(x, .rspec, r_signif_up)
}

r_signif_even <- function(x, digits = 6, breaks = NULL){
  signif(x, digits = digits)
}

fr_signif_even <- function(x , .rspec){
  fr_dispatch(x, .rspec, r_signif_even)
}

fr_magnitude <- function(x, .rspec){

  r_fun <- switch(.rspec$round_half,
                  'even' = r_decimal_even,
                  'up' = r_decimal_up)

  out <- rep(.rspec$miss_replace, length(x))

  if (all(is.na(x))) return(out)

  # take absolute value to round based on magnitude
  x_abs <- abs(x)

  breaks <- .rspec$breaks
  # the breaks are based on rounded x instead of x itself
  breaks_smallest_10 <- sapply(breaks, find_smallest_10)

  # makes my code easier to read...
  decimals <- .rspec$digits
  # rounding to 0 decimals, 9.5 should be considered as if it were 10
  # rounding to 1 decimals, 9.95 should be considered as if it were 10
  # rounding to 2 decimals, 9.995 should be considered as if it were 10
  # in general the formula for bump down value is (1/2) / 10^decimals

  bump_down <- 0.5 / (10^decimals)

  x_brks <- c(0, breaks - bump_down)

  if(max(x_brks) < Inf){
    x_brks <- c(x_brks, Inf)
    decimals <- duplicate_last(decimals)
  }

  # x_cuts create boundary categories for rounding
  x_cuts <- cut(
    x_abs,
    breaks = x_brks,
    include.lowest = TRUE,
    right = FALSE
  )

  out_breaks <- lapply(
    levels(x_cuts),
    function(.x) which(x_cuts == .x)
  )

  for (i in seq_along(out_breaks)) {

    ob <- out_breaks[[i]]

    if(!is_empty(ob)){

      ob_rounded <- r_fun(x[ob], digits = decimals[i])

      out[ob] <- format(
        ob_rounded,
        nsmall = safe_nsmall(decimals[i]),
        big.mark = .rspec$big_mark,
        big.interval = .rspec$big_interval,
        small.mark = .rspec$small_mark,
        small.interval = .rspec$small_interval,
        decimal.mark = .rspec$decimal_mark,
        zero.print = .rspec$zero_print,
        trim = TRUE
      )

    }

  }

  out

}
