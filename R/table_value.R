

table_value <- function(x, rspec = NULL, fspec = NULL){

  .rspec <- rspec %||% getOption(x = 'round_spec') %||% default_rspec()
  .fspec <- fspec %||% getOption(x = 'format_spec') %||% default_fspec()

  .table_value(
    x = x,
    round_half_to_even = rspec$round_half == 'even',
    breaks = rspec$args$round_using$breaks,
    decimals = rspec$args$round_using$decimals,
  )

}

.table_value <- function(
  x,
  round_half_to_even = FALSE,
  breaks = c(1, 10, Inf),
  decimals = c(2, 1, 0),
  miss_replace = '--',
  big_mark = ',',
  big_interval = 3L,
  small_mark = '',
  small_interval = 5L,
  decimal_mark = getOption('OutDec'),
  zero_print = NULL,
  trim = TRUE
) {

  check_call(
    match.call(),
    expected = list(
      'x' = list(
        type = 'numeric',
        length = NULL,
        lwr = NULL,
        upr = NULL
      ),
      'round_half_to_even' = list(
        type = 'logical',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'breaks' = list(
        type = 'numeric',
        length = c(1, 3, length(decimals)),
        lwr = 0,
        upr = NULL
      ),
      'decimals' = list(
        type = 'numeric',
        length = c(1, 3, length(breaks)),
        lwr = -20,
        upr = 20
      ),
      'miss_replace' = list(
        type = 'character',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'big_mark' = list(
        type = 'character',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'big_interval' = list(
        type = 'numeric',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'small_mark' = list(
        type = 'character',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'small_interval' = list(
        type = 'numeric',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'decimal_mark' = list(
        type = 'character',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'zero_print' = list(
        type = c('logical', 'character'),
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'trim' = list(
        type = 'logical',
        length = 1,
        lwr = NULL,
        upr = NULL
      )
    )
  )

  # recycle decimals if length is 1
  if(length(decimals) == 1) decimals <- rep(decimals, length(breaks))

  if (length(breaks) != length(decimals))
    stop('breaks and decimals should have equal length', call. = FALSE)

  if (is_empty(x)) stop("cannot format empty vectors", call. = FALSE)

  if (!is.numeric(x))
    stop("x should be numeric", call. = FALSE)

  if (is.integer(x)) return(format(x, big.mark = big_mark))

  if(any(diff(breaks) < 0))
    stop("breaks should be strictly monotonically increasing", call. = FALSE)

  ..round <- if(round_half_to_even) base::round else .round

  out <- rep(miss_replace, length(x))

  if (all(is.na(x))) return(out)

  # take absolute value to round based only on magnitudes
  x_abs <- abs(x)

  # the breaks are based on rounded x instead of x itself
  breaks_smallest_10 <- sapply(breaks, find_smallest_10)

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

      ob_rounded <- ..round(x[ob], digits = decimals[i])

      out[ob] <- format(
        ob_rounded,
        nsmall = safe_nsmall(decimals[i]),
        big.mark = big_mark,
        big.interval = big_interval,
        small.mark = small_mark,
        small.interval = small_interval,
        decimal.mark = decimal_mark,
        zero.print = zero_print,
        #justify = justify,
        trim = trim,
      )

    }

  }

  out

}
