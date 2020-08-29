

format_spec <- function() {

  fspec <- list(
    miss_replace = NULL,
    big_mark = NULL,
    big_interval = NULL,
    small_mark = NULL,
    small_interval = NULL,
    decimal_mark = NULL,
    zero_print = NULL
  )

  class(fspec) <- 'formatting_specification'

  fspec

}
