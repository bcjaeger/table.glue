

.onLoad <- function(libname, pkgname) {

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
