##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param .names
add_missing_cols <- function(data, .names, value = NA_real_) {

  cols_missing <- setdiff(.names, names(data))

  if (is_not_empty(cols_missing)) {
    for(.col in cols_missing) data[[.col]] <- value
  }

  data

}
