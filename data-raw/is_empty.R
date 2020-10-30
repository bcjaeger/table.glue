##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param x
is_empty <- function(x) {

  length(x) == 0

}

is_not_empty <- function(x) !is_empty(x)
