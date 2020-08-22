
# package is built on rounding and formatting specifications

## round_spec
# creates an S3 object (list)
# attributes are initially unspecified
#
## functions to set rounding behavior
# round_half_up
# round_half_even
#
## functions to set decimal behavior
# round_by_signif
# round_by_digits
# round_by_values

round_spec <- function() {

  rspec <- list(
    round_half = NULL,
    round_using = NULL,
    args = NULL
  )

  class(rspec) <- 'rounding_specification'

  rspec

}

.round_half <- function(rspec, set_to){

  rspec$round_half <- set_to
  rspec

}

.round_using <- function(rspec, set_to, args){

  rspec$round_using <- set_to
  rspec$args$round_using <- args

  rspec

}

round_half_up <- function(rspec){
  .round_half(rspec, 'up')
}

round_half_even <- function(rspec){
  .round_half(rspec, 'even')
}

round_using_signif <- function(rspec, digits){
  .round_using(rspec, 'signif', list(digits = digits, breaks = NULL))
}

round_using_decimal <- function(rspec, digits){
  .round_using(rspec, 'decimal', list(digits = digits, breaks = NULL))
}

round_using_magnitude <- function(rspec, digits, breaks){
  .round_using(rspec, 'magnitude', list(digits = digits, breaks = breaks))
}

round_apply <- function(rspec, x){

  rspec_footprint <-
    glue::glue("{round_using}_{round_half}",
               .envir = rspec)

  .r <- switch(rspec_footprint,
              "decimal_up" = r_decimal_up,
              "decimal_even" = r_decimal_even,
              "signif_up" = r_signif_up,
              "signif_even" = r_signif_even)

  .args <- c(rspec$args$round_using)

  .args$x <- x

  do.call(.r, args = .args)

}

r_decimal_up = function(x, digits = 0, breaks = NULL){

  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg

}
r_signif_up = function(x, digits = 6, breaks = NULL){
  signif(x + 1*10^(-digits-1), digits = digits)
}
r_signif_even <- function(x, digits = 6, breaks = NULL){
  signif(x, digits = digits)
}
r_decimal_even <- function(x, digits = 0, breaks = NULL){
  round(x, digits = digits)
}
