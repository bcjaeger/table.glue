



table_glue <- function(
  ...,
  .sep = '',
  .envir = parent.frame(),
  rspec,
  fspec
){

  .dots <- substitute(alist(...))

  string <- Reduce(base::c, eval(.dots))
  string <- paste(string, collapse = .sep)

  # objects inside of {}
  pattern <- "(?<=\\{).+?(?=\\})"
  objects <- stringi::stri_extract_all_regex(string, pattern)
  objects <- objects[[1]]
  objects <- unique(objects)

  if(is.data.frame(.envir)) .envir <- list2env(.envir)

  .envir$..f <- function(x){

    if (is.numeric(x)){
      trimws(
        tbl_val(x,
                breaks = breaks,
                decimals = decimals,
                round_half_to_even = round_half_to_even,
                miss_replace = miss_replace,
                big_mark = big_mark,
                big_interval = big_interval,
                small_mark = small_mark,
                small_interval = small_interval,
                decimal_mark = decimal_mark,
                zero_print = zero_print,
                trim = trim
        )
      )
    } else {
      x
    }

  }

  # _p_attern
  p <- paste0("{", objects, "}")

  # _r_eplacement
  r <- paste0("{..f(", objects, ")}")

  for( i in seq_along(objects) ) string <- gsub(string,
                                                pattern = p[i], replacement = r[i], fixed = TRUE)

  # make output a character
  # (avoid potential issues with attributes of glue objects)
  as.character(glue::glue(string, .envir = .envir))


}
