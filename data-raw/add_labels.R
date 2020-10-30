##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_out
##' @param var_guide
add_labels <- function(data_out, var_guide, ref_nhanes = FALSE) {

  var_vec <- var_guide %>%
    mutate(
      across(where(is.factor), as.character),
      ref_nhanes = ref_nhanes
    ) %>%
    transmute(
      term,
      label = if_else(ref_nhanes,
                      true = as.character(glue('{descr} ({nhanes})')),
                      false = descr)
    ) %>%
    deframe()

  for(i in names(var_vec)){
    var_label(data_out[[i]]) <- var_vec[i]
  }

  data_out

}
