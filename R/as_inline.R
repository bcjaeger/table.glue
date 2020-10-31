

#' Convert table data to inline list
#'
#' @param data a data frame.
#' @param tbl_variables column names that will be used to form groups in the table
#' @param tbl_values column name that contains table values.
#'
#' @return a list of `tbl_values` values for each permutation of `tbl_variables`
#'
#' @note variables in `tbl_variables` that have missing values will be
#'   have their missing values converted into an explicit category named
#'   variable_missing, where 'variable' is the name of the variable.
#'
#' @export
#'
#' @examples
#'
#' example_data <- data.frame(
#'   sex = c("female", "male"),
#'   height = c("158 (154 - 161)", "178 (175 - 188)")
#' )
#'
#' as_inline(example_data, tbl_variables = 'sex', tbl_values = 'height')
#'
#' car_data <- mtcars
#' car_data$car_name <- rownames(mtcars)
#' as_inline(car_data, tbl_variables = 'car_name', tbl_values = 'mpg')
#'
as_inline <- function(data,
                      tbl_variables,
                      tbl_values){


  check_call(
    match.call(),
    expected = list(
      'data' = list(type = 'data.frame'),
      'tbl_variables' = list(type = 'character',
                             expected = names(data)),
      'tbl_values' = list(type = 'character',
                         expected = names(data))
    )
  )

  output <- fill_na_levels(data, cols = tbl_variables)

  for (variable in tbl_variables) {
    output <- split_by(output, variable = variable)
  }

  lapply(output, get_element, tbl_values)

}

fill_na_levels <- function(data, cols){

  for(variable in cols){

    variable_is_factor <- is.factor(data[[variable]])

    if(variable_is_factor){
      variable_levels <- levels(data[[variable]])
      data[[variable]] <- as.character(data[[variable]])
    }

    na_index <- which(is.na(data[[variable]]))
    missing_level <- NULL

    if(!is_empty(na_index)){
      missing_level <- paste(variable, 'missing', sep = '_')
      data[[variable]][na_index] <- missing_level
    }

    if(variable_is_factor){
      data[[variable]] <- factor(data[[variable]],
                                 levels = c(variable_levels, missing_level))
    }

  }

  data

}

split_by <- function(data, variable){

  if(inherits(data, 'data.frame')){
    return(split(data, f = data[[variable]]))
  }

  lapply(data, split_by, variable = variable)

}

get_element <- function(data, variable){

  if(inherits(data, 'data.frame')){

    if(length(variable) == 1){
      return(getElement(object = data, name = variable))
    }

    return(as.list(data[, variable]))

  }

  lapply(data, get_element, variable = variable)

}

