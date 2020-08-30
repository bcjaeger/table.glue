



check_arg_type <- function(arg_value, arg_name, expected_type){

  if(expected_type == 'numeric') expected_type <- c('double', 'integer')

  arg_type <- typeof(arg_value)

  type_match <- arg_type %in% expected_type

  if (!type_match) {

    expected_types <- glue::glue_collapse(x = expected_type,
                                          sep = ', ',
                                          last = ' or ')

    error_msg <- glue::glue("{arg_name} should have type <{expected_types}>",
                            "\nbut instead has type <{arg_type}>")

    stop(as.character(error_msg), call. = FALSE)

  }

}

check_arg_is <- function(arg_value, arg_name, expected_class){

  arg_is <- inherits(arg_value, expected_class)

  if (!arg_is) {

    expected_classes <- glue::glue_collapse(x = expected_class,
                                            sep = ', ',
                                            last = ' or ')

    arg_classes <- glue::glue_collapse(x = class(arg_value),
                                       sep = ', ',
                                       last = ' or ')

    error_msg <- glue::glue(
      "{arg_name} should inherit from class <{expected_classes}>",
      "\nbut instead inherits from <{arg_classes}>"
    )

    stop(as.character(error_msg), call. = FALSE)

  }

}

check_arg_length <- function(arg_value, arg_name, expected_length){

  if(is.null(expected_length)) return(invisible())

  arg_length <- length(arg_value)

  length_match <- arg_length %in% expected_length

  if (!length_match) {

    expected_lengths <- glue::glue_collapse(x = expected_length,
                                            sep = ', ',
                                            last = ' or ')

    error_msg <- glue::glue("{arg_name} should have length <{expected_lengths}>",
                            "\nbut instead has length <{arg_length}>")

    stop(as.character(error_msg), call. = FALSE)

  }

}

check_arg_bounds <- function(arg_value, arg_name, bound_lwr, bound_upr){

  arg_value <- arg_value[!is.na(arg_value)]
  if(!is.null(bound_lwr)) check_bound_lwr(arg_value, arg_name, bound_lwr)
  if(!is.null(bound_upr)) check_bound_upr(arg_value, arg_name, bound_upr)

}

check_bound_lwr <- function(arg_value, arg_name, bound_lwr) {

  if(any(arg_value < bound_lwr)){
    error_msg <- glue::glue("{arg_name} = {arg_value} should be >= {bound_lwr}")
    stop(as.character(error_msg), call. = FALSE)
  }

}

check_bound_upr <- function(arg_value, arg_name, bound_upr) {

  if(any(arg_value > bound_upr)){
    error_msg <- glue::glue("{arg_name} = {arg_value} should be <= {bound_upr}")
    stop(as.character(error_msg), call. = FALSE)
  }

}

check_arg_is_valid <- function(arg_value, arg_name, valid_options) {

  valid_arg <- arg_value %in% valid_options

  if (!valid_arg) {

    expected_values <- glue::glue_collapse(x = valid_options,
                                          sep = ', ',
                                          last = ' or ')

    arg_values <- glue::glue_collapse(x = arg_value,
                                     sep = ', ',
                                     last = ' or ')

    error_msg <- glue::glue(
      "{arg_name} should be <{expected_values}>",
      "\nbut is instead <{arg_values}>"
    )

    stop(as.character(error_msg), call. = FALSE)

  }

}

check_input <- function(arg_name, arg_value, expected = list()){

  if(!is.null(expected$type))
    check_arg_type(arg_name = arg_name,
                   arg_value = arg_value,
                   expected_type = expected$type)

  if(!is.null(expected$length))
    check_arg_length(arg_name = arg_name,
                     arg_value = arg_value,
                     expected_length = expected$length)

  if(!is.null(expected$lwr) || !is.null(expected$upr))
    check_arg_bounds(arg_name = arg_name,
                     arg_value = arg_value,
                     bound_lwr = expected$lwr,
                     bound_upr = expected$upr)

  if(!is.null(expected$class))
    check_arg_is(arg_name = arg_name,
                 arg_value = arg_value,
                 expected_class = expected$class)

  if(!is.null(expected$options))
    check_arg_is_valid(arg_name = arg_name,
                       arg_value = arg_value,
                       valid_options = expected$options)

}

check_call <- function(call, expected){

  arg_names <- setdiff( names(call), '' )

  #browser()
  n_frames <- length(sys.frames())

  for (arg_name in arg_names ){

    object_found <- FALSE

    n <- 1

    while(n <= n_frames & !object_found){

      arg_value <- try(
        eval(call[[arg_name]], envir = parent.frame(n = n)),
        silent = TRUE
      )

      if(!inherits(arg_value, 'try-error')) object_found <- TRUE

      n <- n + 1

    }

    if(inherits(arg_value, 'try-error'))
      stop("object '", deparse(call[[arg_name]]),"' not found",
           call. = FALSE)

    if(is.null(arg_value)) return(invisible())

    expected_type <- expected[[arg_name]]$type
    expected_length <- expected[[arg_name]]$length
    bound_lwr = expected[[arg_name]]$lwr
    bound_upr = expected[[arg_name]]$upr
    expected_options = expected[[arg_name]]$options

    if(!is.null(expected_type))
      check_arg_type(arg_name = arg_name,
                     arg_value = arg_value,
                     expected_type = expected_type)

    if(!is.null(expected_length))
      check_arg_length(arg_name = arg_name,
                       arg_value = arg_value,
                       expected_length = expected_length)

    if(!is.null(bound_lwr) | !is.null(bound_upr))
      check_arg_bounds(arg_name = arg_name,
                       arg_value = arg_value,
                       bound_lwr = bound_lwr,
                       bound_upr = bound_upr)

    if(!is.null(expected_options))
      check_arg_is_valid(arg_name = arg_name,
                         arg_value = arg_value,
                         valid_options = expected_options)

  }

}
