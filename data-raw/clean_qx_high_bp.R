##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
clean_qx_high_bp <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
) {

  fnames <- make_exam_files(exams, data_label = 'BPQ')

  var_guide <- tibble(
    term = c(
      'bp_high_aware',
      'meds_bp'
    ),
    nhanes = c(
      'BPQ020',
      'BPQ050A'
    ),
    descr = c(
      'Ever told you had high blood pressure',
      'Currently using meds to lower blood pressure'
    )
  )

  data_in <- map_dfr(
    .x = fnames,
    .f = read_xpt,
    .id = 'exam'
  ) %>%
    transmute(
      exam,
      seqn = SEQN,
      bp_high_aware = recode(BPQ020,
                             "1" = "yes",
                             "2" = "no",
                             "7" = 'Refused',
                             "9" = 'Don\'t know'),
      meds_bp_rec = recode(BPQ040A,
                           "1" = "yes",
                           "2" = "no",
                           "7" = 'Refused',
                           "9" = 'Don\'t know'),
      meds_bp_now = recode(BPQ050A,
                           "1" = "yes",
                           "2" = "no",
                           "7" = 'Refused',
                           "9" = 'Don\'t know')
    )

  # There are a couple of assumptions we can make based on survey responses:
  # - If the survey participant has never been told that they have
  #   high blood pressure or doesn't know if they were told this,
  #   then the values of `meds_bp_rec` and `meds_bp_use` should be `'no'`.
  #
  # - If the survey participant has never been recommended to take
  #   blood pressure medication or doesn't know if they were
  #   recommended to do so, then the value of `meds_bp_use` should be `'no'`.
  #
  #   These rules are codified here:

  unsure <- c(NA_character_, 'Refused', 'Don\'t know')

  data_out <- data_in %>%
    mutate(
      meds_bp = case_when(
        bp_high_aware == 'yes' & meds_bp_now == 'yes' ~ 'yes',
        bp_high_aware %in% unsure ~ NA_character_,
        meds_bp_rec == 'yes' & meds_bp_now %in% unsure ~ NA_character_,
        TRUE ~ 'no'
      )
    )

  # We test our classification of blood pressure medication use by showing
  # the counts of each combination of categories. Each row of the data below
  # should be reasonable, though some make stronger assumptions than others.

  # count(data_out, bp_high_aware, meds_bp_rec, meds_bp_now, meds_bp)

  data_out %<>%
    select(exam, seqn, bp_high_aware, meds_bp)


  # The naming convention for meds_glucose was different in exams
  # 2005-2006 and 2007-2008 compared to all other exams.
  # The names are synchronized here.

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }

}
