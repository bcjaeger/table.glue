##' blood pressure (BP) data are imported from NHANES website and cleaned.
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
##' @param include_variable_labels
##'
clean_exam_bp <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
) {

  # retrieve data ----

  data_all <- map_dfr(
    .x = make_exam_files(exams, data_label = 'BPX'),
    .f = read_xpt,
    .id = 'exam'
  )

  # Variable description ----

  var_guide <- tibble(
    term = c(
      'bp_sys_mmhg',
      'bp_dia_mmhg',
      'n_msr_sbp',
      'n_msr_dbp'
    ),
    nhanes = c(
      'BPXSY1 - BPXSY4',
      'BPXDI1 - BPXDI4',
      'BPXSY1 - BPXSY4',
      'BPXDI1 - BPXDI4'
    ),
    descr = c(
      'Systolic blood pressure, mm Hg',
      'Diastolic blood pressure, mm Hg',
      'Number of systolic blood pressure readings',
      'Number of diastolic blood pressure readings'
    )
  )

  # Variable derivation ----

  # Note 1:
  # Some DBP values are 0. The NHANES documentation acknowledges this.
  # I have set those DBP to missing, but kept the corresponding SBP.

  # Note 2:
  # Blood pressure is measured up to 4 times for each participant.
  # To create analytic blood pressure variables, I average the four
  # measurements for each participant.

  # Note 3:
  # Some participants have zero BP values. taking their mean results in
  # NaN (not a number). I set these NaN values to missing.


  data_cleaned_bp <- select(
    data_all,
    exam,
    seqn = SEQN,
    sbp_1 = BPXSY1,
    sbp_2 = BPXSY2,
    sbp_3 = BPXSY3,
    sbp_4 = BPXSY4,
    dbp_1 = BPXDI1,
    dbp_2 = BPXDI2,
    dbp_3 = BPXDI3,
    dbp_4 = BPXDI4
  ) %>%
    # reshape so I can do operations on all BP values by participant
    pivot_longer(cols = -c(exam, seqn)) %>%
    separate(name, into = c('type', 'measure')) %>%
    pivot_wider(names_from = type, values_from = value) %>%
    mutate(dbp = replace(dbp, dbp == 0, NA_real_)) %>% # see note 1
    group_by(exam, seqn) %>%
    summarise(
      bp_sys_mmhg = mean(sbp, na.rm = TRUE),
      bp_dia_mmhg = mean(dbp, na.rm = TRUE),
      n_msr_sbp = sum(!is.na(sbp)),
      n_msr_dbp = sum(!is.na(dbp)),
      .groups = 'drop'
    ) %>% # see note 2
    mutate(
      across(
        .cols = starts_with('bp'),
        .fns = ~ replace(.x, is.nan(.x), NA_real_)
      )
    ) # see note 3

  if(include_variable_labels){
    add_labels(data_cleaned_bp, var_guide)
  } else {
    data_cleaned_bp
  }

}
