##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
##' @param include_variable_labels

clean_demo <- function(
  exams = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017),
  include_variable_labels = TRUE
) {


  # retrieve data
  fnames <- make_exam_files(exams, data_label = 'DEMO')

  var_guide <- tibble(
    term = c(
      'age',
      'sex',
      'race_ethnicity',
      'education',
      'income_hh',
      'pregnant'
    ),
    nhanes = c(
      'RIDAGEYR',
      'RIAGENDR',
      'RIDRETH1 and RIDRETH3',
      'DMDEDUC2 and DMDEDUC3',
      'INDHHINC and INDHHIN2',
      'RIDEXPRG'
    ),
    descr = c(
      "age in years",
      "sex",
      "race and ethnicity",
      "education",
      "household income",
      "pregnancy status"
    )
  )

  .names <- c('RIDAGEYR',
              'RIAGENDR',
              'RIDRETH1',
              'RIDRETH3',
              'DMDEDUC2',
              'DMDEDUC3',
              'INDHHINC',
              'INDHHIN2',
              'RIDEXPRG')

  data_in <- fnames %>%
    map_dfr(.f = read_xpt, .id = 'exam') %>%
    add_missing_cols(.names)

  # race-ethnicity (RIDRETH) -------------------------------------------------
  #
  # `RIDRETH` is measured in three different patterns:
  #
  # - 1999 to 2000 (DEMO)   has `RIDRETH1` and `RIDRETH2`
  # - 2001 to 2002 (DEMO_B) has `RIDRETH1` and `RIDRETH2`
  # - 2003 to 2003 (DEMO_C) has `RIDRETH1` and `RIDRETH2`
  # - 2005 to 2006 (DEMO_D) has `RIDRETH1` only
  # - 2007 to 2008 (DEMO_E) has `RIDRETH1` only
  # - 2009 to 2010 (DEMO_F) has `RIDRETH1` only
  # - 2011 to 2012 (DEMO_G) has `RIDRETH1` and `RIDRETH3`
  # - 2013 to 2014 (DEMO_H) has `RIDRETH1` and `RIDRETH3`
  # - 2015 to 2016 (DEMO_I) has `RIDRETH1` and `RIDRETH3`
  # - 2017 to 2018 (DEMO_J) has `RIDRETH1` and `RIDRETH3`
  # `RIDRETH3` includes a category for non-hispanic Asian participants,
  # and this should be incorporated in and after the 2011 exam.

  data_clean_race <- data_in %>%
    mutate(
      RIDRETH = if_else(
        exam %in% c(
          "1999-2000",
          "2001-2002",
          "2003-2004",
          "2005-2006",
          "2007-2008",
          "2009-2010"
        ),
        true = recode(
          RIDRETH1,
          '1' = 'Mexican American',
          '2' = 'Other Hispanic',
          '3' = 'Non-Hispanic White',
          '4' = 'Non-Hispanic Black',
          '5' = 'Other Race - Including Multi-Racial'
        ),
        false = recode(
          RIDRETH3,
          '1' = "Mexican American",
          '2' = "Other Hispanic",
          '3' = "Non-Hispanic White",
          '4' = "Non-Hispanic Black",
          '6' = "Non-Hispanic Asian",
          '7' = "Other Race - Including Multi-Racial"
        )
      ),
    )

  # education (DMDEDUC) -----------------------------------------------------
  #
  # The education data are collected using different questions for
  # NHANES participants <20 and 20+ years old. We derive a recoded
  # education variable for all participants with 3 categories:
  # Less than high school, High school/some college, and College graduate.

  suppressWarnings(
    data_clean_educ <- data_clean_race %>%
      mutate(
        edu_lt20 = recode(DMDEDUC3,
          "0" = 'Never Attended / Kindergarten Only',
          "1" = '1st Grade',
          "2" = '2nd Grade',
          "3" = '3rd Grade',
          "4" = '4th Grade',
          "5" = '5th Grade',
          "6" = '6th Grade',
          "7" = '7th Grade',
          "8" = '8th Grade',
          "9" = '9th Grade',
          "10" =  '10th Grade',
          "11" =  '11th Grade',
          "12" =  '12th Grade, No Diploma',
          "13" =  'High School Graduate',
          "14" =  'GED or Equivalent',
          "15" =  'More than high school',
          "55" =  'Less Than 5th Grade',
          "66" =  'Less Than 9th Grade',
          "77" =  'Refused',
          "99" =  'Unknown'
        ),
        edu_20plus = recode(
          DMDEDUC2,
          "1" = 'Less Than 9th Grade',
          "2" = '9th to 11th Grade (Includes 12th grade with no diploma)',
          "3" = 'High School Grad/GED or Equivalent',
          "4" = 'Some College or AA degree',
          "5" = 'College Graduate or above',
          "7" = 'Refused',
          "9" = 'Unknown'
        ),
        edu_lt20 = fct_collapse(
          factor(edu_lt20),
          "Less than high school" = c(
            "Never Attended / Kindergarten Only",
            "1st Grade",
            "2nd Grade",
            "3rd Grade",
            "4th Grade",
            "5th Grade",
            "6th Grade",
            "7th Grade",
            "8th Grade",
            "9th Grade",
            "10th Grade",
            "11th Grade",
            "12th Grade, No Diploma",
            "Less Than 5th Grade",
            "Less Than 9th Grade"
          ),
          "High school/some college" = c(
            "High School Graduate",
            "GED or Equivalent"
          ),
          "missing" = c("Refused", "Unknown", "More than high school")
        ),
        edu_20plus = fct_collapse(
          factor(edu_20plus),
          "Less than high school" = c(
            "Less Than 9th Grade",
            "9th to 11th Grade (Includes 12th grade with no diploma)"
          ),
          "High school/some college" = c(
            "High School Grad/GED or Equivalent",
            "Some College or AA degree"
          ),
          "College graduate" = c(
            "College Graduate or above"
          ),
          'missing' = c("Refused", "Unknown")
        ),
        education = as.character(coalesce(edu_20plus, edu_lt20)),
        education = factor(
          education,
          # missing is naturally coded as NA since it is not listed in levels
          levels = c(
            "Less than high school",
            "High school/some college",
            "College graduate"
          )
        )
      )
  )

  # income_hh (INDHHINC) ----------------------------------------------------

  # Income is measured differently from 1999 to 2005 and from 2007 and onward.
  # We handle these two variable types separately and then combine them.

  # income is different from 1999-2005 versus 2007 and onward
  income1_years <- c(
    '1999-2000',
    '2001-2002',
    '2003-2004',
    '2005-2006'
  )

  income2_years <- c(
    "2007-2008",
    "2009-2010",
    "2011-2012",
    "2013-2014",
    "2015-2016",
    "2017-2018"
  )

  data_clean_income <- data_clean_educ

  if ( !any(income1_years %in% unique(data_clean_educ$exam)) ) {
    data_clean_income$INDHHINC <- NA_character_
  }

  if ( !any(income2_years %in% unique(data_clean_educ$exam)) ) {
    data_clean_income$INDHHIN2 <- NA_character_
  }

  # warnings about unused factor levels may pop up when
  # some exam years are left out. They do not need to be printed
  suppressWarnings(
    data_clean_income <- data_clean_income %>%
      mutate(
        income_hh = if_else(
          exam %in% income1_years,
          true = recode(
            INDHHINC,
            "1" = "$0 to $4,999",
            "2" = "$5,000 to $9,999",
            "3" = "$10,000 to $14,999",
            "4" = "$15,000 to $19,999",
            "5" = "$20,000 to $24,999",
            "6" = "$25,000 to $34,999",
            "7" = "$35,000 to $44,999",
            "8" = "$45,000 to $54,999",
            "9" = "$55,000 to $64,999",
            "10" = "$65,000 to $74,999",
            "11" = "$75,000 and Over",
            "12" = "Over $20,000",
            "13" = "Under $20,000",
            "77" = "Refused",
            "99" = "Unknown"
          ),
          false = recode(
            INDHHIN2,
            "1" = "$0 to $4,999",
            "2" = "$5,000 to $9,999",
            "3" = "$10,000 to $14,999",
            "4" = "$15,000 to $19,999",
            "5" = "$20,000 to $24,999",
            "6" = "$25,000 to $34,999",
            "7" = "$35,000 to $44,999",
            "8" = "$45,000 to $54,999",
            "9" = "$55,000 to $64,999",
            "10" = "$65,000 to $74,999",
            "12" = "Over $20,000",
            "13" = "Under $20,000",
            "14" = "$75,000 to $99,999",
            "15" = "$100,000 and Over",
            "77" = "Refused",
            "99" = "Unknown"
          )
        )
      )
  )



  data_out <- data_clean_income %>%
    select(
      exam,
      seqn = SEQN,
      psu = SDMVPSU,
      strata = SDMVSTRA,
      wts_mec_2yr = WTMEC2YR,
      exam_status = RIDSTATR,
      age = RIDAGEYR,
      sex = RIAGENDR,
      race_ethnicity = RIDRETH,
      education,
      income_hh,
      pregnant = RIDEXPRG
    ) %>%
    mutate(
      sex = recode(sex, '1' = "Male", '2' = "Female"),
      pregnant = recode(
        pregnant,
        "1" = "yes",
        "2" = "no",
        "3" = NA_character_
      ),
      pregnant = replace(pregnant, sex == 'Male', 'no'),
      exam_status = recode(
        exam_status,
        '1' = 'interview only',
        '2' = 'interview and exam'
      )
    )

  # Pregnancy: Persons who reported they were pregnant at the time of exam
  # were assumed to be pregnant (RIDEXPRG=1). Those who reported they were not
  # pregnant or did not know their pregnancy status were further classified
  # based on the results of the urine pregnancy test. If the respondent reported
  # “no” or “don’t know” and the urine test result was positive, the respondent
  # was coded as pregnant (RIDEXPRG=1). If the respondent reported “no” and the
  # urine test was negative, the respondent was coded not pregnant (RIDEXPRG=2).
  # If the respondent reported did not know her pregnancy status and the urine
  # test was negative, the respondent was coded "could not be determined”
  # (RIDEXPRG=3). Persons who were interviewed, but not examined also have an
  # RIDEXPRG value = 3 (could not be determined)

  if(include_variable_labels){
    add_labels(data_out, var_guide)
  } else {
    data_out
  }




}
