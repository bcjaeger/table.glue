## code to prepare `nhanes` dataset goes here

library(dplyr)
library(glue)
library(magrittr)
library(purrr)
library(haven)
library(forcats)
library(tibble)
library(labelled)
library(tidyr)

to_source <- setdiff(list.files(path = 'data-raw/'), 'nhanes.R')

for(f in to_source) source(paste0('data-raw/', f))

exams = c(2013, 2015, 2017)
demo = clean_demo(exams)
exam_bp = clean_exam_bp(exams)
qx_high_bp = clean_qx_high_bp(exams)

nhanes = reduce(
  .x = list(demo,exam_bp,qx_high_bp),
  .f = left_join,
  by = c('exam', 'seqn')
) %>%
  select(-income_hh) %>%
  mutate(
    race_ethnicity = recode(
      race_ethnicity,
      "Mexican American" = 'hispanic',
      "Non-Hispanic Asian" = 'asian',
      "Non-Hispanic Black" = 'black',
      "Non-Hispanic White" = 'white',
      "Other Hispanic" = 'other',
      "Other Race - Including Multi-Racial" = 'other'
    ),
    sex = tolower(sex),
    exam = recode(
      exam,
      '2013-2014' = 'exam_2013_2014',
      '2015-2016' = 'exam_2015_2016',
      '2017-2018' = 'exam_2017_2018'
    ),
    education = recode(
      education,
      "Less than high school" = 'less_than_highschool',
      "High school/some college" = 'highschool_or_some_college',
      "College graduate" = 'college_graduate'
    )
  )



usethis::use_data(nhanes, overwrite = TRUE)
