##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param exams
make_exam_files <- function(exams, data_label) {

  exam_chr <- as.character(exams)

  exam_names <- recode(exam_chr,
    "1999" = "1999-2000",
    "2001" = "2001-2002",
    "2003" = "2003-2004",
    "2005" = "2005-2006",
    "2007" = "2007-2008",
    "2009" = "2009-2010",
    "2011" = "2011-2012",
    "2013" = "2013-2014",
    "2015" = "2015-2016",
    "2017" = "2017-2018"
  )

  exam_letters <- recode(exam_chr,
    "1999" = "",
    "2001" = "_B",
    "2003" = "_C",
    "2005" = "_D",
    "2007" = "_E",
    "2009" = "_F",
    "2011" = "_G",
    "2013" = "_H",
    "2015" = "_I",
    "2017" = "_J"
  )

  glue(
    "https://wwwn.cdc.gov/Nchs/Nhanes/",
    "{exam_names}/{data_label}{exam_letters}.XPT"
  ) %>%
    as.character() %>%
    set_names(exam_names)

}
