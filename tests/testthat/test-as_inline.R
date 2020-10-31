

test_data <-
  data.frame(
    sex = c("female",
            "female",
            "female",
            "male",
            "male",
            "male"),
    eye_color = c("blue",
                  "brown",
                  "hazel",
                  "blue",
                  "brown",
                  "hazel"),
    tbv_height = c(
      "166 (166 - 168)",
      "158 (154 - 161)",
      "178 (178 - 178)",
      "178 (175 - 188)",
      "183 (179 - 184)",
      "170 (170 - 170)"
    ),
    tbv_mass = c(
      "56.2 (53.1 - 65.6)",
      "47.0 (46.0 - 48.0)",
      "55.0 (55.0 - 55.0)",
      "84.0 (79.0 - 112)",
      "79.5 (78.8 - 81.0)",
      "77.0 (77.0 - 77.0)"
    ),
    stringsAsFactors = FALSE
  )

test_data_with_na <- test_data

test_data_with_na$sex[c(1,2)] <- NA_character_

test_that(
  "example runs correctly",
  code = {
    expect_equal(
      as_inline(data = test_data,
                tbl_variables = c('sex', 'eye_color'),
                tbl_values = 'tbv_height'),
      list(
        female = list(
          blue = "166 (166 - 168)",
          brown = "158 (154 - 161)",
          hazel = "178 (178 - 178)"
        ),
        male = list(
          blue = "178 (175 - 188)",
          brown = "183 (179 - 184)",
          hazel = "170 (170 - 170)"
        )
      )
    )
  }
)

test_that(
  "NAs are made to be informative",
  code = {
    expect_equal(
      as_inline(
        data = test_data_with_na,
        tbl_variables = c('sex', 'eye_color'),
        tbl_values = 'tbv_height'
      ),
      list(
        female = list(hazel = "178 (178 - 178)"),
        male = list(
          blue = "178 (175 - 188)",
          brown = "183 (179 - 184)",
          hazel = "170 (170 - 170)"
        ),
        sex_missing = list(
          blue = "166 (166 - 168)",
          brown = "158 (154 - 161)"
        )
      )
    )
  }
)

test_that(
  "Factor levels are maintained",
  code = {
    test_data$eye_color <- factor(test_data$eye_color,
                                  levels = c('hazel', 'brown', 'blue'))
    expect_equivalent(
      as_inline(data = test_data,
                tbl_variables = c('sex', 'eye_color'),
                tbl_values = 'tbv_height'),
      list(
        female = list(
          hazel = "178 (178 - 178)",
          brown = "158 (154 - 161)",
          blue = "166 (166 - 168)"
        ),
        male = list(
          hazel = "170 (170 - 170)",
          brown = "183 (179 - 184)",
          blue = "178 (175 - 188)"
        )
      )
    )
  }
)


