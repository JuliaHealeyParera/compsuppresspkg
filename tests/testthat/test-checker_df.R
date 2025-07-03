test_that("checker_df() returns dataframe", {
  target_class <- "data.frame"
  df <- suppressed_df()

  expect_equal(
    class(df),
    target_class
  )
})

test_that("checker_df() requires suppression columns", {
  df <- suppressed_df()
  rc <- "\\*"

  # empty vector provided
  expect_error(
    checker_df(
      df,
      supp_col = c(),
      regex_char = rc
    )
  )

  # no argument provided
  expect_error(
    checker_df(
      df,
      regex_char = rc
    )
  )
})

test_that("checker_df() requires dataframe as input", {
  # string instead of dataframe
  expect_error(
    checker_df(
      "string as improper input that should throw error",
      c('x', 'y'),
      '\\*'
    )
  )

  # NULL instead of dataframe
  expect_error(
    checker_df(
      NULL,
      c('x', 'y'),
      '\\*'
    )
  )

  # dataframe but empty
  expect_error(
    checker_df(
      data.frame(),
      c('x'),
      "\\&"
    )
  )

  # dataframe should be good
  expect_no_error(
    checker_df(
      df = unsuppressed_df(),
      supp_col = c('x'),
      regex_char = "\\^"
    )
  )

  # tibble should be good
  expect_no_error(
    checker_df(
      df = unsuppressed_df() |> dplyr::as_tibble(),
      supp_col = c('x'),
      regex_char = "\\^"
    )
  )
})

test_that("checker_df() requires supp_col to be in df", {
  expect_error(
    checker_df(
      data.frame(
        x = c('*', '3', '4'),
        y = c('4', '1', '*')
      ),
      c('z', 'y'),
      '\\*'
    )
  )
})

test_that("checker_df() allows number as regex character", {
  expect_equal(
    checker_df(
      data.frame(
        x = c('*', '3', '4'),
        y = c('4', '1', '*')
      ),
      c('x', 'y'),
      '4'
    ),
    data.frame(
      x = c(0, 0, 1),
      y = c(1, 0, 0)
    )
  )
})

test_that("checker_df() requires suppression character", {
  expect_error(
    checker_df(
      data.frame(
        x = c('*', '3', '4'),
        y = c('4', '1', '*')
      ),
      c('x', 'y'),
      ''
    )
  )
})
