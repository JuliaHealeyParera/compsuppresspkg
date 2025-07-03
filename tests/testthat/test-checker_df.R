test_that("checker_df() returns dataframe", {
  target_class <- "data.frame"
  df <- suppressed_df()

  expect_equal(
    class(df),
    target_class
  )
})

test_that("checker_df() requires suppression columns", {
  # making it easy to swap in different parameters later
  df <- suppressed_df()
  rc <- rc()

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
  # making it easy to swap in different parameters later
  rc <- rc()

  # string instead of dataframe
  expect_error(
    checker_df(
      df = "string as improper input that should throw error",
      supp_col = c('x', 'y'),
      regex_char = rc
    )
  )

  # NULL instead of dataframe
  expect_error(
    checker_df(
      df = NULL,
      supp_coll = c('x', 'y'),
      regex_char = rc
    )
  )

  # dataframe but empty
  expect_error(
    checker_df(
      data.frame(),
      supp_col = c('x'),
      regex_char = rc
    )
  )

  # dataframe should be good
  expect_no_error(
    checker_df(
      df = unsuppressed_df(),
      supp_col = c('x'),
      regex_char = rc
    )
  )

  # tibble should be good
  expect_no_error(
    checker_df(
      df = unsuppressed_df() |> dplyr::as_tibble(),
      supp_col = c('x'),
      regex_char = rc
    )
  )
})

test_that("checker_df() requires supp_col to be in df", {
  df <- suppressed_df()
  rc <- rc()
  bad_cols <- c("y", "z")
  good_cols <- c("x", "y")

  expect_error(
    checker_df(
      df = df,
      supp_col = bad_cols,
      regex_char = rc
    )
  )

  expect_no_error(
    checker_df(
      df = df,
      supp_col = good_cols,
      regex_char = rc
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
