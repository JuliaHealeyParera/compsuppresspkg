test_that("checker_df() returns dataframe", {
  target_class <- "data.frame"
  df <- checker_df(
    suppressed_df(),
    supp_col = c('x', 'y', 'z'),
    regex_char = rc()
  )

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

test_that("checker_df() only returns suppression columns", {
  df <- suppressed_df()
  rc <- rc()

  # all columns
  expect_equal(
    names(
      checker_df(
        df,
        supp_col = c('x', 'y', 'z'),
        regex_char = rc
      )
    ),
    c('x', 'y', 'z')
  )

  # select columns
  expect_equal(
    names(
      checker_df(
        df,
        supp_col = c('y', 'z'),
        regex_char = rc
      )
    ),
    c('y', 'z')
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
  some_bad_cols <- c("y", "w")
  all_bad_cols <- c("q", "g")
  good_cols <- c("x", "y")

  # all bad cols
  expect_error(
    checker_df(
      df = df,
      supp_col = all_bad_cols,
      regex_char = rc
    )
  )

  # some cols
  expect_error(
    checker_df(
      df = df,
      supp_col = some_bad_cols,
      regex_char = rc
    )
  )

  # good to go
  expect_no_error(
    checker_df(
      df = df,
      supp_col = good_cols,
      regex_char = rc
    )
  )
})

test_that("checker_df() allows number as regex character", {
  df <- suppressed_df()
  comparison_df <-
    data.frame(
      x = c(0, 0, 1),
      y = c(1, 0, 0)
    )

  expect_equal(
    checker_df(
      df = df,
      supp_col = c('x', 'y'),
      '4'
    ),
    comparison_df
  )

  # I can't rememebr exactly how crucial
  # this func is - if it sees alot of use
  # we may want to allow numerics, maybe non-standard eval
  # and convert before eval etc...

  # expect_equal(
  #   checker_df(
  #     df = df,
  #     supp_col = c('x', 'y'),
  #     4
  #   ),
  #   comparison_df
  # )
})

test_that("checker_df() requires suppression character", {
  df <- suppressed_df()
  empty_char <- ""
  cols <- cols()

  # empty char
  expect_error(
    checker_df(
      df = df,
      supp_col = cols,
      regex_char = empty_char
    )
  )

  # nothing
  expect_error(
    checker_df(
      df = df,
      supp_col = cols
    )
  )

  # good to go
  expect_no_error(
    checker_df(
      df = df,
      supp_col = cols,
      regex_char = "&"
    )
  )
})
