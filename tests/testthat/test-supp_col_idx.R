test_that("supp_col_idx() returns correct indices", {
  df <- unsuppressed_df()
  cols <- cols() # c('x', 'y'), default

  # select columns
  expect_equal(
    supp_col_idx(df, cols),
    c(1, 2)
  )

  # all columns
  expect_equal(
    supp_col_idx(df, c('x', 'y', 'z')),
    c(1, 2, 3)
  )

  # one column
  expect_equal(
    supp_col_idx(df, c('z')),
    c(3)
  )
})

test_that("supp_col_idx() returns integer vector", {
  target_class <- "integer"
  df <- unsuppressed_df()
  cols <- cols() # c('x', 'y'), default

  expect_equal(
   class(supp_col_idx(df, cols)),
   target_class
  )
})

test_that("supp_col_idx() takes proper inputs", {
  df <- suppressed_df()
  cols <- cols() # c('x', 'y'), default

  # empty dataframe
  expect_error(
    supp_col_idx(
      data.frame(),
      cols)
    )

  # empty suppression column vector
  expect_error(
    supp_col_idx(
      df,
      c())
    )

  # suppression columns fully not in df
  expect_error(
    supp_col_idx(
      df,
      c('w', 'h')
    )
  )

  # suppression columns partially not in df
  expect_error(
    supp_col_idx(
      df,
      c('z', 'h')
    )
  )
})



