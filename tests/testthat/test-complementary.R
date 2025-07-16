test_that("complementary() returns correct dataframe", {
  df <- data.frame(x = c('*', '13', '8'),
                   y = c('9', '6', '*'),
                   z = c('*', '8', '8'))
  cols <- cols()
  col_idx <- supp_col_idx(df, cols)
  rc <- '*'

  # two indices to choose from
  second_burner_df <- data.frame(x = c('*', '13', '8'),
                                 y = c('9', '6', '*'),
                                 z = c('*', '8', '8'),
                                 w = c('6', '8', '12'))
})

test_that("complementary() returns dataframe", {
  target_class <- "data.frame"
  df <- suppressed_df()
  cols <- cols()
  col_idx <- supp_col_idx(df, cols)
  rc <- '*'

  # one index to choose
  expect_equal(
    class(complementary(df, cols, col_idx, rc)),
    target_class
  )
})

test_that("complementary() takes proper inputs", {
  df <- suppressed_df()
  cols <- cols()
  col_idx <- supp_col_idx(df, c('x', 'y', 'z'))
  rc <- '*'

  # empty dataframe
  expect_error(
    complementary(
      data.frame(),
      cols,
      col_idx,
      rc
    )
  )

  # empty suppression column vector
  expect_error(
    complementary(
      df,
      cols,
      c(),
      rc
    )
  )

  # missing inputs
  expect_error(
    fix_row(
      df,
      c('x', 'y', 'z'),
      rc
    )
  )
})
