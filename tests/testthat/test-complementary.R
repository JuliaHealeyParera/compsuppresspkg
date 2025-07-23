test_that("complementary() returns correct dataframe", {
  df <- data.frame(
    x = c('-', '6', '8'),
    y = c('-', '6', '-'),
    z = c('10', '-', '9')
  )
  cols <- c('x', 'y', 'z')
  col_idx <- supp_col_idx(df, cols)
  rc <- '-'

  sol1 <- data.frame(
    x = c('-', '-', '-'),
    y = c('-', '6', '-'),
    z = c('10', '-', '-')
  )
  sol2 <- data.frame(
    x = c('-', '6', '-'),
    y = c('-', '-', '-'),
    z = c('10', '-', '-')
  )
  function_sol <- complementary(df, cols, col_idx, rc)

  expect_true(
    isTRUE(
      all.equal(
        function_sol,
        sol1
      )
    ) |
      isTRUE(
        all.equal(
          function_sol,
          sol2
        )
      )
  )
})

test_that("complementary() returns dataframe", {
  target_class <- "data.frame"
  df <- suppressed_df()
  cols <- c('x', 'y', 'z')
  col_idx <- supp_col_idx(df, cols)
  rc <- '-'

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
