test_that("fix_row() returns correct dataframe", {
  df <- suppressed_df()
  col_idx <- supp_col_idx(df, c('x', 'y', 'z'))
  rc <- '-'

  # one index to choose
  expect_equal(
    fix_row(df, 3, col_idx, rc),
    data.frame(x = c('-', '3', '4'), y = c('4', '1', '-'), z = c('-', '2', '-'))
  )

  # two indices to choose from
  second_burner_df <- data.frame(
    x = c('*', '13', '8'),
    y = c('9', '6', '*'),
    z = c('*', '8', '8'),
    w = c('6', '8', '12')
  )
  second_burner_sol1 <- data.frame(
    x = c('*', '13', '*'),
    y = c('9', '6', '*'),
    z = c('*', '8', '8'),
    w = c('6', '8', '12')
  )
  second_burner_sol2 <- data.frame(
    x = c('*', '13', '8'),
    y = c('9', '6', '*'),
    z = c('*', '8', '*'),
    w = c('6', '8', '12')
  )
  function_sol <- fix_row(second_burner_df, 3, c(1, 2, 3, 4), '*')

  expect_true(
    isTRUE(
      all.equal(
        function_sol,
        second_burner_sol1
      )
    ) |
      isTRUE(
        all.equal(
          function_sol,
          second_burner_sol2
        )
      )
  )
})

test_that("fix_row() returns dataframe", {
  target_class <- "data.frame"
  df <- suppressed_df()
  col_idx <- supp_col_idx(df, c('x', 'y', 'z'))
  rc <- '-'

  # one index to choose
  expect_equal(
    class(fix_row(df, 3, col_idx, rc)),
    target_class
  )
})

test_that("fix_row() takes proper inputs", {
  df <- suppressed_df()
  col_idx <- supp_col_idx(df, c('x', 'y', 'z'))
  rc <- '-'

  # empty dataframe
  expect_error(
    fix_row(
      data.frame(),
      3,
      col_idx,
      rc
    )
  )

  # empty suppression column vector
  expect_error(
    fix_row(
      df,
      3,
      c(),
      rc
    )
  )

  # character suppression column vector
  expect_error(
    fix_row(
      df,
      3,
      c('x', 'y', 'z'),
      rc
    )
  )
})
