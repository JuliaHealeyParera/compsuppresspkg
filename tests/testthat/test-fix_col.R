test_that("fix_col() returns correct dataframe", {
  df <- suppressed_df()
  rc <- '*'

  # one index to choose
  expect_equal(
    fix_col(df, 1, rc),
    data.frame(x = c('*', '*', '4'),
               y = c('4', '1', '*'),
               z = c('*', '2', '3'))
  )

  # two indices to choose from
  second_burner_df <- data.frame(x = c('*', '13', '8'),
                                 y = c('9', '6', '*'),
                                 z = c('*', '8', '8'),
                                 w = c('6', '8', '12'))
  second_burner_sol1 <- data.frame(x = c('*', '13', '8'),
                                   y = c('9', '6', '*'),
                                   z = c('*', '*', '8'),
                                   w = c('6', '8', '12'))
  second_burner_sol2 <- data.frame(x = c('*', '13', '8'),
                                   y = c('9', '6', '*'),
                                   z = c('*', '8', '*'),
                                   w = c('6', '8', '12'))
  function_sol <- fix_col(second_burner_df, 3, '*')

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

test_that("fix_col() returns dataframe", {
  target_class <- "data.frame"
  df <- suppressed_df()
  rc <- '*'

  # one index to choose
  expect_equal(
    class(fix_col(df, 3, rc)),
    target_class
  )
})

test_that("fix_col() takes proper inputs", {
  df <- suppressed_df()
  rc <- '*'

  # empty dataframe
  expect_error(
    fix_col(
      data.frame(),
      3,
      rc
    )
  )

  # no suppression character
  expect_error(
    fix_col(
      df,
      3
    )
  )

  # column index greater than ncol(df)
  expect_error(
    fix_col(
      df,
      5,
      rc
    )
  )
})
