test_that("check_cols() returns correct column indices", {
  checker <- checker_df_burner()
  cols <- cols() # c('x', 'y'), default

  # select columns
  expect_equal(
    check_cols(checker, cols),
    c(1, 2)
  )

  # all columns
  expect_equal(
    check_cols(checker, c('x', 'y', 'z')),
    c(1,2)
  )

  # one column
  expect_equal(
    check_cols(checker, c('z')),
    NA_integer_
  )
})

test_that("check_cols() returns integer vector", {
  target_class <- "integer"
  checker <- checker_df_burner()
  cols <- c('x', 'y', 'z')

  expect_equal(
    class(check_cols(checker, cols)),
    target_class
  )
})

test_that("check_rows() takes proper inputs", {
  cols <- cols() # c('x', 'y'), default

  # empty dataframe
  expect_error(
    check_cols(
      data.frame(),
      cols)
  )

  # empty suppression column vector
  expect_error(
    check_cols(
      df,
      c())
  )

  # suppression columns fully not in df
  expect_error(
    check_cols(
      df,
      c('w', 'h')
    )
  )

  # suppression columns partially not in df
  expect_error(
    check_cols(
      df,
      c('z', 'h')
    )
  )

  # df not of proper format
  expect_error(
    check_cols(
      data.frame(x = c('this', 'should', 'throw'),
                 y = c('an', 'error', 'otherwise'),
                 z = c('function', 'not', 'right')),
      c('x', 'y', 'z')
    )
  )
})
