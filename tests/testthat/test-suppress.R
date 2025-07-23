test_that("suppress() returns correct dataframe", {
  df <- data.frame(
    x = c(1, 6, 8),
    y = c(3, 6, 2),
    z = c(10, 4, 9)
  )
  cols <- c('x', 'y', 'z')
  rc <- '-'
  val <- 5
  totals <- "row"

  sol1 <- data.frame(
    x = c('-', '-', '-'),
    y = c('-', '6', '-'),
    z = c('10', '-', '-'),
    Total = c('14', '16', '19')
  )
  sol2 <- data.frame(
    x = c('-', '6', '-'),
    y = c('-', '-', '-'),
    z = c('10', '-', '-'),
    Total = c('14', '16', '19')
  )
  function_sol <- suppress(df, val, rc, cols, totals)

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

test_that("suppress() returns dataframe", {
  target_class <- "data.frame"
  df <- data.frame(
    x = c(1, 6, 8),
    y = c(3, 6, 2),
    z = c(10, 4, 9)
  )
  cols <- c('x', 'y', 'z')
  rc <- '-'
  val <- 5
  totals <- "row"

  # one index to choose
  expect_equal(
    class(suppress(df, val, rc, cols, totals)),
    target_class
  )
})

test_that("suppress() takes proper inputs", {
  df <- data.frame(
    x = c(1, 6, 8),
    y = c(3, 6, 2),
    z = c(10, 4, 9)
  )
  cols <- c('x', 'y', 'z')
  rc <- '-'
  val <- 5
  totals <- "row"

  # empty dataframe
  expect_error(
    suppress(
      data.frame(),
      val,
      rc,
      cols,
      totals
    )
  )

  # empty suppression column vector
  expect_error(
    suppress(
      df,
      val,
      rc,
      c(),
      totals
    )
  )

  # missing inputs
  expect_error(
    suppress(
      df,
      val,
      rc,
      totals
    )
  )

  # suppression value should be number
  expect_error(
    suppress(
      df,
      'string',
      rc,
      cols,
      totals
    )
  )

  # suppression character should not be a number
  expect_error(
    suppress(
      df,
      val,
      3,
      c(),
      totals
    )
  )
})
