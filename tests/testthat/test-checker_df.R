test_that("checker_df() returns dataframe", {
  expect_equal(
    class(checker_df(
      data.frame(
        x = c('*', '3', '4'),
        y = c('4', '1', '*')
      ),
      c('x', 'y'),
      '\\*'
    )),
    'data.frame'
  )
})

test_that("checker_df() requires suppression columns", {
  expect_error(
    checker_df(
      data.frame(
        x = c('*', '3', '4'),
        y = c('4', '1', '*')
      ),
      c(),
      '\\*'
    )
  )
})

test_that("checker_df() requires dataframe as input", {
  expect_error(
    checker_df(
      "string as improper input that should throw error",
      c('x', 'y'),
      '\\*'
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
