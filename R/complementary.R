#' Recursively fix insufficiently-suppressed rows and columns in dataframe
#'
#' @param df Dataframe to be checked and fixed for necessary suppression.
#' @param supp_col Vector of columns to be checked for suppression.
#' @param supp_col_idx Indices of supp_col in df.
#' @param regex_char Character to replace suppressed value.
#'
#' @returns A dataframe.
#' @export
#'
#' @examples
#' x <-
#'  data.frame(
#'    x = c('-', '6', '8'),
#'    y = c('-', '6', '-'),
#'    z = c('10', '-', '9')
#' )
#'
#' complementary(x, c('x', 'y', 'z'), c(1,2,3), '-')
complementary <- function(df, supp_col, supp_col_idx, regex_char) {
  checkmate::assert(
    checkmate::check_data_frame(
      df,
      any.missing = FALSE,
      min.rows = 1,
      min.cols = 1
    ),
    checkmate::check_atomic_vector(
      supp_col
    ),
    checkmate::check_atomic_vector(
      supp_col_idx
    ),
    checkmate::check_numeric(
      supp_col_idx
    ),
    combine = "and"
  )

  # Checker dataframe needs to be remade every recursive call
  checker <- checker_df(
    df,
    supp_col,
    regex_char
  )

  # Base case: check_rows() returns NULL AND check_cols() returns NA
  rows_to_fix <- check_rows(
    checker,
    supp_col
  )

  cols_to_fix <- check_cols(
    checker,
    supp_col
  )

  # Base case, both rows_to_fix and cols_to_fix are NA
  if (base::length(rows_to_fix) == 1 && base::is.na(rows_to_fix)) {
    if (base::length(cols_to_fix) == 1 && base::is.na(cols_to_fix)) {
      return(df)
    }
    # Fix rows
  } else {
    df <- purrr::reduce(
      .x = rows_to_fix,
      .f = function(acc, i) {
        fix_row(
          acc, # For each call to fix_row, first argument is newly edited dataframe
          i, # For each call to fix_row, second argument is next row to fix
          supp_col_idx,
          regex_char
        )
      },
      .init = df # Initial input: (current, from this recursive call) df
    )

    # Need to re-evaluate df since columns have changed
    checker <- checker_df(
      df,
      supp_col,
      regex_char
    )

    cols_to_fix <- check_cols(
      checker,
      supp_col
    )
  }

  # Fix columns
  if (!(base::length(cols_to_fix) == 1) || !(base::is.na(cols_to_fix))) {
    df <- purrr::reduce(
      .x = cols_to_fix,
      .init = df,
      .f = function(acc, col_idx) {
        fix_col(acc, col_idx, regex_char)
      }
    )
  }

  # Recurse with edited dataframe
  complementary(df, supp_col, supp_col_idx, regex_char)
}
