complementary <- function(df, supp_col, supp_col_idx, rc_char) {
  checkmate::assert(
    checkmate::check_data_frame(
      df,
      any.missing = FALSE,
      min.rows = 1,
      min.cols = 1
    ),
    checkmate:: check_atomic_vector(
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
  rc_cleared_char <- dplyr::if_else(rc_char %in% c('*', '.'), paste0('\\', rc_char), rc_char)

  checker <- checker_df(
    df,
    supp_col,
    rc_cleared_char
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
        .f = function(acc, i) fix_row(
          acc, # For each call to fix_row, first argument is newly edited dataframe
          i,   # For each call to fix_row, second argument is next row to fix
          supp_col_idx,
          rc_char
          ),
        .init = df # Initial input: (current, from this recursive call) df
      )

    # Need to re-evaluate df since columns have changed
    checker <- checker_df(
      df,
      supp_col,
      rc_cleared_char
    )

    cols_to_fix <- check_cols(
      checker,
      supp_col
    )
  }

  # Fix columns
  if (base::length(cols_to_fix) != 1) {
    df <- purrr::reduce(
      .x = cols_to_fix,
      .init = df,
      .f = function(acc, col_idx) {fix_col(acc, col_idx, rc_char)}
    )
  }

  # Recurse with edited dataframe
  complementary(df, supp_col, supp_col_idx, rc_char)
}
