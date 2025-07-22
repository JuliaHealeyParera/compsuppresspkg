#' Fix column with insufficient number of suppressed values.
#'
#' @param df Dataframe with row to be fixed.
#' @param i Index of column to be fixed.
#' @param rc_char Character to replace suppressed value.
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
#' fix_col(x, 1, '-')
#' fix_col(x, 3, '-')
fix_col <- function(df, i, rc_char) {
  checkmate::assert(
    checkmate::check_data_frame(
      df,
      any.missing = FALSE,
      min.rows = 1,
      min.cols = 1
    ),
    checkmate::check_numeric(
      i,
      lower = 1,
      upper = ncol(df)
    ),
    combine = "and"
  )

  # Save column to be fixed as vector
  col <- df[, i]
  # Convert to numeric, remove suppressed values and 0s (should not be supp.)
  num_col <- base::as.numeric(
    col[
      !(col %in% c(rc_char, "0"))
    ]
  )

  # Randomly choose the index of one cell (equal to the minimum column value)
  poss_idx <- base::which(col == min(num_col))
  if (base::length(poss_idx) != 1) {
    idx_to_supp <- base::sample(
      # If multiple indices, sample
      poss_idx,
      size = 1
    )
  } else {
    idx_to_supp <- poss_idx
  }

  # Suppress selected cell with suppression character
  # psuedocode: df[idx. of row to be suppressed, idx. of fixed column]
  df[idx_to_supp, i] <- rc_char
  return(df)
}
