#' Fix row with insufficient number of suppressed values.
#'
#' @param df Dataframe with row to be fixed.
#' @param i Index of row to be fixed.
#' @param supp_col_idx Vector of indices of suppression columns.
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
#'    z = c('10', '0', '9')
#' )
#'
#' fix_row(x, 3, c(1,2,3), '-')
#' fix_row(x, 3, c(1,2) , '-')
fix_row <- function(df, i, supp_col_idx, rc_char) {
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
      upper = nrow(df)
    ),
    checkmate::check_atomic_vector(
      supp_col_idx,
      min.len = 1
    ),
    checkmate::check_numeric(
      supp_col_idx
    ),
    combine = "and"
  )

  # Save row to be fixed as vector
  row <- df[i, supp_col_idx]
  # Remove suppressed values (rc_char) and convert numbers from char to numeric
  num_row <- base::as.numeric(row[!(row %in% c(rc_char, "0"))])

  # Extract indices of minimum value in row
  # These are the possible indices that could be used for suppression
  poss_idx <- base::which(row == min(num_row))

  # If there is more than one possible index, randomly sample
  if (base::length(poss_idx) != 1) {
    idx_to_supp <- base::sample(
      poss_idx,
      size = 1
    ) -
      1 # Subtract one to switch from 1-based indexing to 0-based indexing
  } else {
    # Else, use only available index
    idx_to_supp <- poss_idx - 1 # Switch to 0-based indexing for subsetting
  }

  # Suppress selected cell with suppression character
  df[
    i,
    idx_to_supp + supp_col_idx[1] # Idx within supp_col + first supp_col idx
  ] <- rc_char

  return(df)
}
