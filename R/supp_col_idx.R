#' Return indices of dataframe suppression columns.
#'
#' @param df Dataframe to be scanned.
#' @param supp_col Columns to be searched for.
#'
#' @returns An integer vector.
#' @export
#'
#' @examples
#' x <-
#'  data.frame(
#'    x = c(1, 2, 3),
#'    y = c(3, 4, 5),
#'    z = c(5, 6, 7)
#' )
#'
#' supp_col_idx(x, c('x', 'y'))
#' supp_col_idx(x, c('z'))
supp_col_idx <- function(df, supp_col) {
  # Check User Input
  checkmate::assert(
    checkmate::check_data_frame(
      df,
      min.rows = 1,
      min.cols = 1
    ),
    checkmate::check_character(
      supp_col,
      min.len = 1
    ),
    checkmate::check_subset(supp_col, names(df)),
    combine = "and"
  )

  # Get indices for suppression columns in dataframe
  indices <- base::which(
    base::names(df) %in% supp_col
  )

  return(indices)
}
