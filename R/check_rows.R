#' Check checker dataframe row-wise for proper number of suppressed values.
#'
#' @param checker Dataframe to be analyzed, product of checker_df().
#' @param supp_col Columns to be checked row-wise.
#'
#' @returns An integer vector.
#' @export
#'
#' @examples
#' x <-
#'  data.frame(
#'    x = c(1, 0, 0),
#'    y = c(0, 1, 1),
#'    z = c(0, 1, 0)
#' )
#'
#' check_rows(x, c('x', 'y', 'z'))
#' check_rows(x, c('z'))
check_rows <- function(checker, supp_col) {
  # Check User Input
  # Theoretically, should only accept df of checker format... build in check later
  checkmate::assert(
    checkmate::check_data_frame(
      checker,
      types = c("double", "integer", "numeric"), # checker_df() returns dbl -- restrict to dbl?
      any.missing = FALSE,
      min.rows = 1,
      min.cols = 1
    ),
    checkmate::check_character(
      supp_col,
      min.len = 1
    ),
    checkmate::check_subset(supp_col, names(checker)),
    combine = "and"
  )

  supp_col_idx <- supp_col_idx(checker, supp_col)

  # How many cells in a given row have been suppressed?
  if (length(supp_col_idx) > 1) {
    row_checker <- checker |>
      dplyr::mutate(
        num_supp = base::rowSums(
          checker[, supp_col_idx] #All rows, only suppression columns
        )
      )

    # What are the indices of the rows that need to be fixed (have only 1 value suppressed)?
    fix_rows <- base::which(
      row_checker$num_supp == 1
    )
  } else {
    fix_rows <- base::which(
      checker[supp_col] == 1
    )
  }

  if (length(fix_rows) == 0) {
    return(NA_integer_)
  }

  return(fix_rows)
}
