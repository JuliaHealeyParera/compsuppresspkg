#' Check checker dataframe column-wise for proper number of suppressed values.
#'
#' @param checker Dataframe to be analyzed, product of checker_df().
#' @param supp_col Columns to be checked.
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
#' check_cols(x, c('x', 'y', 'z'))
#' check_cols(x, c('z'))
check_cols <- function(checker, supp_col) {
  checkmate::assert(
    checkmate::check_data_frame(
      checker,
      types = c("double", "integer", "numeric"),
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

  # What are the indices of the columns that need to be fixed (have only 1 value suppressed)?
  fix_cols <- as.vector(
    base::which(
      base::colSums(
        checker[
          base::which(
            base::names(checker) %in% supp_col
            )
          ]
        ) == 1
      )
    )

  if (length(fix_cols) == 0) {
    return(NA_integer_)
  }

  return (fix_cols)
}
