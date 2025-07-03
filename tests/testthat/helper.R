#' Standard unsuppressed dataframe
#'
#' @returns A dataframe.
#'
#' @examples
#' unsuppressed_df()
unsuppressed_df <- function() {
  data.frame(
    x = c(1, 3, 4),
    y = c(4, 1, 2)
  )
}

#' Standard suppressed dataframe
#'
#' @returns A dataframe.
#'
#' @examples
#' suppressed_df()
suppressed_df <- function() {
  data.frame(
    x = c('*', '3', '4'),
    y = c('4', '1', '*')
  )
}
