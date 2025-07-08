#' Standard unsuppressed dataframe
#'
#' @returns A dataframe.
#'
#' @examples
#' unsuppressed_df()
unsuppressed_df <- function() {
  data.frame(
    x = c(1, 3, 4),
    y = c(4, 1, 2),
    z = c(1, 2, 3)
  )
}

#' Standard partially suppressed dataframe
#'
#' @returns A dataframe.
#'
#' @examples
#' suppressed_df()
suppressed_df <- function() {
  data.frame(
    x = c('*', '3', '4'),
    y = c('4', '1', '*'),
    z = c('*', '2', '3')
  )
}


#' Standard suppression regex character
#'
#' @returns A character string.
#'
#' @examples
#' rc()
rc <- function() {
  return("\\*")
}


#' Standard suppression columns
#'
#' @returns A character vector.
#'
#' @examples
#' cols()
cols <- function() {
  return(c('x', 'y'))
}
