# Thoughts:
# Should we define a new data object type, one that has print value and underlying value,
# to allow for supppressed values to be used in aggregate calculations?
# Process as NA/0 for purporses of aggregation, but input special char. for purposes
# of printing

#' Check a dataframe for current suppression
#'
#' @param df Dataframe to be checked.
#' @param supp_cols Columns to be checked for suppression.
#' @param regex_suppchar Suppression character to check for (special char. processed)
#'
#' @returns A dataframe.
#' @export
#'
#' @examples
#' x <- data.frame(x = c('2', '*', '*', '3', '5', '2'), y = c('*', '3', '5', '*', '2', '*'))
#' supp_col <- c('x','y')
#' regex_suppchar <- '\\*'
checker_df <- function(df, supp_cols, regex_suppchar) {
  checker <- df |>
    # If a value has been suppressed, replace it with 1.
    # If a value has not been suppressed, replace it with 0.
    mutate_at(supp_cols, ~ as.numeric(str_detect(., regex_suppchar)))

  return (checker)
}
