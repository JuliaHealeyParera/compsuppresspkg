# Thoughts:
# Should we define a new data object type, one that has print value and underlying value,
# to allow for suppressed values to be used in aggregate calculations?
# Process as NA/0 for purposes of aggregation, but input special char. for purposes
# of printing

# Response:
# That is a cool idea - and I kinda want to do it for fun because I think
# that would be useful for many things and I don't know how I would do
# that in something like R. However - folks, or pipelines
# will already have access to the raw data in memory to give the functions.
# So they will have an unsuppressed version - so I we should return a suppressed
# dataframe to keep it simple to get the job done - but we should circle back
# to that - also we should get a github project going for discussions :D

#' Check a dataframe for current suppression
#'
#' @param df Dataframe to be checked.
#' @param supp_col Columns to be checked for suppression.
#' @param regex_char Suppression character to check for (special char. processed)
#'
#' @returns A dataframe.
#' @export
#'
#' @examples
#' x <-
#'  data.frame(
#'    x = c('2', '*', '*', '3', '5', '2'),
#'    y = c('*', '3', '5', '*', '2', '*')
#' )
#'
#' checker_df(x, c('x','y'), '\\*')
#' checker_df(x, c('x'), '2')
checker_df <- function(df, supp_col, regex_char) {
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

  checkmate::assert(
    checkmate::check_character(regex_char, min.chars = 1, all.missing = FALSE),
    checkmate::check_numeric(regex_char),
    combine = "or"
  )

  # If a value has been suppressed, replace it with 1.
  # If a value has not been suppressed, replace it with 0.
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(supp_col),
        \(col) {
          as.numeric(
            stringr::str_detect(col, regex_char)
          )
        }
      )
    )
}
