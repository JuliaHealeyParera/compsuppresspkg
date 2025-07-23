#' Use complementary suppression to protect small cell counts.
#'
#' @param df Dataframe to be fixed for necessary suppression.
#' @param supp_val Maximum value to be suppressed (all values equal to or smaller suppressed).
#' @param supp_char Character to be used to indicate suppressed value.
#' @param supp_col Vector of columns to be checked for suppression.
#' @param totals "rows", "totals", both, or "none" Will dictate which totals are returned with the df.
#'
#' @returns A dataframe.
#' @export
#'
#' @examples
#' x <-
#'  data.frame(
#'    x = c('1', '6', '8'),
#'    y = c('2', '6', '0'),
#'    z = c('10', '4', '9')
#' )
#'
#' suppress(x, 5, '-', c('x', 'y', 'z'), "row")
suppress <- function(df, supp_val, supp_char, supp_col, totals = c("row", "col")) {
  checkmate::assert(
    checkmate::check_data_frame(
      df,
      any.missing = FALSE,
      min.rows = 1,
      min.cols = 1
    ),
    checkmate::check_numeric(
      supp_val,
      lower = 1
    ),
    checkmate::check_character(
      supp_char
    ),
    checkmate::check_atomic_vector(
      supp_col
    ),
    checkmate::check_atomic_vector(
      totals,
      max.len = 2
    ),
    combine = "and"
  )

  if ("row" %in% totals) {
    df_row_totals <- df |>
      dplyr::mutate(
        total = rowSums(
          df |>
            dplyr::select(
              tidyselect::all_of(supp_col)
            ),
          na.rm = TRUE
        )
      ) |>
      pull(total)

    if (!("col" %in% totals)) {
      df_row_totals <- base::data.frame(
        Total = df_row_totals
        )
    }
  }

  if ("col" %in% totals) {
    supp_col_idx <- supp_col_idx(
      df,
      supp_col
      )
    fill_col_totals <- base::rep(
      supp_char,
      ncol(df)
      )
    fill_col_totals[supp_col_idx] <- base::as.character(
      base::colSums(
        df |>
          dplyr::select(
            tidyselect::all_of(
              supp_col
              )
            )
        )
      )

    if ("row" %in% totals) {
      df_row_totals <- base::data.frame(
        Total =
          c(
            df_row_totals,
            base::sum(
              base::as.numeric(
                fill_col_totals[
                  fill_col_totals != supp_char
                  ]
                )
              )
          )
      )
    }
  }


  # First pass:
  # For all suppression columns (supp_col), replace any cell value equal to
  # or less than suppression value (supp_val) with suppression character (supp_ch)
  df <- df |>
    dplyr::mutate(
      dplyr::across(
        supp_col, ~ dplyr::if_else(
          .x <= supp_val & .x != 0,
          supp_char,
          base::as.character(.x)
          )
        )
      )

  regex_char <- dplyr::if_else(
    supp_char %in% c('*', '.'),
    base::paste0(
      '\\',
      supp_char
      ),
    supp_char
  )

  # Call recursive function to perform complementary suppression
  df <- complementary(
    df,
    supp_col,
    supp_col_idx,
    base::as.character(regex_char)
    )
  if ("row" %in% totals) {
    df <- base::cbind(
      df,
      df_row_totals
      )
    }
  if ("col" %in% totals) {
    df <- base::rbind(
      df,
      fill_col_totals
      )
    base::row.names(df) <- c(
      c(2:base::nrow(df) - 1),
      "Total"
      )
  }
  return(df)
}
