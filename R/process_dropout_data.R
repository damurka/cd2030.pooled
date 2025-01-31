#' Process Dropout Data
#'
#' This function processes dropout data based on user-selected indicators and
#' optional country filtering.
#'
#' @param data A data frame containing `year`, `country`, and the selected indicator.
#' @param indicator A character string specifying the column name for the dropout indicator.
#' @param country_name (Optional) A character string to filter data by a specific country.
#' @param admin_level (Optional) A character string specifying an administrative
#'   level column (e.g., 'adminlevel_1').
#'
#' @return A processed data frame with an additional `dropout_category` column.
#' @export
#'
#' @examples
#' df <- data.frame(year = c(2020, 2021), country = c('A', 'B'), dropout_1 = c(-12, 8))
#' process_dropout_data(df, 'dropout_1')
#' process_dropout_data(df, 'dropout_1', country = 'A')

process_dropout_data <- function(.data, indicator, country_name = NULL, admin_level = NULL) {

  # Ensure the indicator column exists
  if (!indicator %in% names(.data)) {
    stop('The specified indicator column does not exist in the dataset.')
  }

  # Select required columns
  selected_cols <- c('year', 'country', indicator)
  if (!is.null(admin_level)) {
    selected_cols <- c(selected_cols, admin_level)
  }

  # Filter dataset if a country is specified
  selected_data <- .data %>%
    select(all_of(selected_cols)) %>%
    filter(if (!is.null(country_name)) country == country_name else TRUE) %>%
    mutate(
      category = case_when(
        !!sym(indicator) < -10 ~ 'Could be Serious Data Issues (< -10%)',
        !!sym(indicator) < -5 ~ 'Could be Data Issues (< -5%)',
        !!sym(indicator) >= -5 & !!sym(indicator) < 5 ~ 'Best (< 5%)',
        !!sym(indicator) >= 5 & !!sym(indicator) < 10 ~ 'At the Margin (5-10%)',
        !!sym(indicator) >= 10 ~ 'Worst (> 10%)',
        .default = 'No Data',
        .ptype = factor(levels = c('Could be Serious Data Issues (< -10%)',
                                   'Could be Data Issues (< -5%)',
                                   'Best (< 5%)',
                                   'At the Margin (5-10%)',
                                   'Worst (> 10%)',
                                   'No Data'))
      )
    )

  new_tibble(
    selected_data,
    class = 'pooled_dropout',
    indicator = indicator,
    country = country_name,
    admin_level = admin_level
  )
}
