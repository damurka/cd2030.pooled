#' Process Coverage Data
#'
#' This function processes coverage data based on user-selected indicators and
#' optional country filtering.
#'
#' @param data A data frame containing `year`, `country`, and the selected indicators.
#' @param indicator A character string specifying the column name for the indicator.
#' @param country_name (Optional) A character string to filter data by a specific country.
#' @param admin_level (Optional) A character string specifying an administrative
#'   level column (e.g., 'adminlevel_1').
#'
#' @return A processed data frame with an additional `coverage_category` column.
#' @export
#'
#' @examples
#' df <- data.frame(year = c(2020, 2021), country_name = c('A', 'B'),
#'                   indicator_1 = c(85, 92))
#' process_coverage_data(df, 'indicator_1')
#' process_coverage_data(df, 'indicator_1', country_name = 'A')

process_coverage_data <- function(.data, indicator, country_name = NULL, admin_level = NULL) {
  # Ensure the indicator column exists
  if (!indicator %in% names(.data)) {
    stop('The specified indicator column does not exist in the dataset.')
  }

  # Select required columns
  selected_cols <- c('year', 'country', indicator)
  if (!is.null(admin_level)) {
    selected_cols <- c(selected_cols, admin_level)
  }

  selected_data <- .data %>%
    select(all_of(selected_cols)) %>%
    filter(if(is.null(country_name)) TRUE else country == country_name) %>%
    mutate(
      category = case_when(
        !!sym(indicator) >= 90 ~ '90%+',
        !!sym(indicator) >= 80 ~ '80-90%',
        !!sym(indicator) >= 70 ~ '70-80%',
        !!sym(indicator) >= 60 ~ '60-70%',
        !!sym(indicator) >= 50 ~ '50-60%',
        .default = 'Below 50%',
        .ptype = factor(levels = c('90%+', '80-90%', '70-80%', '60-70%', '50-60%', 'Below 50%'))
      )
    )

  new_tibble(
    selected_data,
    class = 'pooled_coverage',
    admin_level = admin_level,
    indicator = indicator,
    country = country_name
  )
}
