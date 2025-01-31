#' Process Regional Immunization Coverage Data
#'
#' This function calculates yearly mean immunization coverage indicators
#' at the regional level. It summarizes coverage data by year and returns
#' a tibble of class `pooled_regional_coverage`.
#'
#' The function computes averages for several immunization indicators,
#' including Measles-1, Penta-1 to Penta-3 dropout rates, ROTA-2, and BCG coverage.
#'
#' @param .data A data frame containing yearly immunization coverage indicators.
#'
#' @return A tibble with class `pooled_regional_coverage`, containing
#'   summarized mean coverage percentages for different vaccines by year.
#' @export
#'
#' @examples
#' # Sample dataset
#' df <- data.frame(
#'   year = rep(2015:2022, each = 3),
#'   cov_measles1_penta1 = runif(24, 50, 90),
#'   cov_dropout_penta13_penta1 = runif(24, 5, 20),
#'   cov_penta3_penta1 = runif(24, 60, 90),
#'   cov_measles2_penta1 = runif(24, 40, 85),
#'   cov_rota2_penta1 = runif(24, 50, 90),
#'   cov_bcg_penta1 = runif(24, 60, 95)
#' )
#'
#' # Process the data
#' processed_df <- process_regional_coverage_data(df)
#'
#' # Print the processed data
#' print(processed_df)

process_regional_coverage_data <- function(.data) {
  selected_data <- .data %>%
    summarise(
      mean_cov_measles1_penta1 =mean(cov_measles1_penta1, na.rm = TRUE),
      mean_cov_dropout_penta13_penta1 = mean(cov_dropout_penta13_penta1, na.rm = TRUE),
      mean_cov_penta3_penta1 = mean(cov_penta3_penta1, na.rm = TRUE),
      mean_cov_measles2_penta1 = mean(cov_measles2_penta1, na.rm = TRUE),
      mean_cov_rota2_penta1 = mean(cov_rota2_penta1, na.rm = TRUE),
      mean_cov_bcg_penta1 = mean(cov_bcg_penta1, na.rm = TRUE),
      .by = year
    )

  new_tibble(
    selected_data,
    class = 'pooled_regional_coverage'
  )
}
