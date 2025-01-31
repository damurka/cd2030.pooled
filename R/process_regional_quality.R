#' Process Regional Immunization Data Quality
#'
#' This function calculates yearly mean immunization data quality indicators
#' at the regional level. It summarizes quality data by year and returns
#' a tibble of class `pooled_regional_quality`.
#'
#' The function computes averages for three key immunization data quality metrics:
#' - **Overall Data Quality (`overall`)**: General assessment of data quality.
#' - **Vaccination-Only Data Quality (`overall_vacc_only`)**: Data quality for reported vaccines only.
#' - **Tracer Vaccine Data Quality (`overall_vacc_tracer`)**: Data quality for tracer vaccines.
#'
#' @param .data A data frame containing yearly immunization quality indicators.
#'
#' @return A tibble with class `pooled_regional_quality`, containing
#'   summarized mean data quality percentages by year.
#' @export
#'
#' @examples
#' # Sample dataset
#' df <- data.frame(
#'   year = rep(2015:2022, each = 3),
#'   overall = runif(24, 60, 100),
#'   overall_vacc_only = runif(24, 50, 95),
#'   overall_vacc_tracer = runif(24, 40, 90)
#' )
#'
#' # Process the data
#' processed_df <- process_regional_quality_data(df)
#'
#' # Print the processed data
#' print(processed_df)
#'
process_regional_quality_data <- function(.data) {
  selected_data <- .data %>%
    summarise(
      mean_overall_dqa = mean(overall, na.rm = TRUE),
      mean_overall_vacc_only= mean(overall_vacc_only, na.rm = TRUE),
      mean_overall_vacc_tracer = mean(overall_vacc_tracer, na.rm = TRUE),
      .by = year
    )

  new_tibble(
    selected_data,
    class = 'pooled_regional_quality'
  )
}
