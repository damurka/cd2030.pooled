#' Plot Regional Immunization Data Quality Trends
#'
#' This function generates a line plot visualizing trends in regional
#' immunization data quality indicators over time. The function is designed for
#' data frames of class `pooled_regional_quality`, typically produced by
#' `process_regional_quality_data()`.
#'
#' The plot displays trends for three key immunization data quality metrics:
#' - **Overall Data Quality (`mean_overall_dqa`)**: General assessment of data quality.
#' - **Vaccination-Only Data Quality (`mean_overall_vacc_only`)**: Data quality for reported vaccines only.
#' - **Tracer Vaccine Data Quality (`mean_overall_vacc_tracer`)**: Data quality for tracer vaccines.
#'
#' @param x A data frame of class `pooled_regional_quality` containing
#'   summarized regional immunization data quality.
#' @param ... Additional arguments passed to `ggplot()`, if any.
#'
#' @return A `ggplot` object representing immunization data quality trends over time.
#' @export
#'
#' @examples
#' # Sample dataset
#' df <- data.frame(
#'   year = rep(2015:2022, each = 3),
#'   mean_overall_dqa = runif(24, 60, 100),
#'   mean_overall_vacc_only = runif(24, 50, 95),
#'   mean_overall_vacc_tracer = runif(24, 40, 90)
#' )
#'
#' # Assign class to match expected input type
#' class(df) <- c("pooled_regional_quality", class(df))
#'
#' # Generate the quality plot
#' plot.pooled_regional_quality(df)
#'
plot.pooled_regional_quality <- function(x, ...) {

  ggplot(x, aes(x = year)) +
    geom_line(aes(y = mean_overall_dqa, color = "Overall Data Quality"), linewidth = 1.2) +
    geom_point(aes(y = mean_overall_dqa, color = "Overall Data Quality"), size = 3, shape = 21, fill = "white") +
    geom_line(aes(y = mean_overall_vacc_only, color = "Reported Vaccines Only"), linewidth = 1.2) +
    geom_point(aes(y = mean_overall_vacc_only, color = "Reported Vaccines Only"), size = 3, shape = 21, fill = "white") +
    geom_line(aes(y = mean_overall_vacc_tracer, color = "Tracer Vaccines Only"), linewidth = 1.2) +
    geom_point(aes(y = mean_overall_vacc_tracer, color = "Tracer Vaccines Only"), size = 3, shape = 21, fill = "white") +
    labs(
      x = "Year",
      y = "Data Quality (%)",
      title = "Trends in Immunization Data Quality",
      color = "Metrics"
    ) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    scale_color_manual(values = c(
      "Overall Data Quality" = "darkblue",
      "Reported Vaccines Only" = "darkgreen",
      "Tracer Vaccines Only" = "darkred"
    )) +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      legend.position = "bottom"
    )
}
