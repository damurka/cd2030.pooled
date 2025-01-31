#' Plot Regional Immunization Coverage Trends
#'
#' This function generates a line plot visualizing trends in regional
#' immunization coverage indicators over time. The function is designed for
#' data frames of class `pooled_regional_coverage`, typically produced by
#' `process_regional_coverage_data()`.
#'
#' The plot displays trends for key immunization indicators, including Measles-1,
#' Measles-2, BCG, ROTA-2, Penta-3 vaccination, and dropout rates for Penta1-Penta3.
#'
#' @param x A data frame of class `pooled_regional_coverage` containing
#'   summarized regional immunization coverage data.
#' @param ... Additional arguments passed to `ggplot()`, if any.
#'
#' @return A `ggplot` object representing the immunization coverage trends.
#' @export
#'
#' @examples
#' # Sample dataset
#' df <- data.frame(
#'   year = rep(2015:2022, each = 3),
#'   mean_cov_measles1_penta1 = runif(24, 50, 90),
#'   mean_cov_dropout_penta13_penta1 = runif(24, 5, 20),
#'   mean_cov_penta3_penta1 = runif(24, 60, 90),
#'   mean_cov_measles2_penta1 = runif(24, 40, 85),
#'   mean_cov_rota2_penta1 = runif(24, 50, 90),
#'   mean_cov_bcg_penta1 = runif(24, 60, 95)
#' )
#'
#' # Assign class to match expected input type
#' class(df) <- c("pooled_regional_coverage", class(df))
#'
#' # Generate the coverage plot
#' plot.pooled_regional_coverage(df)
#'
plot.pooled_regional_coverage <- function(x, ...) {
  ggplot(x, aes(x = year)) +
    geom_line(aes(y = mean_cov_measles1_penta1, color = "Measles-1"), linewidth = 1.2) +
    geom_point(aes(y = mean_cov_measles1_penta1, color = "Measles-1"), size = 3, shape = 21, fill = "white") +
    geom_line(aes(y = mean_cov_measles2_penta1, color = "Measles-2"), linewidth = 1.2) +
    geom_point(aes(y = mean_cov_measles2_penta1, color = "Measles-2"), size = 3, shape = 21, fill = "white") +
    geom_line(aes(y = mean_cov_bcg_penta1, color = "BCG"), linewidth = 1.2) +
    geom_point(aes(y = mean_cov_bcg_penta1, color = "BCG"), size = 3, shape = 21, fill = "white") +
    geom_line(aes(y = mean_cov_rota2_penta1, color = "ROTA-2"), linewidth = 1.2) +
    geom_point(aes(y = mean_cov_rota2_penta1, color = "ROTA-2"), size = 3, shape = 21, fill = "white") +
    geom_line(aes(y = mean_cov_dropout_penta13_penta1, color = "Drop-out-Penta1-Penta3"), linewidth = 1.2) +
    geom_point(aes(y = mean_cov_dropout_penta13_penta1, color = "Drop-out-Penta1-Penta3"), size = 3, shape = 21, fill = "white") +
    geom_line(aes(y = mean_cov_penta3_penta1, color = "Penta-3 Vaccination"), linewidth = 1.2) +
    geom_point(aes(y = mean_cov_penta3_penta1, color = "Penta-3 Vaccination"), size = 3, shape = 21, fill = "white") +
    labs(
      x = "Year",
      y = "Coverage (%)",
      color = "Metrics"
    ) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    scale_color_manual(values = c(
      "Measles-1" = "blue",
      "Drop-out-Penta1-Penta3" = "darkgreen",
      "Penta-3 Vaccination" = "darkred",
      "ROTA-2" = "purple",
      "Measles-2" = "orange",
      "BCG" = "skyblue"
    )) +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, size = 16))
}
