#' Plot Dropout Heatmap for Pooled Dropout Data
#'
#' This function generates a heatmap visualization for dropout rates categorized
#' into predefined groups. The function is specifically designed for objects of class
#' `pooled_coverage`, which are created using `process_dropout_data()`.
#'
#' The heatmap represents dropout categories with colors, where higher dropout percentages
#' are shown in dark red and lower percentages in green or yellow.
#'
#' @param x A data frame of class `pooled_coverage` containing processed dropout data.
#' @param ... Additional arguments passed to `ggplot()`, if any.
#'
#' @return A `ggplot` object representing the dropout heatmap.
#' @export
#'
#' @examples
#' # Sample dataset
#' df <- data.frame(
#'   year = c(2020, 2021, 2021, 2022),
#'   country = c("A", "B", "A", "B"),
#'   adminlevel_1 = c("Region1", "Region2", "Region1", "Region2"),
#'   dropout_1 = c(-12, 8, -4, 11)
#' )
#'
#' # Process the dropout data
#' processed_df <- process_dropout_data(df, "dropout_1")
#'
#' # Generate the dropout heatmap
#' plot(processed_df)
#'
plot.pooled_dropout <- function(x, ...) {

  indicator <- attr(x, 'indicator')
  admin_level <- attr(x, 'admin_level')
  y_var <- if (!is.null(admin_level)) admin_level else 'country'

  ggplot(x, aes(x = year, y = !!sym(y_var), fill = category)) +
    geom_tile(color = "white") +
    scale_fill_manual(
      values = c(
        "Could be Serious Data Issues (< -10%)" = "#d73027",
        "Could be Data Issues (< -5%)" = "#fc8d59",
        "Best (< 5%)" = "#66c2a5",
        "At the Margin (5-10%)" = "#FFBF00",
        "Worst (> 10%)" = "darkred",
        "No Data" = "#f4cae4"
      )
    ) +
    labs(
      x = "Year",
      y = if (!is.null(admin_level)) "Admin Level 1" else "Country",
      fill = "Dropout Category",
      title = paste("Estimates of", indicator, if (!is.null(admin_level)) "by Admin Level" else "by Country")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid.major = element_line(color = "lightgrey"),
      panel.grid.minor = element_blank()
    ) +
    geom_text(aes(label = round(!!sym(indicator), 2)), color = "black", size = 3, vjust = 0.5)
}
