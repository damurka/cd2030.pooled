#' Plot Coverage Heatmap for Pooled Coverage Data
#'
#' This function generates a heatmap visualization of coverage estimates
#' for different years and administrative levels (if applicable). The
#' function is specifically designed for data objects of class `pooled_coverage`,
#' which are created using `process_coverage_data()`.
#'
#' The heatmap represents the coverage category using a color gradient,
#' where higher coverage percentages are shown in teal and lower percentages
#' in shades of red.
#'
#' @param x A data frame of class `pooled_coverage` containing processed coverage data.
#' @param ... Additional arguments passed to `ggplot()`, if any.
#'
#' @return A ggplot object representing the heatmap of coverage estimates.
#' @export
#'
#' @examples
#' # Sample dataset
#' df <- data.frame(
#'   year = c(2020, 2021, 2021, 2022),
#'   country_name = c('A', 'B', 'A', 'B'),
#'   adminlevel_1 = c('Region1', 'Region2', 'Region1', 'Region2'),
#'   indicator_1 = c(85, 92, 73, 50)
#' )
#'
#' # Process the coverage data
#' processed_df <- process_coverage_data(df, 'indicator_1')
#'
#' # Generate the heatmap
#' plot(processed_df)
#'
plot.pooled_coverage <- function(x, ...) {

  country <- attr(x, 'country')
  indicator <- attr(x, 'indicator')
  admin_level <- attr(x, 'admin_level')
  y_var <- if (!is.null(admin_level)) admin_level else 'country'

  ggplot(x, aes(x = year, y = !!sym(y_var), fill = category)) +
    geom_tile(color = 'white') +
    scale_fill_manual(
      values = c(
        '90%+' = '#66c2a5',         # Soft Teal
        '80-90%' = '#b3e2cd',       # Pale Aqua
        '70-80%' = '#fdcc8a',       # Light Peach
        '60-70%' = '#f4a582',       # Soft Coral
        '50-60%' = '#fc8d59',       # Light Red
        'Below 50%' = '#d73027'     # Dark Red
      )
    ) +
    labs(
      x = 'Year',
      y = if (!is.null(admin_level)) 'Admin Level 1' else 'Country',
      fill = 'Coverage Category',
      title = paste('Estimates of', indicator, if (!is.null(country)) paste('in', country) else 'by CD2030 country')
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),
      panel.grid.major = element_line(color = 'lightgrey'),
      panel.grid.minor = element_blank()
    ) +
    geom_text(aes(label = round(!!sym(indicator), 2)), color = 'black', size = 3, vjust = 0.5)

}
