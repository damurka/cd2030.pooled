regionalCoverageUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('regional_coverage'), 'Overall Regional Coverage Estimates'),
    contentBody(
      box(
        title = 'Overall Regional Coverage Estimates',
        status = 'success',
        width = 12,
        height = '100%',
        fluidRow(
          column(12, plotlyOutput(ns('plot'), height = '600px'))
        )
      )
    )
  )
}

regionalCoverageServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$plot <- renderPlotly({
        req(cache(), cache()$national_estimates)
        coverage_data <- process_regional_coverage_data(cache()$national_estimates)

        ggplotly(plot(coverage_data)) %>%
          layout(
            title = list(
              text = "Trends in the coverage of immunization indicators in CD2030 sub-Saharan Africa",
              font = list(size = 16),
              x = 0.5
            )
          )
      })

      contentHeaderServer(
        'regional_coverage',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Overall Regional Coverage Estimates'
      )

    }
  )
}
