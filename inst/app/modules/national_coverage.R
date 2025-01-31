nationalCoverageUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('national_coverage'), 'Overall National Coverage Estimates'),
    contentBody(
      box(
        title = 'Overall National Coverage Estimates',
        status = 'success',
        width = 12,
        height = '100%',
        fluidRow(
          column(
            3,
            selectizeInput(ns('indicator'), label = 'Select Indicator',
                           choices = c("Measles-1" = "cov_measles1_penta1",
                                       "Penta-3 Vaccination" = "cov_penta3_penta1",
                                       "BCG"="cov_bcg_penta1",
                                       "ROTA-2"="cov_rota2_penta1",
                                       "Measles-2"="cov_measles2_penta1")
            )
          ),
          column(12, plotlyOutput(ns('plot'), height = '600px'))
        )
      )
    )
  )
}

nationalCoverageServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$plot <- renderPlotly({
        req(input$indicator, cache(), cache()$national_estimates)
        coverage_data <- process_coverage_data(.data = cache()$national_estimates, indicator = input$indicator)
        ggplotly(plot(coverage_data))
      })

      contentHeaderServer(
        'national_coverage',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Overall National Coverage Estimates'
      )

    }
  )
}
