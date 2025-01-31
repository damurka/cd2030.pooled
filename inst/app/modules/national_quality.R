nationalQualityUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('national_quality'), 'National Data Quality'),
    contentBody(
      box(
        title = 'National Data Quality',
        status = 'success',
        width = 12,
        fluidRow(
          column(
            3,
            selectizeInput(ns('indicators'), 'Select Indicators',
                           choices = c('Overall Data quality'='overall',
                                       'Data Quality for only reported vaccines'='overall_vacc_only',
                                       'Data Quality for only Tracer vaccines'='overall_vacc_tracer'))
          ),
          column(
            12,
            plotlyOutput(ns('plot'), height = '600px')
          )
        )
      )
    )
  )
}

nationalQualityServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$plot <- renderPlotly({
        req(cache(), cache()$quality_data, input$indicators)
        coverage_data <- cache()$quality_data %>%
          rename_with(tolower) %>%
          process_coverage_data(indicator = input$indicators)
        ggplotly(plot(coverage_data))
      })

      contentHeaderServer(
        'national_quality',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'National Data Quality'
      )

    }
  )
}
