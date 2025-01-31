regionalQualityUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('regional_quality'), 'Regional Data Quality'),
    contentBody(
      box(
        title = 'Regional Data Quality',
        status = 'success',
        width = 12,
        plotlyOutput(ns('plot'))
      )
    )
  )
}

regionalQualityServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$plot <- renderPlotly({
        req(cache(), cache()$quality_data)
        quality_data <- process_regional_quality_data(cache()$quality_data)
        ggplotly(plot(quality_data))
      })

      contentHeaderServer(
        'regional_quality',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Regional Data Quality'
      )

    }
  )
}
