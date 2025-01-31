nationalDropoutUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('national_dropout'), 'Overall National Dropout Rates'),
    contentBody(
      box(
        title = 'Overall National Dropout Rates',
        status = 'success',
        width = 12,
        height = '100%',
        fluidRow(
          column(
            3,
            selectizeInput(ns('indicator'), label = 'Select Indicator',
                           choices = c("Dropout Penta1-Penta3" = "cov_dropout_penta13_penta1",
                                       "Dropout Penta3-Measles-1" = "cov_dropout_penta3mcv1_penta1")
            )
          ),
          column(12, plotlyOutput(ns('plot'), height = '600px'))
        )
      )
    )
  )
}

nationalDropoutServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$plot <- renderPlotly({
        req(input$indicator, cache(), cache()$national_estimates)
        dropout_data <- process_dropout_data(cache()$national_estimates, indicator = input$indicator)
        ggplotly(plot(dropout_data))
      })

      contentHeaderServer(
        'national_dropout',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Overall National Dropout Rates'
      )

    }
  )
}
