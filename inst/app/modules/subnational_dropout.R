subnationalDropoutUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('subnational_dropout'), 'Subnational Dropout Rates'),
    contentBody(
      box(
        title = 'Subnational Dropout Rates',
        status = 'success',
        width = 12,
        height = '100%',
        fluidRow(
          column(
            3,
            selectizeInput(ns('country'), label = 'Select Country',
                           choices = c("Ethiopia", "Uganda", "Tanzania", "Rwanda", "Kenya", "Senegal", "Burkina Faso",
                                       "Cameroon", "Cote_dIvoire", "Chad", "Ghana", "CAR", "DRC", "Guinea",
                                       "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mozambique",
                                       "Niger", "Nigeria", "Sierra Leone", "Somalia", "Zambia", "Zimbabwe")
            )
          ),
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

subnationalDropoutServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$plot <- renderPlotly({
        req(input$indicator, input$country, cache(), cache()$regional_estimates)
        dropout_data <- process_dropout_data(cache()$regional_estimates,
                                             indicator = input$indicator,
                                             country_name = input$country,
                                             admin_level = 'adminlevel_1')
        ggplotly(plot(dropout_data))
      })

      contentHeaderServer(
        'subnational_dropout',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Subnational Dropout Rates'
      )

    }
  )
}
