subnationalAnalysisUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('subnational_coverage'), 'Subnational Coverage Analysis'),
    contentBody(
      box(
        title = 'Subnational Coverage Analysis',
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
                                       "Niger", "Nigeria", "Sierra Leone", "Somalia", "Zambia", "Zimbabwe"),
            )
          ),
          column(
            3,
            selectizeInput(ns('indicator'), label = 'Select Indicator',
                           choices = c("Measles-1" = "cov_measles1_penta1",
                                       "Penta-3 Vaccination" = "cov_penta3_penta1")
            )
          ),
          column(12, plotlyOutput(ns('plot'), height = '600px'))
        )
      )
    )
  )
}

subnationalAnalysisServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$plot <- renderPlotly({
        req(input$indicator, input$country, cache(), cache()$regional_estimates)
        coverage_data <- process_coverage_data(.data = cache()$regional_estimates,
                                               indicator = input$indicator,
                                               country_name = input$country,
                                               admin_level = 'adminlevel_1')
        ggplotly(plot(coverage_data))
      })

      contentHeaderServer(
        'subnational_coverage',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Subational Coverage Analysis'
      )

    }
  )
}
