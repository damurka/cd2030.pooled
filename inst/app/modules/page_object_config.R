pageObjectsConfig <- function(input) {
  list(
    national_quality = list(
      'Overall Data Quality' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Quality for Reported Vaccines' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Quality for Tracer Vaccines' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      )
    ),
    regional_quality = list(
      'Overall' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      )
    ),
    national_coverage = list(
      'Measles 1' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta 3' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'BCG' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Rota 2' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Measles 2' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      )
    ),
    regional_coverage = list(
      'Overall' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      )
    ),
    national_dropout = list(
      'Penta1 to Penta3 Dropout' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta3 to Measles 1 Dropout' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      )
    ),
    subnational_coverage = list(
      'Measles 1' = list(
        parameters = list(
          country = reactive({ input$country })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Penta 3' = list(
        parameters = list(
          country = reactive({ input$country })
        ),
        always_include = TRUE,
        single_entry = FALSE
      )
    ),
    subnational_dropout = list(
      'Penta 1 to Penta 3 Dropout' = list(
        parameters = list(
          country = reactive({ input$country })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Penta 3 to Measles 1 Dropout' = list(
        parameters = list(
          country = reactive({ input$country })
        ),
        always_include = TRUE,
        single_entry = FALSE
      )
    )
  )
}
