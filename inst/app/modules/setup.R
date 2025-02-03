setupUI <- function(id) {
  ns <- NS(id)

  box(
    title = 'Upload Data',
    status = 'success',
    solidHeader = TRUE,
    width = 12,
    fluidRow(
      column(3, h4(icon('upload'), 'Upload')),
      column(2, offset = 7, helpButtonUI(ns('upload_data')), align = 'right')
    ),
    fluidRow(
      column(
        12,
        shinyDirLink(
          ns('choose_dir'),
          label = 'Upload Country Data',
          title = 'Browse or Drop...',
          icon = icon('folder-open')
        ),

        messageBoxUI(ns('feedback'))
      )
    )
  )
}

setupServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      messageBox <- messageBoxServer('feedback')

      cache <- init_CacheConnection()$reactive()

      shinyDirChoose(input, 'choose_dir', roots = getRoots(), allowDirCreate = TRUE)

      # Set directory logic
      selected_dir <- eventReactive(input$choose_dir, {
        req(input$choose_dir)
        parseDirPath(getRoots(), input$choose_dir)
      })

      observeEvent(selected_dir(), {
        req(selected_dir())

        path <- selected_dir()
        if (is.null(path) || length(path) == 0 || nchar(path) == 0) {
          showNotification("Invalid directory selected. Try again.", type = "error")
          return()
        }

        countries <- c('Ethiopia', 'Uganda', 'Tanzania', 'Rwanda', 'Kenya', 'Senegal', 'Burkina Faso',
                       'Cameroon', 'Cote_dIvoire', 'Chad', 'Ghana', 'CAR', 'DRC', 'Guinea',
                       'Liberia', 'Madagascar', 'Malawi', 'Mali', 'Mauritania', 'Mozambique',
                       'Niger', 'Nigeria', 'Sierra Leone', 'Somalia', 'Zambia', 'Zimbabwe')

        quality_data <<- NULL
        national_data <<- NULL
        admin1_data <<- NULL

        messageBox$update_message('Loading files', 'success')

        walk(countries, ~ {

          quality <- file.path(path, .x, 'code 1a output', 'Checks.xlsx')
          national <- file.path(path, .x, 'code 2 output', 'National_Coverage_Estimates.xlsx')
          admin1 <- file.path(path, .x, 'code 2 output', 'Admin1_Coverage_Estimates.xlsx')

          if (file.exists(quality)) {
            dqa_data <- read_excel(quality, sheet = "Overall score", skip = 1) %>%
              mutate(country = .x)
            quality_data <<- if (is.null(quality_data)) {
              dqa_data
            } else {
              bind_rows(quality_data, dqa_data)
            }
          } else {
            messageBox$add_message(paste("Quality data File not found for country:", .x), 'error')
          }

          if (file.exists(national)) {
            nat <- read_excel(national)%>%
              mutate(country = .x)
            national_data <<- if (is.null(national_data)) {
              nat
            } else {
              bind_rows(national_data, nat)
            }
          } else {
            messageBox$add_message(paste("National File not found for country:", .x), 'error')
          }

          if (file.exists(admin1)) {
            adm1 <- read_excel(admin1) %>%
              mutate(country = .x)
            admin1_data <<- if (is.null(admin1_data)) {
              adm1
            } else {
              bind_rows(admin1_data, adm1)
            }
          } else {
            messageBox$add_message(paste("Admin 1 File not found for country:", .x), 'error')
          }
        })

        cache()$set_national_estimates(national_data)
        cache()$set_regional_estimates(admin1_data)
        cache()$set_quality_data(quality_data)

        messageBox$add_message('Loading files completed', 'success')
      })

      downloadButtonServer(
        id = 'download_data',
        filename = 'master_dataset',
        extension = 'dta',
        content = function(file) {
          haven::write_dta(cache()$countdown_data, file)
        },
        data = cache,
        label = 'Download Master Dataset'
      )

      helpButtonServer(
        id = 'upload_data',
        title = 'Upload Data',
        md_file = 'load_data_upload_files.md'
      )

      return(cache)
    }
  )
}
